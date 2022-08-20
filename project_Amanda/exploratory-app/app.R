library(dplyr)
library(GGally)
library(shiny)
library(ggforce)
library(broom)


# setwd(paste(getwd(), sep = "", "/project_Amanda/exploratory-app"))
# load("project_Amanda/final_data.RData") # swpd_new_rp1
load("final_data_logis.RData") # swpd_new_rp2



# mydata <- swpd_new_rp1
mydata <- swpd_new_rp2
mydata <- mydata[, c("degree.level", "ins.type", "lec.type", "train.ava", "train.req", "tech.std", "num.stu", "Q14", "Q16", "disb.yn")]
names(mydata)[8:10] <- c("num.total", "num.succ", "swpd.yn")
mydata <- mydata[-which(mydata$ins.type == "Private for Profit"), ] # remove the level with only one obs
mydata[2, "num.total"] <- 2 # column 5 in the original SWPD dataset
mydata$num.total[is.na(mydata$num.total)] <- 0

dim(mydata)


mydata <- mutate(
    mydata,
    num.stu = as.numeric(num.stu),
    num.succ = ifelse(num.succ > num.total, num.total, num.succ), # change to total if more
    grad.rate = num.succ / num.total,
    swpd.rate = num.total / (10 * num.stu),
    across(where(is.character), as.factor)
    )

names(mydata) <- c("carnegie", "ins.type", "CAPTE", "train.ava", "train.req", "techstd.update", "num.admit", "num.swpd", "num.swpd.grad", "swpd.yn", "grad.rate.swpd", "swpd.rate")
levels(mydata$carnegie) <- c("Baccalaureate", "Doctoral", "Masters", "Special Focus")
levels(mydata$CAPTE) <- c("Hybrid", "In-person")
mydata <- mydata[, c("num.admit", "num.swpd", "num.swpd.grad", "grad.rate.swpd", "swpd.rate", "carnegie", "ins.type", "CAPTE", "train.ava", "train.req", "techstd.update", "swpd.yn")]


# mydata <- mydata[-which(mydata$carnegie == "Baccalaureate"), ]


# myfit <- glm(swpd.yn ~ carnegie + ins.type + CAPTE + train.ava + train.req + techstd.update, data = mydata, family = "binomial")
# confint(myfit)
# summary(myfit)

# myfit.selected <- step(myfit)
# summary(myfit.selected)
# confint(myfit.selected)


# tempfit <- glm(swpd.yn ~ carnegie, data = mydata, family = "binomial")
# summary(tempfit)

# table(mydata$swpd.yn, mydata$carnegie)


pairwise_cor_test <- function(dat) {
    dat_names <- names(dat)
    p_mat <- matrix(NA, length(dat_names), length(dat_names))
    rownames(p_mat) <- dat_names
    colnames(p_mat) <- dat_names
    for (row_name in dat_names) 
        for (col_name in dat_names) {
            x <- dat[, row_name]
            y <- dat[, col_name]
            tmp_dt <- data.frame(x = x, y = y)
            tmp_dt <- na.omit(tmp_dt)
            
            if (row_name == col_name) next
            if (row_name == "swpd.yn" && col_name %in% c("num.swpd", "num.swpd.grad", "grad.rate.swpd", "swpd.rate")) next
            if (col_name == "swpd.yn" && row_name %in% c("num.swpd", "num.swpd.grad", "grad.rate.swpd", "swpd.rate")) next
            if (is.numeric(x) && is.numeric(y))
                p.value <- cor.test(tmp_dt$x, tmp_dt$y)$p.value
            if (is.factor(x) && is.numeric(y))
                p.value <- summary(aov(y ~ x, data = tmp_dt))[[1]][["Pr(>F)"]][1]
            if (is.numeric(x) && is.factor(y))
                p.value <- summary(aov(x ~ y, data = tmp_dt))[[1]][["Pr(>F)"]][1]
            if (is.factor(x) && is.factor(y))
                p.value <- chisq.test(x, y, simulate.p.value = TRUE, B = 10000)$p.value
            p_mat[row_name, col_name] <- p.value
        }
    p_mat
}

# temp = pairwise_cor_test(mydata)


ui <- fluidPage(
  headerPanel('Data Exploration for SWPD Data'),
  sidebarPanel(
    checkboxGroupInput('var', 'Choose at most two variables to be explored:', 
    c(
        "Annual cohort size (Q11, num.admit)" = "num.admit",
        "Number of SWPD (Q14, num.swpd)" = "num.swpd",
        "Number of graduated SWPD (Q16, num.swpd.grad)" = "num.swpd.grad",
        "SWPD graduation rate (R1, grad.rate.swpd)" = "grad.rate.swpd",
        "Percentage of SWPD among cohorts (R2, swpd.rate)" = "swpd.rate",
        "Carnegie classification (Q2, carnegie)" = "carnegie",
        "Institutional classification (Q3, ins.type)" = "ins.type",
        "Primary delivery method (Q4, CAPTE)" = "CAPTE",
        "Is training on accommodation implementation available (Q5, train.ava)" = "train.ava",
        "Is training on accommodation implementation required (Q6, train.req)" = "train.req",
        "Whether matriculation standards have be updated or not (Q8, techstd.update)" = "techstd.update",
        "Whether SWPD have been admitted or not (Q13, swpd.yn)" = "swpd.yn"
    ),
    c("num.admit", "num.swpd")
    ),
    submitButton("Submit"),
    helpText("Note: when columns involved SWPD information (Q16, R1, R2) are selected, the comparison is done only on institutions who answered \"Yes\" on Q13 (whether SWPD have been admitted or not).")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Pairwise Plot",
        plotOutput('pairplot'),
        plotOutput("pairplot2")
      ),
      tabPanel(
        "Pairwise Correlation Test",
        tableOutput("pairtable")
      )
    )   
  )
  # mainPanel(
  #   plotOutput('pairplot'),
  #   tableOutput("pairtable")
  # )
)


server <- function(input, output) {  
  output$pairplot <- renderPlot({
      validate(
        need(length(input$var) %in% c(1, 2), "For display clarity, choose 1 or 2 variables for visualization.")
      )
    
      check <- !(("swpd.yn" %in% input$var) && any(c("num.swpd.grad", "grad.rate.swpd", "swpd.rate") %in% input$var))
      validate(
        need(check, "No SWPD information available for institutions that haven't admitted any SWPD yet.\nMake sure that Q13 is not selected at the same time with either of Q16, R1 and R2.")
      )
      
      temp.df <- na.omit(data.frame(mydata[, input$var]))
      # temp.df <- data.frame(mydata[, input$var])
      names(temp.df) <- input$var
      if (length(input$var) == 1)
        if (is.numeric(mydata[, 1]))
          ggally_barDiag(temp.df, aes_string(input$var)) + ggtitle("Histogram")
        else
          ggally_barDiag(temp.df, aes_string(input$var)) + ggtitle("Bar plot")
          
      else {
        # length(input$var) == 2
        if (is.numeric(temp.df[, 1]) && is.numeric(temp.df[, 2]))
          ggally_smooth_loess(temp.df, aes_string(x = input$var[1], y = input$var[2])) + ggtitle("Scatterplot 1 with LOESS fitting")
        else if (is.numeric(temp.df[, 1]) && is.factor(temp.df[, 2]))
          ggally_box_no_facet(temp.df, aes_string(x = input$var[2], y = input$var[1])) + ggtitle("Side-by-side boxplot")
        else if (is.numeric(temp.df[, 2]) && is.factor(temp.df[, 1]))
          ggally_box_no_facet(temp.df, aes_string(x = input$var[1], y = input$var[2])) + ggtitle("Side-by-side boxplot")
        else
          ggally_crosstable(temp.df, aes_string(x = input$var[1], y = input$var[2])) + ggtitle("Contingency table")
      }
  })

  output$pairplot2 <- renderPlot({
      validate(
        need(length(input$var) %in% c(1, 2), "For display clarity, choose 1 or 2 variables for visualization.")
      )
    
      check <- !(("swpd.yn" %in% input$var) && any(c("num.swpd.grad", "grad.rate.swpd", "swpd.rate") %in% input$var))
      validate(
        need(check, "No SWPD information available for institutions that haven't admitted any SWPD yet.\nMake sure that Q13 is not selected at the same time with either of Q16, R1 and R2.")
      )
      
      temp.df <- na.omit(data.frame(mydata[, input$var]))
      # temp.df <- data.frame(mydata[, input$var])
      names(temp.df) <- input$var
      if (length(input$var) == 1){
        if (is.numeric(temp.df[, 1]))
          ggally_densityDiag(temp.df, aes_string(input$var)) + ggtitle("Density plot")
        else
          ggally_blank()
      }
      
      else {
        # length(input$var) == 2
        if (is.numeric(temp.df[, 1]) && is.numeric(temp.df[, 2]))
          ggally_smooth_loess(temp.df, aes_string(x = input$var[2], y = input$var[1])) + ggtitle("Scatterplot 2 with LOESS fitting")
        else if (is.numeric(temp.df[, 1]) && is.factor(temp.df[, 2]))
          ggally_facethist(temp.df, aes_string(x = input$var[1], y = input$var[2])) + ggtitle("Side-by-Side histogram")
        else if (is.numeric(temp.df[, 2]) && is.factor(temp.df[, 1]))
          ggally_facethist(temp.df, aes_string(x = input$var[2], y = input$var[1])) + ggtitle("Side-by-Side histogram")
        else
          ggally_colbar(temp.df, aes_string(x = input$var[1], y = input$var[2])) + ggtitle("Stacked bar plot")
      }
  })


  
  output$pairtable <- renderTable({
      validate(
        need(length(input$var) >= 2, "Make sure you choose at least two variables for pairwise correlation test")
      )
      
      check <- !(("swpd.yn" %in% input$var) && any(c("num.swpd.grad", "grad.rate.swpd", "swpd.rate") %in% input$var))
      validate(
        need(check, "No SWPD information available for institutions that haven't admitted any SWPD yet. \nMake sure that Q13 is not selected at the same time with either of Q16, R1 and R2.")
      )
      
      temp.df <- data.frame(mydata[, input$var])
      names(temp.df) <- input$var
      pairwise_cor_test(temp.df)
  },
  rownames = TRUE,
  caption = "p values from pairwise correlation tests (unadjusted for multiple comparison)")

}

shinyApp(ui = ui, server = server)




# mydata_plot <- mydata[, c("ins.type", "CAPTE", "carnegie")]
# myplot <- ggpairs(
#   mydata_plot,
#   lower = list(continuous = "cor", combo = "box_no_facet", discrete = "crosstable", na = "na")
#   )
# personal_plot <- ggally_text(
#   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
# )
# myplot[2, 1] <- personal_plot
# p_(myplot)
  

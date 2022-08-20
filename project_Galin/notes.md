# Notes from the lecture
Primary questions: How does the treatment (along with other confounding variables) affect the survival of the dogs that were bitten by snakes?

Primary response: *Outcome* (>> *Cost* and others)

Other resposne: just do some summary work

Primary Variable of interest: *number of vials*, and *dose* I think

Multivariate analysis: the response is a 4-dimensional vector (*Outcome*, *React*, *Hospdays*, *Cost*) and is is mixed-type. They are very few on this kind of model in literature, but still a open reasearch question (*we are doing consulting instead of reasearch*). Probably not a good idea to consider multiple regression.

Miscellaneous:
* The hospital probably gave the dog a vial even if it didn't need one; The hospital stopped to give the dog another vial when the doctors were sure the dog was gonna live or die;
* The data came from electronic records, so it's a retrospective study. They are probably errors, a lot of missing values, and weird / impossible entries; Here is a glimpse at the dataset:
  * Only 9 dogs died (3 out of 52 from UF and 6 out of ... from FVS);
  * A lot of missing values, not a single complete case, and some variables have almost nothing but missing values;
  * No cost data from UF hospital;
  * Some variables have entry like *>1*.
* We should only extract the **variables only relevant to the questions** (they are grouped in 7 groups, one can check that within each group); Maybe some model selection should be done before the analysis;
* Plot to explore the association (some variables are closely related, for example, continuous one versus its categorized version);
* **How to deal with missing values**:
  * Depends on how the data is missing;
  * Always consult the clients if you have some specific solutions;
  * The missing values of one variable might depend on the values of another variable (nested variables). For example, some variables are missing because there is no *React* (so there was no followup).
  * Some ideas from students: drop some; impute some;
* Sample size consideration via simulations (power anlysis: size, power, a detectable difference, then simulate to estimate the power?);


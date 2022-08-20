# keep updated with new versions of R
.libPaths(new = "~/R/ubuntu_18.04/4.0.2")


Sys.setenv(TERM_PROGRAM = "vscode")
source(file.path(Sys.getenv(
  if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"),
  ".vscode-R", "init.R")
)

cat("\nThis is the last line of .Rprofile.\n")

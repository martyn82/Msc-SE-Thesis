# This script performs wavelet transformation on evolution data.
# It takes the monthly facts for analysis data and outputs a csv file per project per variable.

library("hash")
library("wavelets")

dataset <- read.csv2("data/factsForAnalysis.csv")
output <- "output/wavelet"

if(!file.exists(output)){
  dir.create(output)
}

idcol <- "Project.Id"
namecol <- "Project.Name"
timecols <- c("Age.Days", "Active.Developers")

varcols <- hash(keys=timecols)
varcols[["Age.Days"]] <- c("LOC", "Commit.LOC.Churn", "Active.Developers")
varcols[["Active.Developers"]] <- c("Commit.LOC.Churn")

pids <- unique(dataset[[idcol]])

dst.dwt.cols <- c("seq", "variable", "pid", "coefficient", "level", "value", "revlevel")

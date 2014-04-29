# This script plots the Kaplan-Meier survival curve for projects identified as potential dying against
# a control group of equal size containing projects that are not dead and not potential dying.
# By default, dying projects containing the longest sequence are selected.

library(plyr)
library(splines)
library(survival)

source("tools/analysis/ggkm.R")

metric <- "LOC"

facts.data <- read.csv2("data/factsForAnalysis.csv")
dead.data <- read.csv2("output/deadProjectsValidated.csv")

dying.file.name <- paste(
  paste("dyingProjectsValidated", metric, sep="_"), "csv", sep="."
)
dying.data <- read.csv2(paste("output", dying.file.name, sep="/"))
max.deadcount <- max(dying.data$dead.count)

dead.data <- subset(dead.data, dead.data$confirmed.dead == "TRUE")
dying.data <- subset(dying.data, dying.data$dead.count == max.deadcount)
control.data <- subset(
  facts.data,
  !(facts.data$Project.Id %in% dying.data$pid)
)

cols <- c("pid", "time", "group", "status")
projects <- as.data.frame(
  matrix(
    ncol=length(cols),
    nrow=0
  )
)

dead.pids <- unique(dead.data$pid)
dying.pids <- unique(dying.data$pid)
control.pids <- unique(control.data$Project.Id)[1:length(dying.pids)]

control.data <- subset(control.data, control.data$Project.Id %in% control.pids)
all.pids <- append(dying.pids, control.pids)

for(pid in all.pids){
  project <- subset(facts.data, facts.data$Project.Id == pid)
  is.dead <- (pid %in% dead.pids)
  has.sequence <- (pid %in% dying.pids)

  row <- c(
    "pid"=pid,
    "time"=max(project$Age.Months),
    "group"=as.numeric(has.sequence),
    "status"=as.numeric(!is.dead)
  )

  projects <- rbind(projects, row)
}

colnames(projects) <- cols

surv.fit <- survfit(Surv(time, status) ~ group, data=projects)
ggkm(surv.fit, timeby=12)

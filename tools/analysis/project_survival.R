# This script plots the Kaplan-Meier survival curve for projects identified as potential dying against
# a control group of equal size containing projects that are not dead and not potential dying.
# By default, dying projects containing the longest sequence are selected.

library(plyr)
library(splines)
library(survival)
library(zoo)

source("tools/analysis/ggkm.R")

metric <- "LOC"

facts.data <- read.csv2("data/factsForAnalysis.csv")
dead.data <- read.csv2("output/deadProjectsValidated.csv")

dying.file.name <- paste(
  paste("dyingProjectsValidated", metric, sep="_"), "csv", sep="."
)
dying.data <- read.csv2(paste("output", dying.file.name, sep="/"))
dying.data <- subset(dying.data, dying.data$dead.count == max(dying.data$dead.count))

dead.data <- subset(dead.data, as.logical(dead.data$confirmed.dead) == TRUE)
dead.data$X <- NULL
dying.data.dies <- subset(
  dying.data,
  as.numeric(dying.data$year.contributors.change) == -1
  & as.numeric(dying.data$year.commits.change) == -1
)
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
dying.dies.pids <- unique(dying.data.dies$pid)
control.pids <- unique(control.data$Project.Id)[1:length(dying.pids)]

control.data <- subset(control.data, control.data$Project.Id %in% control.pids)
all.pids <- as.numeric(append(dying.pids, control.pids))

for(pid in all.pids){
  project <- subset(facts.data, facts.data$Project.Id == pid)
  is.dead <- (pid %in% dead.pids)
  dies <- (pid %in% dying.dies.pids)
  has.sequence <- (pid %in% dying.pids)

  if(is.dead){
    dead.project <- dead.data[dead.data$pid == pid, ]
    max.date <- max(as.Date(project$Date))
    dead.date <- as.Date(dead.project$dead.date)
    max.months <- max(project$Age.Months)
    diff.months <- 12 * as.numeric(as.yearmon(max.date) - as.yearmon(dead.date))
    time <- max.months - diff.months
    print(paste("Project", pid, "aged", max.months, "months, died at", time, "months"))
  } else {
    time <- max(project$Age.Months)
  }

  row <- c(
    "pid"=pid,
    "time"=time,
    "group"=as.numeric(has.sequence),
    "status"=as.numeric(is.dead)
  )

  projects <- rbind(projects, row)
}

rm(row)

colnames(projects) <- cols

surv.fit <- survfit(Surv(time, status) ~ group, data=projects, type="kaplan-meier")
ggkm(
  surv.fit,
  timeby=12,
  main="OSS project survival",
  xlabs="Age in months"
)

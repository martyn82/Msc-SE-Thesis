# This script plots the Kaplan-Meier survival curve for projects identified as potential dying against
# a control group of equal size containing projects that are not dead and not potential dying.
# By default, dying projects containing the longest sequence are selected.

library(plyr)
library(splines)
library(survival)
library(zoo)

source("tools/analysis/ggkm.R")

# Specify the metric to be analysed
metric <- "LOC"

# Read all facts
facts.data <- read.csv2("data/factsForAnalysis.csv")
# Read all validated dead projects
dead.data <- read.csv2("output/deadProjectsValidated.csv")
# Keep only the dead projects that are confirmed to be dead
dead.data <- subset(dead.data, as.logical(dead.data$confirmed.dead) == TRUE)
dead.data$X <- NULL

# Read the validated dying projects (i.e., the projects with a potential 'warning' pattern)
dying.file <- paste(
  "output",
  paste(
    paste("dyingProjectsValidated", metric, sep="_"),
    "csv",
    sep="."
  ),
  sep="/"
)
dying.data <- read.csv2(dying.file)
# Order dying by number of dead projects having same pattern (descending)
dying.data <- dying.data[order(-dying.data$dead.count), ]

# Determine the maximum group size to be analysed
group.size <- min(
  c(
    floor(length(unique(facts.data$Project.Id)) / 2),
    length(unique(dying.data$pid))
  )
)

# Select the sample of projects with the pattern
dying.data <- dying.data[1:group.size, ]

# Select the projects without the pattern
control.data <- subset(
  facts.data,
  !(facts.data$Project.Id %in% dying.data$pid)
)

# Determine which dead projects had a 'warning' pattern
deads.lists <- dying.data$deads
deads.with.sequence <- list()

for(i in 1:length(deads.lists)){
  deads.list <- as.character(deads.lists[i])
  deads.list.pids <- as.numeric(unlist(strsplit(deads.list, "|", fixed=TRUE)))
  deads.with.sequence <- append(deads.with.sequence, deads.list.pids)
}
deads.with.sequence <- unique(deads.with.sequence)

dead.pids <- unique(dead.data$pid)
dying.pids <- unique(dying.data$pid)

# Select sample of projects without pattern
control.pids <- unique(control.data$Project.Id)[1:group.size]
control.data <- subset(control.data, control.data$Project.Id %in% control.pids)

all.pids <- unique(as.numeric(append(dying.pids, control.pids)))

cols <- c("pid", "time", "group", "status")
projects <- as.data.frame(
  matrix(
    ncol=length(cols),
    nrow=0
  )
)

for(pid in all.pids){
  project <- subset(facts.data, facts.data$Project.Id == pid)
  is.dead <- (pid %in% dead.pids)
  has.sequence <- (pid %in% dying.pids) | (is.dead & pid %in% deads.with.sequence)

  if(is.dead){
    dead.project <- dead.data[dead.data$pid == pid, ]
    max.date <- max(as.Date(project$Date))
    dead.date <- as.Date(dead.project$dead.date)
    max.months <- max(project$Age.Months)
    diff.months <- floor(12 * as.numeric(as.yearmon(max.date) - as.yearmon(dead.date)))
    time <- max.months - diff.months

    print(paste("Project", pid, "aged", max.months, "months, died at", time, "months"))

    rm(dead.project, max.date, dead.date, max.months, diff.months)
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

rm(row, time, has.sequence, is.dead, i, group.size)

colnames(projects) <- cols

surv.fit <- survfit(Surv(time, status) ~ group, data=projects, type="kaplan-meier")
ggkm(
  surv.fit,
  timeby=12,
  main=paste("OSS project survival by", metric),
  xlabs="Age in months"
)

# Plot projects' survivability
# Some members of each group dies.
#
# Set of projects:
# pid    := project ID
# time   := age (age at death or current age)
# group  := 1 (with pattern), or 0 (w/o pattern)
# status := 1 (dead), or 0 (alive)

library(plyr)
library(splines)
library(survival)
library(zoo)

source("tools/analysis/ggkm.R")

opts.time <- "Age.Months"
opts.var <- "LOC"
opts.time.interval <- 12
opts.pattern.type <- "A"

patterns.dead <- read.csv2(paste("output", paste(paste("patterns", opts.time, opts.var, sep="_"), "csv", sep="."), sep="/"))
sequences.data <- read.csv2(paste("output", paste(paste("haar", "similar", opts.time, opts.var, sep="_"), "csv", sep="."), sep="/"))
dead.data <- read.csv2("output/deadProjectsValidated.csv")
facts.data <- read.csv2("data/factsForAnalysis.csv")

# Store PIDs of dead projects
dead.pids <- unique(dead.data[as.logical(dead.data$confirmed.dead) == TRUE, ]$pid)

# Set which pattern type should be analysed
patterns <- patterns.dead[patterns.dead$seq.type == opts.pattern.type, ]

# Store PIDs of dead projects in group 1
patterns.dead.pids <- unique(as.numeric(unlist(strsplit(as.character(patterns$pid), "|", fixed=TRUE))))

# Retrieve the PIDs having sequences for the patterns under analysis
sequences <- sequences.data[sequences.data$Seq.Id %in% patterns$seq.id, ]
sequences.pids <- unique(as.numeric(unlist(strsplit(as.character(sequences$Project.list), "|", fixed=TRUE))))
rm(sequences)

# Retrieve the PIDs having no sequences for the patterns under analysis
control.pids <- unique(facts.data[!(facts.data$Project.Id %in% sequences.pids), ]$Project.Id)

# Select group
group.size <- min(
  c(
    floor(length(control.pids) / 2),
    length(sequences.pids)
  )
)
sequences.pids.sample <- sequences.pids[1:group.size]
control.pids.sample <- control.pids[1:group.size]

# Store PIDs of both groups
all.pids <- unique(as.numeric(append(sequences.pids.sample, control.pids.sample)))

# Prepare final data frame
projects <- data.frame(
  "pid"=numeric(0),
  "time"=numeric(0),
  "group"=numeric(0),
  "status"=numeric(0)
)
p <- 1

for(i in 1:length(all.pids)){
  pid <- all.pids[i]
  project <- facts.data[facts.data$Project.Id == pid, ]
  project.status <- (pid %in% dead.pids)
  project.group <- (pid %in% sequences.pids)

  if(project.status){
    project.death <- dead.data[dead.data$pid == pid, ]
    max.date <- max(as.Date(project$Date))
    dead.date <- as.Date(project.death$dead.date)
    max.months <- max(project$Age.Months)
    diff.months <- floor(12 * as.numeric(as.yearmon(max.date) - as.yearmon(dead.date)))
    project.age <- max.months - diff.months

    rm(project.death, max.date, dead.date, max.months, diff.months)
  }
  else {
    project.age <- max(project$Age.Months)
  }

  projects[p, ] <- c(
    "pid"=as.numeric(pid),
    "time"=as.numeric(project.age),
    "group"=as.numeric(project.group),
    "status"=as.numeric(project.status)
  )
  p <- p + 1
}

rm(all.pids, control.pids, control.pids.sample, dead.pids, group.size, patterns.dead.pids, sequences.pids, sequences.pids.sample)
rm(i, p, pid, project, project.status, project.group, project.age)

surv.fit <- survfit(Surv(time, status) ~ group, data=projects, type="kaplan-meier")
ggkm(
  surv.fit,
  timeby=opts.time.interval,
  main=paste("Survivial of projects by", opts.var, "having patterns of type", opts.pattern.type),
  xlabs=opts.time
)

rm(opts.time.interval, opts.time, opts.var, surv.fit)
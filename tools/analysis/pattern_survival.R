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
opts.reprsample <- TRUE

patterns.dead <- read.csv2(paste("output", paste(paste("patterns", opts.time, opts.var, sep="_"), "csv", sep="."), sep="/"))
sequences.data <- read.csv2(paste("output", paste(paste("haar", "similar", opts.time, opts.var, sep="_"), "csv", sep="."), sep="/"))
sequences.locations <- read.csv2(paste("output", paste(paste("sequenceLocations", opts.var, sep="_"), "csv", sep="."), sep="/"))
dead.data <- read.csv2("output/deadProjectsValidated.csv")
facts.data <- read.csv2("data/factsForAnalysis.csv")

# Store PIDs of dead projects
dead.pids <- unique(dead.data[as.logical(dead.data$confirmed.dead) == TRUE, ]$pid)

# Set which pattern type should be analysed
patterns <- patterns.dead[patterns.dead$seq.type == opts.pattern.type, ]
patterns$pids <- strsplit(as.character(patterns$pids), split="|", fixed=TRUE)

# Retrieve the PIDs having sequences for the patterns under analysis
sequences <- sequences.data[sequences.data$Seq.Id %in% patterns$seq.id, ]
sequences.pids <- unique(as.numeric(unlist(strsplit(as.character(sequences$Project.list), "|", fixed=TRUE))))

sequences.data$Project.list <- strsplit(as.character(sequences.data$Project.list), split="|", fixed=TRUE)
sequences.data$Project.id.ss.rl <- strsplit(as.character(sequences.data$Project.id.ss.rl), split="|", fixed=TRUE)

# Retrieve the PIDs having no sequences for the patterns under analysis
control.pids <- unique(facts.data[!(facts.data$Project.Id %in% sequences.pids), ]$Project.Id)

# Select group
group.size <- min(
  c(
    length(control.pids),
    length(sequences.pids)
  )
)
sequences.pids.sample <- sample(sequences.pids, group.size)

if(opts.reprsample){
  ## Representative sample selection
  source("tools/analysis/sample_project_selection.R")
  ohloh <- read.delim("../OhlohAnalytics/lib/SampleSoftwareProjects-0.1.1/masterdata_filtered.csv", header=T, na.strings=c("", "NA"))
  ohloh.sub <- ohloh[ohloh$id %in% control.pids, ]
  sample <- ohloh.sub[ohloh.sub$id == 4361, ] # 4361 : netbsd
  np <- next.projects(group.size, sample, universe=ohloh.sub, id ~ total_code_lines + twelve_month_contributor_count)
  new.projects <- as.data.frame(np[1])
  control.pids.sample <- new.projects$new.projects.id

  colnames(new.projects) <- colnames(ohloh.sub)
  new.projects.score <- score.projects(new.projects, universe=ohloh.sub, id ~ total_code_lines + twelve_month_contributor_count)
} else {
  control.pids.sample <- sample(control.pids, group.size)
}

# Store PIDs of both groups
all.pids <- unique(as.numeric(append(sequences.pids.sample, control.pids.sample)))

# Prepare final data frame
projects <- data.frame(
  "pid"=numeric(0),
  "time"=numeric(0),
  "group"=numeric(0),
  "status"=numeric(0),
  "ttl"=numeric(0),
  "diagnosed"=numeric(0)
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
    project.age <- max.months - diff.months # The age of dying

    rm(project.death, max.date, dead.date, max.months, diff.months)
  }
  else {
    project.age <- max(project$Age.Months)
  }

  project.diagnosed <- NA
  
  if(project.group) {
    pattern.locs <- sequences.data[sequences.data$pid0 == pid
                                   & sequences.data$Seq.Id %in% sequences$Seq.Id, ]
    
    if(nrow(pattern.locs) == 0){
      pattern.locs <- sequences.data[pid %in% unlist(sequences.data$Project.list)
                                     & sequences.data$Seq.Id %in% sequences$Seq.Id, ]

      if(nrow(pattern.locs) == 0){
        stop(paste("No patterns for pid", pid))
      }

      pattern.locs <- sequences.data[pid %in% unlist(sequences.data$Project.list), ]
      for(l in 1:length(pattern.locs$Project.id.ss.rl)){
        regions <- pattern.locs$Project.id.ss.rl[l]
        regions <- strsplit(unlist(regions), split="-", fixed=TRUE)
        
        for(r in 1:length(regions)){
          region <- as.numeric(unlist(regions[r]))
          if(region[1] != pid){
            next
          }
          
          pattern.locs <- sequences.data[sequences.data$pid0 == pid
                                       & sequences.data$pid0.revlevel == region[2]
                                       & sequences.data$pid0.startseq == region[3], ]
          break
        }
        
        if(nrow(pattern.locs) > 0){
          break
        }
      }
    }

    if(nrow(pattern.locs) > 0){
      patterns.locations <- sequences.locations[sequences.locations$seq.id %in% pattern.locs$Seq.Id, ]
      project.diagnosed <- min(patterns.locations$seq.start)
    }
  }

  projects[p, ] <- c(
    "pid"=as.numeric(pid),
    "time"=as.numeric(project.age),
    "group"=as.numeric(project.group),
    "status"=as.numeric(project.status),
    "ttl"=as.numeric(project.age - project.diagnosed),
    "diagnosed"=as.numeric(project.diagnosed)
  )
  p <- p + 1
}

rm(all.pids, control.pids, control.pids.sample, dead.pids, group.size, sequences.pids, sequences.pids.sample)
rm(i, p, pid, project, project.status, project.group, project.age)

surv.fit <- survfit(Surv(time, status) ~ group, data=projects, type="kaplan-meier")
ggkm(
  surv.fit,
  timeby=opts.time.interval,
  main=paste("OSS projects with Type", opts.pattern.type, "pattern on", opts.var),
  xlabs=opts.time
)

rm(opts.time.interval, opts.time, opts.var, opts.pattern.type, ggkm)

write.csv2(projects, paste("output", paste(paste("survival", "Age.Months", "LOC", sep="_"), "csv", sep="."), sep="/"))
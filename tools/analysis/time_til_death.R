# Computes a data set with time-til-death data for all dead projects and sequences.

library(plyr)
library(zoo)

metric <- "LOC"

locations.data <- read.csv2(paste("output", paste(paste("sequenceLocations", metric, sep="_"), "csv", sep="."), sep="/"))
dead.data <- read.csv2("output/deadProjectsValidated.csv")
dead.data <- dead.data[as.logical(dead.data$confirmed.dead) == TRUE, ]
facts.data <- read.csv2("data/factsForAnalysis.csv")

output.cols <- c(
  "seq.id",
  "pid",
  "age",
  "died",
  "seq.start",
  "time.to.death"
)
output.data <- as.data.frame(
  matrix(
    ncol=length(output.cols),
    nrow=0
  ),
  stringsAsFactors=FALSE
)
colnames(output.data) <- output.cols

sids <- unique(locations.data$seq.id)
i <- 0

for(sid in sids){
  seq.loc <- locations.data[locations.data$seq.id == sid, ]
  pid <- as.numeric(seq.loc$pid)

  if(!(pid %in% dead.data$pid)){
    next
  }

  project <- facts.data[facts.data$Project.Id == pid, ]
  death <- dead.data[dead.data$pid == pid, ]

  max.date <- max(as.Date(project$Date))
  dead.date <- as.Date(death$dead.date)
  max.months <- max(project$Age.Months)
  diff.months <- floor(12 * as.numeric(as.yearmon(max.date) - as.yearmon(dead.date)))
  died.after <- max.months - diff.months

  seq.start <- seq.loc$seq.start
  ttd <- died.after - seq.start

  row <- c(
    "seq.id"=sid,
    "pid"=pid,
    "age"=max(project$Age.Months),
    "died"=died.after,
    "seq.start"=seq.start,
    "time.to.death"=ttd
  )
  output.data <- rbind(output.data, row)
  colnames(output.data) <- output.cols
}

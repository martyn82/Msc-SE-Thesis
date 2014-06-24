#pattern.ids <- c(10081, 15138, 15139, 15790, 3256, 3257, 3397, 3398)
pattern.ids <- c(3256)
xlab <- "time (months)"
ylab <- "LOC"

facts.data <- read.csv2("data/factsForAnalysis.csv")
sequences.locations <- read.csv2("output/sequenceLocations_LOC.csv")
patterns <- read.csv2("output/patterns_Age.Months_LOC.csv")

for (p in pattern.ids) {
  pattern <- patterns[patterns$seq.id == p, ]
  pids <- as.numeric(unlist(strsplit(as.character(pattern$pids), split="|", fixed=T)))

  for (pid in pids) {
    pattern.location <- sequences.locations[sequences.locations$seq.id == pattern$seq.id, ]

    if (nrow(pattern.location) != 1) {
      warning(paste("Found ", nrow(pattern.location), "locations"))
    }

    project <- facts.data[facts.data$Project.Id == pid, ]

    rel.start <- pattern.location$seq.start.rel
    rel.end <- pattern.location$seq.end.rel
    total <- nrow(project)

    abs.start <- round(total * rel.start)
    abs.end <- round(total * rel.end)

    project.sequence <- project[project$Age.Months >= abs.start
                                & project$Age.Months <= abs.end, ]
    sequence <- as.numeric(project.sequence$LOC)

    plot(
      x=abs.start:abs.end,
      y=sequence,
      type="l",
      xlab=xlab,
      ylab=ylab,
      main=paste("pattern #", p, "in project #", pid)
    )
  }
}
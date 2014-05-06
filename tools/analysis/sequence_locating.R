# This script evaluates the sequences found in a certain group of projects.
# It evaluates each sequence and outputs a sheet of the sequence related to a project and the relative position
# in the project's evolution where it occurred.

# 1. Pick a group of projects.
# 2. Read their sequences.
# 3. Find the relative position of the sequence in the life-time of the project.
#    - Pick project Age.Months
#    - Pick number of observations in revlevel of the sequence.
#    - Calculate the percentage of the first observation of the sequence.
#    - Calculate the percentage of the last observation of the sequence.

input.pid.col <- "pid"
metric <- "LOC"

input.data <- read.csv2(paste("output", "deadProjectsValidated.csv", sep="/"))

sim.data <- read.csv2(paste("output", paste(paste("haar", "similar", "Age.Months", metric, sep="_"), "csv", sep="."), sep="/"))
facts.data <- read.csv2(paste("data", "factsForAnalysis.csv", sep="/"))
wavelet.dir <- paste("output", "wavelet_cum", sep="/")

output.file <- paste("output", paste(paste("sequenceLocations", metric, sep="_"), "csv", sep="."), sep="/")
output.cols <- c(
  "pid",           # The project ID
  "max.age",       # The age of the project at the latest observation in months
  "seq.id",        # The ID of the sequence
  "seq.revlevel",  # The revlevel of the sequence
  "seq.start",     # The start month of the sequence
  "seq.end",       # The end month of the sequence
  "seq.length",    # The length of the sequence in months
  "seq.start.rel", # The start of the sequence relative to the project's age
  "seq.end.rel"    # The end of the sequence relative to the project's age
)
output.data <- as.data.frame(
  matrix(
    ncol=length(output.cols),
    nrow=0
  ),
  stringsAsFactor=FALSE
)
colnames(output.data) <- output.cols

pids <- unique(input.data[[input.pid.col]])
sim.data <- subset(sim.data, sim.data$pid0 %in% pids)

for(pid in pids){
  project <- facts.data[(facts.data$Project.Id == pid), ]
  project.age <- max(project$Age.Months)
  project.obs <- nrow(project)

  project.regions <- sim.data[(sim.data$pid0 == pid), ]
  
  if(nrow(project.regions) == 0){
    warning(paste("No similar sequences in project", pid))
    next
  }

  for(i in 1:nrow(project.regions)){
    region <- project.regions[i, ]

    dwt.dwtvar <- as.character(region$DWT_var)
    dwt.timecol <- as.character(region$Time.col)
    dwt.varcol <- as.character(region$Variable)

    wavelet.file <- paste("haar", dwt.timecol, dwt.dwtvar, dwt.varcol, pid, "dwt.csv", sep="_")
    wavelet.data <- read.csv2(paste(wavelet.dir, wavelet.file, sep="/"))

    region.startseq <- region$pid0.startseq
    region.revlevel <- region$pid0.revlevel
    region.length   <- region$length
    region.end      <- region.startseq + region.length - 1 # startseq should be inclusive

    wavelet <- wavelet.data[(wavelet.data$revlevel == region.revlevel), ]
    wavelet.length <- nrow(wavelet)

    relative.start <- region.startseq / wavelet.length
    months.start <- round(project.obs * relative.start - 1)

    relative.end <- region.end / wavelet.length
    months.end <- round(project.obs * relative.end - 1)

    months.length <- round(months.end - months.start)

    if(months.end > project.obs - 1){
      stop("The seq.end cannot be larger than the project's age. There's an error in the script.")
    }

    row <- c(
      "pid"=pid,
      "max.age"=project.age,
      "seq.id"=region$Seq.Id,
      "seq.revlevel"=region.revlevel,
      "seq.start"=months.start,
      "seq.end"=months.end,
      "seq.length"=months.length,
      "seq.start.rel"=relative.start,
      "seq.end.rel"=relative.end
    )

    output.data <- rbind(output.data, row)
    colnames(output.data) <- output.cols
  }
}

write.csv2(output.data, file=output.file)

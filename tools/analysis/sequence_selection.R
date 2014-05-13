# This script selects and analyses the sequences found at locations during a project.
#
# PART 1:
# It takes the sequenceLocations data for all projects.
# It will select the sequences in the following order:
# 1. Select sequences for a project.
# 2. Filter by start time and keep earliest.
# 3. Filter by length and keep longest.
# 4. Filter by revlevel and keep maximum (most detail).
# The resulting frame contains the longest, most detailed sequences lasting near the end of evolution data.
#
# PART 2:
# Then the sequences are evaluated and the following data will be extracted:
# a. The LOC difference during the sequence.
# b. The Active.Developers difference during the sequence.
# c. The LOC.Churn difference during the sequence.
#
# Afterwards, it should be clear if such sequences typically indicate a project is already dead or is 'dying'.

# Part 1
metric <- "LOC"
locs <- read.csv2(paste("output", paste(paste("sequenceLocations", metric, sep="_"), "csv", sep="."), sep="/"))
pids <- unique(as.numeric(locs$pid))

sequences <- data.frame(
  "pid"=numeric(0),
  "max.age"=numeric(0),
  "seq.id"=numeric(0),
  "seq.revlevel"=numeric(0),
  "seq.start"=numeric(0),
  "seq.end"=numeric(0),
  "seq.length"=numeric(0),
  "seq.start.rel"=numeric(0),
  "seq.end.rel"=numeric(0)
)
s <- 1

for(pid in pids){
  seqs <- locs[as.numeric(locs$pid) == pid, ]
  seqs <- seqs[seqs$seq.revlevel == max(seqs$seq.revlevel), ]
  seqs <- seqs[seqs$seq.start == min(seqs$seq.start), ]
  seqs <- seqs[seqs$seq.length == max(seqs$seq.length), ]

  if(dim(seqs)[1] > 1){
    stop("More than one sequence left, need more filtering.")
  }

  if(dim(seqs)[1] == 0){
    warning(paste("No more sequences. Too much filtering? pid:", pid))
  }

  sequences[s, ] <- c(
    "pid"=as.numeric(pid),
    "max.age"=as.numeric(seqs$max.age),
    "seq.id"=as.numeric(seqs$seq.id),
    "seq.revlevel"=as.numeric(seqs$seq.revlevel),
    "seq.start"=as.numeric(seqs$seq.start),
    "seq.end"=as.numeric(seqs$seq.end),
    "seq.length"=as.numeric(seqs$seq.length),
    "seq.start.rel"=as.numeric(seqs$seq.start.rel),
    "seq.end.rel"=as.numeric(seqs$seq.end.rel)
  )
  s <- s + 1
}
rm(s, metric, pid, pids, locs, seqs)

# Part 2
facts <- read.csv2("data/factsForAnalysis.csv")

stats <- data.frame(
  "pid"=numeric(0),
  "seq.id"=numeric(0),
  "max.loc.diff"=numeric(0),
  "max.ad.diff"=numeric(0),
  "max.lc.diff"=numeric(0)
)
i <- 1

for(s in 1:nrow(sequences)){
  seq <- sequences[s, ]
  pid <- seq$pid
  project <- facts[facts$Project.Id == pid, ]
  project.seq <- project[project$Age.Months >= seq$seq.start, ]

  diff.loc <- max(abs(diff(project.seq$LOC)))
  has.diff.loc <- (max(diff.loc) != 0)

  diff.ad <- max(abs(diff(project.seq$Active.Developers)))
  has.diff.ad <- (max(diff.ad) != 0)

  diff.lc <- max(abs(diff(project.seq$LOC.Churn)))
  has.diff.lc <- (max(diff.lc) != 0)

  stats[i, ] <- c(
    "pid"=pid,
    "seq.id"=seq$seq.id,
    "max.loc.diff"=diff.loc,
    "max.ad.diff"=diff.ad,
    "max.lc.diff"=diff.lc
  )
  i <- i + 1
}
rm(diff.ad, diff.lc, diff.loc, has.diff.ad, has.diff.lc, has.diff.loc)
rm(i, s, seq, pid, facts, sequences, project, project.seq)

# This script selects and analyses the sequences found at locations during a project.
#
# It takes the sequenceLocations data for all projects.
# Then the sequences are evaluated and the following data will be extracted:
# a. The LOC difference during the sequence.
# b. The Active.Developers difference during the sequence.
# c. The LOC.Churn difference during the sequence.
#
# Afterwards, it should be clear if sequences typically indicate a project is already dead or is 'dying'.

metric <- "LOC"
locs <- read.csv2(paste("output", paste(paste("sequenceLocations", metric, sep="_"), "csv", sep="."), sep="/"))
pids <- unique(as.numeric(locs$pid))
facts <- read.csv2("data/factsForAnalysis.csv")

stats <- data.frame(
  "pid"=numeric(0),
  "seq.id"=numeric(0),
  "rel.loc.diff"=numeric(0),
  "rel.ad.diff"=numeric(0),
  "rel.lc.diff"=numeric(0),
  "max.rel.loc.diff"=numeric(0),
  "max.rel.ad.diff"=numeric(0),
  "max.rel.lc.diff"=numeric(0),
  "max.loc.diff"=numeric(0),
  "max.ad.diff"=numeric(0),
  "max.lc.diff"=numeric(0),
  "loc.seq"=character(0),
  "ad.seq"=character(0),
  "lc.seq"=character(0),
  stringsAsFactors=FALSE
)
i <- 1

reldiff <- function(x){
  y <- c()
  for(i in 1:length(x)-1){
    y[i] <- (x[i+1] - x[i]) / x[i]
  }
  return(as.numeric(y))
}

for(s in 1:nrow(locs)){
  seq <- locs[s, ]
  pid <- seq$pid
  project <- facts[facts$Project.Id == pid, ]
  project.seq <- project[project$Age.Months >= seq$seq.start, ]

  diff.loc <- max(abs(diff(project.seq$LOC)))
  has.diff.loc <- (max(diff.loc) != 0)
  rel.diff <- reldiff(project.seq$LOC)
  rel.diff.loc <- paste(rel.diff, sep="|", collapse="|")
  max.rel.diff.loc <- max(abs(rel.diff))
  loc.seq <- paste(project.seq$LOC, sep="|", collapse="|")

  diff.ad <- max(abs(diff(project.seq$Active.Developers)))
  has.diff.ad <- (max(diff.ad) != 0)
  rel.diff <- reldiff(project.seq$Active.Developers)
  rel.diff.ad <- paste(reldiff(rel.diff), sep="|", collapse="|")
  max.rel.diff.ad <- max(abs(rel.diff))
  ad.seq <- paste(project.seq$Active.Developers, sep="|", collapse="|")

  diff.lc <- max(abs(diff(project.seq$LOC.Churn)))
  has.diff.lc <- (max(diff.lc) != 0)
  rel.diff <- reldiff(project.seq$LOC.Churn)
  rel.diff.lc <- paste(reldiff(rel.diff), sep="|", collapse="|")
  max.rel.diff.lc <- max(abs(rel.diff))
  lc.seq <- paste(project.seq$LOC.Churn, sep="|", collapse="|")

  stats[i, ] <- c(
    "pid"=pid,
    "seq.id"=seq$seq.id,
    "rel.loc.diff"=rel.diff.loc,
    "rel.ad.diff"=rel.diff.ad,
    "rel.lc.diff"=rel.diff.lc,
    "max.rel.loc.diff"=max.rel.diff.loc,
    "max.rel.ad.diff"=max.rel.diff.ad,
    "max.rel.lc.diff"=max.rel.diff.lc,
    "max.loc.diff"=diff.loc,
    "max.ad.diff"=diff.ad,
    "max.lc.diff"=diff.lc,
    "loc.seq"=loc.seq,
    "ad.seq"=ad.seq,
    "lc.seq"=lc.seq
  )
  i <- i + 1
}
rm(reldiff, rel.diff)
rm(diff.ad, diff.lc, diff.loc, has.diff.ad, has.diff.lc, has.diff.loc, loc.seq, ad.seq, lc.seq, rel.diff.loc, rel.diff.ad, rel.diff.lc, max.rel.diff.loc, max.rel.diff.ad, max.rel.diff.lc)
rm(pids, i, s, seq, pid, facts, locs, project, project.seq)

output.file <- paste("output", paste(paste("sequenceMetaData", metric, sep="_"), "csv", sep="."), sep="/")
write.csv2(stats, file=output.file)
rm(output.file, metric)

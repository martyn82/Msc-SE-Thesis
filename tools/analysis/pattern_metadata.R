# Combines the patterns found with their sequence metadata.

metric <- "LOC"
timevar <- "Age.Months"

patterns <- read.csv2(paste("output", paste(paste("patterns", timevar, metric, sep="_"), "csv", sep="."), sep="/"))
sequences <- read.csv2(paste("output", paste(paste("sequenceMetaData", metric, sep="_"), "csv", sep="."), sep="/"))
similar <- read.csv2(paste("output", paste(paste("haar_similar", timevar, metric, sep="_"), "csv", sep="."), sep="/"))

patterns.metadata <- data.frame(
  "seq.id"=numeric(0),
  "seq.type"=character(0),
  "pids"=character(0),
  "rel.loc.diff"=numeric(0),
  "rel.ad.diff"=numeric(0),
  "rel.lc.diff"=numeric(0),
  "max.rel.loc.diff"=numeric(0),
  "max.rel.ad.diff"=numeric(0),
  "max.rel.lc.diff"=numeric(0),
  "max.loc.diff"=numeric(0),
  "max.ad.diff"=numeric(0),
  "max.lc.diff"=numeric(0),
  "has.dead"=logical(0),
  "has.alive"=logical(0),
  "has.mixed"=logical(0),
  "loc.seq"=character(0),
  "ad.seq"=character(0),
  "lc.seq"=character(0),
  stringsAsFactors=FALSE
)
p <- 1

sids <- unique(as.numeric(patterns$seq.id))
for(sid in sids){
  seq <- sequences[sequences$seq.id == sid, ]
  pattern <- patterns[patterns$seq.id == sid, ]
  sim <- similar[similar$Seq.Id == sid, ]

  patterns.metadata[p, ] <- c(
    "seq.id"=as.numeric(sid),
    "seq.type"=as.character(pattern$seq.type),
    "pids"=as.character(pattern$pids),
    "rel.loc.diff"=as.numeric(seq$rel.loc.diff),
    "rel.ad.diff"=as.numeric(seq$rel.ad.diff),
    "rel.lc.diff"=as.numeric(seq$rel.lc.diff),
    "max.rel.loc.diff"=as.numeric(seq$max.rel.loc.diff),
    "max.rel.ad.diff"=as.numeric(seq$max.rel.ad.diff),
    "max.rel.lc.diff"=as.numeric(seq$max.rel.lc.diff),
    "max.loc.diff"=as.numeric(seq$max.loc.diff),
    "max.ad.diff"=as.numeric(seq$max.ad.diff),
    "max.lc.diff"=as.numeric(seq$max.lc.diff),
    "has.dead"=as.logical(sim$HasDead),
    "has.alive"=as.logical(sim$HasAlive),
    "has.mixed"=as.logical(sim$HasMixed),
    "loc.seq"=as.character(seq$loc.seq),
    "ad.seq"=as.character(seq$ad.seq),
    "lc.seq"=as.character(seq$lc.seq)
  )
  p <- p + 1
}
rm(sid, sids, p, similar, sim, pattern, patterns, seq, sequences)

output.file <- paste("output", paste(paste("patternMetaData", timevar, metric, sep="_"), "csv", sep="."), sep="/")
write.csv2(patterns.metadata, file=output.file)
rm(output.file, timevar, metric)

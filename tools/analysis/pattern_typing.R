# This script takes the dead projects and verifies their patterns.
# It classifies the patterns observed in dead projects by type.
#
# +------+---------------------------------------------------+
# | Type | Description                                       |
# +------+---------------------------------------------------+
# |  A   | Strictly occurring at the end of project data.    |
# |  B   | Strictly occurring not at the end of project data.|
# |  AB  | Others.                                           |
# +------+---------------------------------------------------+

options(warn=2)

metric <- "LOC"

similar.file <- paste("output", paste(paste("haar", "similar", "Age.Months", metric, sep="_"), "csv", sep="."), sep="/")
wavelet.dir <- paste("output", "wavelet_cum", sep="/")

similar.data <- read.csv2(similar.file)

if(dim(similar.data)[1] == 0){
  stop(paste("No similar data for this metric:", metric))
}

dead.data <- read.csv2(paste("output", "deadProjectsValidated.csv", sep="/"))
dead.confirmed <- subset(dead.data, as.logical(dead.data$confirmed.dead) == TRUE)
dead.pids <- unique(dead.confirmed$pid)

similar.dead <- subset(similar.data, similar.data$pid0 %in% dead.pids)
similar.count <- nrow(similar.dead)

# Classify the patterns in dead projects.
# 1. Get list of dead projects (dead.pids)
# 2. Get list of patterns for all these dead projects (similar.dead)
# 3. Browse through the patterns and classify by type.

patterns <- data.frame(
  "seq.id"=numeric(0),
  "seq.type"=character(0),
  "pid"=numeric(0),
  stringsAsFactors=FALSE
)
patterns.cols <- colnames(patterns)
p <- 1

for(i in 1:similar.count){
  similar.row <- similar.dead[i, ]

  similar.regions <- as.character(similar.row$Project.id.ss.rl)
  similar.regions <- as.character(unlist(strsplit(similar.regions, split="|", fixed=TRUE)))

  timecol <- similar.row$Time.col
  dwtvar <- similar.row$DWT_var
  varcol <- similar.row$Variable

  for(j in 1:length(similar.regions)){
    region <- as.numeric(unlist(strsplit(similar.regions[j], split="-", fixed=TRUE)))
    region.pid <- region[1]
    region.revlevel <- region[2]
    region.startseq <- region[3]
    region.length <- as.numeric(similar.row$length)

    if(!(region.pid %in% dead.pids)){
      # We are only interested in evaluating patterns in dead projects.
      next
    }

    seq.file <- paste("haar", timecol, dwtvar, varcol, region.pid, "dwt.csv", sep="_")
    seq.data <- read.csv2(paste(wavelet.dir, seq.file, sep="/"))

    seq.region.untrim <- subset(
      seq.data,
      seq.data$revlevel == region.revlevel
      & seq.data$seq >= region.startseq
    )

    seq.region <- subset(
      seq.region.untrim,
      seq.region.untrim$seq <= (region.startseq + region.length)
    )

    if(max(seq.region.untrim$seq) == max(seq.region$seq)){
      pattern.type <- "A"
    } else {
      pattern.type <- "B"
    }
    
    patterns[p, ] <- c(
      "seq.id"=as.numeric(similar.row$Seq.Id),
      "seq.type"=as.character(pattern.type),
      "pid"=as.numeric(region.pid)
    )
    p <- p + 1
  }
}

rm(p, pattern.type, seq.region.untrim, seq.region, seq.data, seq.file)
rm(region.startseq, region.length, region, region.revlevel, region.pid)
rm(similar.data, similar.row, similar.regions, similar.dead, similar.count, similar.file)
rm(dead.pids, dead.data, dead.confirmed, wavelet.dir, metric, dwtvar)

# Strictly type the patterns
for(p in 1:dim(patterns)[1]){
  pattern <- patterns[p, ]
  is.type <- pattern$seq.type
  is.other <- dim(patterns[patterns$seq.id == pattern$seq.id & patterns$seq.type != is.type, ])[1] > 0
  
  if(is.other){
    pattern$seq.type <- "AB"
    patterns[p, ] <- pattern
  }
}

rm(pattern, p, is.type, is.other)

# Aggregate unique patterns by merging pid list
patterns <- aggregate(patterns$pid, by=list(patterns$seq.id, patterns$seq.type), function(x){paste(unique(x), sep="|", collapse="|")})
colnames(patterns) <- patterns.cols

output.file <- paste("output", paste(paste("patterns", timecol, varcol, sep="_"), "csv", sep="."), sep="/")
write.csv2(patterns, file=output.file)

rm(output.file, varcol, timecol)

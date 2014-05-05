# This script takes the dead projects and verifies their patterns.
# It classifies the patterns observed in dead projects by type.
# Types:
# +------+--------------------------------------+
# | Type | Description                          |
# +------+--------------------------------------+
# |  A   | Occurs at the end of project data.   |
# |  B   | Others.                              |
# +------+--------------------------------------+

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

type.a <- c()
type.b <- c()

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
      # Type A
      type.a <- append(type.a, c(similar.row$Seq.Id))
    } else {
      # Type B
      type.b <- append(type.b, c(similar.row$Seq.Id))
    }
  }
}

type.a <- unique(type.a)
type.b <- unique(type.b)
type.ab <- unique(intersect(type.a, type.b))

type.a <- type.a[!(type.a %in% type.ab)]
type.b <- type.b[!(type.b %in% type.ab)]

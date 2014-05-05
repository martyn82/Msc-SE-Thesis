# Script that does the following:
# 1. Get data.
#   a. Read haar_similar.csv
#   b. Read deadProjects.csv
# 2. Select dead project sequences from haar_similar.
#   a. Read the list of project IDs from the sequences that have similar regions.
#   b. Filter the dead projects from the list, keeping the non-dead ones.
# 3. Read the sequences of the projects in the list that are similar.
#   a. Identify if they might be dying: the sequence is at the end of the project evolution wave

metric <- "LOC"

similar.file <- paste("output", paste(paste("haar", "similar", "Age.Months", metric, sep="_"), "csv", sep="."), sep="/")
dead.file <- paste("output", "deadProjectsValidated.csv", sep="/")
wavelet.dir <- paste("output", "wavelet_cum", sep="/")

output.file <- paste("output", paste(paste("dyingProjects", metric, sep="_"), "csv", sep="."), sep="/")
output.cols <- c(
  "pid",          # The project that is potential dying
  "max.revlevel", # The maximum level of detail of the sequence that identified the project as dying
  "dead.count",   # The number of dead projects matching the sequence
  "deads"         # The set of dead pids with the same sequence
#  "match.count"   # The number of times the sequence occurred (must be the last column)
)
output.data <- as.data.frame(
  matrix(ncol=length(output.cols), nrow=0)
)
colnames(output.data) <- output.cols

print("Reading files...")

similar.data <- read.csv2(similar.file)

if(dim(similar.data)[1] == 0){
  stop(paste("No similar data for this metric:", metric))
}

dead.data <- read.csv2(dead.file)
dead.data.confirmed <- subset(dead.data, as.logical(dead.data$confirmed.dead) == TRUE)

print("Selecting sequences of dead projects...")

dead.pids <- unique(dead.data.confirmed$pid)
similar.dead <- subset(similar.data, similar.data$pid0 %in% dead.pids)
similar.rowcount <- dim(similar.dead)[1]

print("Searching for dying projects...")

for(i in 1:similar.rowcount){
  similar.row <- similar.dead[i, ]
  
  # extract the list of projects and split them into a list of pids
  projects.list <- as.character(similar.row$Project.list)
  projects.pids <- as.numeric(unlist(strsplit(projects.list, split="|", fixed=TRUE)))
  
  # the dead pids in the list
  projects.pids.dead <- subset(projects.pids, projects.pids %in% dead.pids)
  # the other pids in the list
  projects.pids.other <- subset(projects.pids, !(projects.pids %in% dead.pids))

  # similar regions from the pids
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

    if(!(region.pid %in% projects.pids.other)){
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

    # if the sequence occurs at the end of the evolution data, then add it
    if(max(seq.region.untrim$seq) == max(seq.region$seq)){
      row <- data.frame(
        pid=region.pid,
        max.revlevel=region.revlevel,
        #match.count=1,
        dead.count=length(projects.pids.dead),
        deads=do.call(paste, c(as.list(projects.pids.dead), sep="|"))
      )

      output.data <- rbind(output.data, row)

      print("Added sequence to potential dying projects...")
    }
  }
}

# print("Aggregating data...")
# output.data <- aggregate(
#   output.data$match.count,
#   by=list(
#     output.data$pid,
#     output.data$max.revlevel
#   ),
#   FUN=sum
# )
# colnames(output.data) <- output.cols
# output.data <- lapply(split(output.data, output.data$pid), function(df){
#   df[which.max(df$max.revlevel), length(output.cols)]
# })
# output.data <- do.call(rbind, output.data)

dying.pids.count <- length(unique(output.data$pid))
print(paste("Found", dying.pids.count, "potential dying projects."))

print("Writing results...")
write.csv2(output.data, file=output.file)

print("Done.")

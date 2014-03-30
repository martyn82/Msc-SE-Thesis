# This script analyzes the outputs of wavelet.R and identifies similar sequences in DWT data.

library("hash")
library("multicore")

input.folder <- "scripts/output/wavelet"
output.folder <- "scripts/output/sequence"

if(!file.exists(output.folder)){
  dir.create(output.folder)
}

allowed.deviation <- 0.005
minimum.sequence <- 3
minimum.revlevel <- ceiling(log(minimum.sequence) / log(2))
maximum.searchseq <- 65
filter.name <- "haar"

idcol <- "pid"
timecols <- c("Age.Days", "Active.Developers")
dwtvars <- c("V", "W")

varcols <- hash(keys=timecols)
varcols[["Age.Days"]] <- c("LOC", "Commit.LOC.Churn", "Active.Developers")
varcols[["Active.Developers"]] <- c("Commit.LOC.Churn")

findSimilarSequences <- function(timecol, target.seq, pid, search.seq, search.revlevel, pid0, varcol, dwtvar, revlevel){
  if(length(search.seq) < minimum.sequence || length(target.seq) < minimum.sequence){
    return()
  }
  
  for(i in c(1:(length(search.seq) - minimum.sequence + 1))){
    ss2 <- 1

    if(pid == pid0 & search.revlevel == revlevel & i <= length(target.seq) - minimum.sequence){
      ss2 <- i + 1
    }
    
    for(j in c(ss2:(length(target.seq) - minimum.sequence + 1))){
      mlen <- ((
        if(length(search.seq) - i < length(target.seq) - j){
          length(search.seq) - i
        } else {
          length(target.seq) - j
        }
      ) + 1)

      a0 <- (search.seq[i] - search.seq[i + 1]) / (target.seq[j] - target.seq[j + 1])

      if(is.na(a0) | is.infinite(a0) | a0 <= 0.0){
        next
      }

      a1 <- 1.0 / a0
      b0 <- search.seq[i] - (target.seq[j] * a0)
      b1 <- target.seq[j] - (search.seq[i] * a1)
      divdifdev0 <- abs(((target.seq[j:(j + mlen - 1)] * a0 * b0) / search.seq[i:(i + mlen - 1)]) - 1.0)
      divdifdev1 <- abs(((search.seq[i:(i + mlen - 1)] * a1 * b1) / target.seq[j:(j + mlen - 1)]) - 1.0)

      for(k in c(minimum.sequence:length(divdifdev0))){
        if(
          is.na(divdifdev0[k]) | is.nan(divdifdev0[k]) | is.infinite(divdifdev0[k]) | divdifdev0[k] > allowed.deviation |
          is.na(divdifdev1[k]) | is.nan(divdifdev1[k]) | is.infinite(divdifdev1[k]) | divdifdev1[k] > allowed.deviation
        ){
          break
        }
        
        print(paste("Similar sequence detected", "time", timecol, "var", varcol, "projects", pid, "and", pid0))

        seq.deviation <- format(
          max(c(divdifdev0[1:k], divdifdev1[1:k])),
          decimal.mark=","
        )
        seq.sequence <- paste(
          format(search.seq[i:(i+k-1)], decimal.mark=","),
          collapse="-"
        )
        seq.a0 <- format(a0, decimal.mark=",")
        seq.b0 <- format(b0, decimal.mark=",")

        # "time", "variable", "pid0", "pid0.revlevel", "pid0.start", "pid1", "pid1.revlevel", "pid1.start", "length", "max.deviation", "seq", "a0", "b0"
        dataset.line <- paste(
          timecol,                  # time
          varcol,                   # variable
          pid0,                     # project ID 0
          search.revlevel,          # revlevel of project 1
          i,                        # sequence start at project 1
          pid,                      # project ID 1
          revlevel,                 # revlevel of project 0
          j,                        # sequence start at project 0
          k,                        # sequence length
          seq.deviation,            # maxumim deviation
          seq.sequence,             # the similar sequence
          seq.a0,                   # ?
          seq.b0,                   # ?
          sep=";"
        )
        writeLines(dataset.line, con=output.file.handle)
      }
    }
  }
}

processPIDs <- function(pids, dataset, timecol, dwtvar, varcol){
  if(length(pids) == 0){
    return()
  }

  for(i in 1:length(pids)){
    pid0 <- pids[i]
    dataset.pid0 <- dataset[dataset[[idcol]] == pid0, ]
    pid0.levels <- max(as.numeric(dataset.pid0[["revlevel"]]))

    print(paste("time", timecol, "dwt", dwtvar, "var", varcol, "pid", pid0, "levels", pid0.levels))

    if(minimum.revlevel > pid0.levels){
      next
    }

    for(i.l in c(minimum.revlevel:pid0.levels)){
      pid0.values <- as.numeric(dataset.pid0[dataset.pid0[["revlevel"]] == i.l, "value"])

      if(length(pid0.values) >= maximum.searchseq){
        next
      }

      for(pid1 in pids[i:length(pids)]){
        dataset.pid1 <- dataset[dataset[[idcol]] == pid1, ]
        pid1.levels <- max(as.numeric(dataset.pid1[["revlevel"]]))

        if(minimum.revlevel > pid1.levels){
          next
        }

        for(j.l in c(minimum.revlevel:pid1.levels)){
          pid1.values <- as.numeric(dataset.pid1[dataset.pid1[["revlevel"]] == j.l, "value"])

          if(length(pid1.values) >= maximum.searchseq){
            next
          }

          try(
            findSimilarSequences(timecol, pid1.values, pid1, pid0.values, i.l, pid0, varcol, dwtvar, j.l)
          )
        }
      }
    }
  }
}

process <- function(timecol, dwtvar, varcol){
  output.filename <- paste(filter.name, timecol, dwtvar, varcol, "similar.csv", sep="_")
  output.file.handle <- file(paste(output.folder, output.filename, sep="/"), open="w")
  
  header <- paste("time", "variable", "pid0", "pid0.revlevel", "pid0.start", "pid1", "pid1.revlevel", "pid1.start", "length", "max.deviation", "seq", "a0", "b0", sep=";")
  writeLines(header, con=output.file.handle)
  
  input.filename <- paste("factsForAnalysis", dwtvar, timecol, varcol, "csv", sep=".")
  print(paste("Processing", input.filename, "..."))
  
  dataset <- read.csv2(paste(input.folder, input.filename, sep="/"), header=TRUE)
  pids <- unique(dataset[[idcol]])
  
  processPIDs(pids, dataset, timecol, dwtvar, varcol)
  
  flush(output.file.handle)
  close(output.file.handle)
}

# Starts
print("Similar sequence detection from wavelet transformations...")

proc.list <- list()

for(timecol in timecols){
  timevarcols <- varcols[[timecol]]

  for(dwtvar in dwtvars){
    for(varcol in timevarcols){
      #proc <- try(
        #parallel(
          process(timecol, dwtvar, varcol)
        #)
      #)
      
      #proc.list[[length(proc.list) + 1]] <- proc
    }
  }
}

#collect(proc.list)

print("Done")

# This script performs wavelet transformation on evolution data.
# 1. It takes the monthly facts for analysis data and outputs a csv file per project per variable.
# 2. It aggregates the variables for all projects.

library("hash")
library("multicore")
library("wavelets")
library("zoo")

dataset <- read.csv2("data/factsForAnalysis.csv")
output <- "scripts/output/wavelet"
output.plots <- paste(output, "plots", sep="/")
makeplot <- FALSE

if(!file.exists(output)){
  dir.create(output)
}

if(!file.exists(output.plots)){
  dir.create(output.plots)
}

# Number of simultaneous processes
procs.sim <- 32

# W := Wavelet coefficients, V := Scaling coefficents
dwtvars <- c("V", "W")

idcol <- "Project.Id"
namecol <- "Project.Name"
timecols <- c("Age.Days", "Active.Developers")

varcols <- hash(keys=timecols)
varcols[["Age.Days"]] <- c("LOC", "Commit.LOC.Churn", "Active.Developers")
varcols[["Active.Developers"]] <- c("Commit.LOC.Churn")

# project ids
pids <- unique(dataset[[idcol]])
dst.dwt.columns <- c("seq", "variable", "pid", "coefficient", "level", "value", "revlevel")

# Initialize data frames per timeseries column, for each coefficient.
# Age.Days
dst.dwt.W.Age.Days <- as.data.frame(
  matrix(nrow=0, ncol=length(dst.dwt.columns))
)
dst.dwt.V.Age.Days <- as.data.frame(
  matrix(nrow=0, ncol=length(dst.dwt.columns))
)
colnames(dst.dwt.W.Age.Days) <- dst.dwt.columns
colnames(dst.dwt.V.Age.Days) <- dst.dwt.columns

# Active.Developers
dst.dwt.W.Active.Developers <- as.data.frame(
  matrix(nrow=0, ncol=length(dst.dwt.columns))
)
dst.dwt.V.Active.Developers <- as.data.frame(
  matrix(nrow=0, ncol=length(dst.dwt.columns))
)
colnames(dst.dwt.W.Active.Developers) <- dst.dwt.columns
colnames(dst.dwt.V.Active.Developers) <- dst.dwt.columns

# Performs wavelet transformation on given project variable and timeseries.
calculateColumnDWT <- function(project, varcol, pid, timecol, makeplot){
  project.name <- as.character(unique(project[[namecol]]))

  time.order.fn <- as.numeric
  aggregate.fn <- sum # this needs revision!!

#   project.zoo <- zoo(project[[varcol]], order.by=time.order.fn(project[[timecol]]))
#   project.zoo.idx <- index(project.zoo)
#   project.zoo.vals <- aggregate(project.zoo, project.zoo.idx, aggregate.fn)
#   project.zoo <- zoo(project.zoo.vals, order.by=unique(project.zoo.idx))

#   project.times <- min(project[[timecol]]):max(project[[timecol]])

#   project.aggregated <- rep(0, length(project.times))
#   project.aggregated[index(project.zoo) - project.times[1] + 1] <- project.zoo
#   project.aggregated <- zoo(project.aggregated, order.by=project.times)

  # this will aggregate all values from varcol where value of timecol is equal
  project.aggregated <- aggregate(project[[varcol]] ~ project[[timecol]], project, aggregate.fn)
  project.aggregated <- setNames(project.aggregated, c(timecol, varcol))

  # sort the aggregated project data by timecol
  project.sorted <- project.aggregated[order(as.numeric(project.aggregated[[timecol]])), ]

  # grab the values for both axes (time values for x and var values for y)
  project.times <- as.numeric(project.sorted[[timecol]])
#   project.vars <- aggregate(project.aggregated, by=project.times, aggregate.fn)
  project.vars <- as.numeric(project.sorted[[varcol]])

  # perform discrete wavelet transformation on var values
  project.dwt <- dwt(as.numeric(project.vars), filter="haar")

  # plot the wavelet transformation
  if(makeplot){
    jpeg(paste(output.plots, "/", timecol, ".", varcol, ".", project.name, ".", "jpg", sep=""))
    try(
      plot(
        project.dwt,
        main=project.name,
        xlab=timecol,
        ylab=varcol
      )
    )
    dev.off()
  }

  for(dwtvar in dwtvars){
    dwt.dataframe <- paste("dst.dwt", dwtvar, timecol, sep=".")

    levelss <- length(attr(project.dwt, dwtvar))

    for(idx in 1:levelss){
      if(length(attr(project.dwt, dwtvar)[[idx]]) < 3){
        next
      }

      mat <- matrix(ncol=length(dst.dwt.columns), nrow=length(attr(project.dwt, dwtvar)[[idx]]))
      mat <- as.data.frame(mat)
      colnames(mat) <- dst.dwt.columns
      mat[["variable"]] <- varcol
      mat[["seq"]] <- 1:length(attr(project.dwt, dwtvar)[[idx]])
      mat[["pid"]] <- pid
      mat[["coefficient"]] <- attr(attr(project.dwt, "filter"), "wt.name")
      mat[["level"]] <- idx
      mat[["value"]] <- attr(project.dwt, dwtvar)[[idx]]
      mat[["revlevel"]] <- levelss + 1 - idx

      assign(dwt.dataframe, rbind(get(dwt.dataframe, envir=.GlobalEnv), mat), envir=.GlobalEnv)
    }

    filtername <- attr(attr(project.dwt, "filter"), "wt.name")
    filename <- paste(filtername, timecol, dwtvar, varcol, pid, "dwt.csv", sep="_")
    write.csv2(get(dwt.dataframe, envir=.GlobalEnv), file=paste(output, filename, sep="/"))
  }
}

handleProject <- function(project.data, pid, timecols, makeplot){
  for(timecol in timecols){
    timevarcols <- varcols[[timecol]]
    
    for(varcol in timevarcols){
      calculateColumnDWT(project.data, varcol, pid, timecol, makeplot)
      break
    }
    break
  }
}

# Discrete wavelet transformation per project variable
print(paste("Running DWT with", procs.sim, "simultaneous processes"))

project.index <- 0
project.count <- length(pids)

while(project.index < project.count){
  procs.index <- 0
  procs.list <- list()

  while(procs.index < procs.sim && project.index < project.count){
    pid <- pids[[project.index + 1]]

    project.data <- subset(dataset, dataset[[idcol]] == pid)
    project.name <- unique(project.data[[namecol]])
    print(paste("Processing", "project", project.name, "..."))

     proc <- parallel(
       try(
        handleProject(project.data, pid, timecols, makeplot)
       )
     )

    # Append to processes list
    procs.list[[length(procs.list) + 1]] <- proc

    procs.index <- procs.index + 1
    project.index <- project.index + 1
  }
  
  collect(procs.list)
}

# Aggregate project transformations per variable
for(timecol in timecols){
  timevarcols <- varcols[[timecol]]

  for(dwtvar in dwtvars){
    for(varcol in timevarcols){
      dst.dwt.W.Age.Days <- dst.dwt.V.Age.Days # why ??

      for(pid in pids){
        print(paste("Time", timecol, "variable", varcol, "dwt", dwtvar, "project", pid))

        dwt.df <- paste("dwt", dwtvar, timecol, sep=".")
        input.filename <- paste(output, paste("haar", timecol, dwtvar, varcol, pid, "dwt.csv", sep="_"), sep="/")

        if(!file.exists(input.filename)){
          warning(paste("No such file:", input.filename))
          next
        }

        try(
          assign("dst.dwt.W.Age.Days", rbind(get("dst.dwt.W.Age.Days", envir=.GlobalEnv), read.csv2(input.filename)), envir=.GlobalEnv)
        )
      }

      output.filename <- paste("factsForAnalysis", dwtvar, timecol, varcol, "csv", sep=".")
      write.csv2(dst.dwt.W.Age.Days, file=paste(output, output.filename, sep="/"))
    }
  }
}

print("Done")

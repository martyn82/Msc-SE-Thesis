# This script performs wavelet transformation on evolution data.
# 1. It takes the monthly facts for analysis data and outputs a csv file per project per variable.
# 2. It aggregates the variables for all projects.

library("hash")
library("multicore")
library("wavelets")

dataset <- read.csv2("data/factsForAnalysis.csv", na.strings="NA")
colnames(dataset)

output.folder <- "scripts/output/wavelet"
output.plots <- paste(output.folder, "plots", sep="/")
makeplot <- FALSE

if(!file.exists(output.folder)){
  dir.create(output.folder)
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

# Initialize data frames per timeseries column, for both coefficients.
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

  # this will aggregate all values from varcol where value of timecol is equal
  project.aggregated <- aggregate(project[[varcol]] ~ project[[timecol]], project, aggregate.fn)
  colnames(project.aggregated) <- c(timecol, varcol)

  # sort the aggregated project data by timecol
  project.sorted <- project.aggregated[order(as.numeric(project.aggregated[[timecol]])), ]

  # grab the values for both axes (time values for x and var values for y)
  project.times <- as.numeric(project.sorted[[timecol]])
  project.vars <- as.numeric(project.sorted[[varcol]])

  # perform discrete wavelet transformation on var values
  project.dwt <- dwt(project.vars, filter="haar")

  # plot the wavelet transformation
  if(makeplot){
    output.plots.filename <- paste(timecol, varcol, project.name, "jpg", sep=".")
    jpeg(paste(output.plots, output.plots.filename, sep="/"))
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
    dwtdf <- paste("dst.dwt", dwtvar, timecol, sep=".")
    levelss <- length(attr(project.dwt, dwtvar))

    for(idx in 1:levelss){
      if(length(attr(project.dwt, dwtvar)[[idx]]) <= 2){
        next
      }

      mat <- as.data.frame(
        matrix(ncol=length(dst.dwt.columns), nrow=length(attr(project.dwt, dwtvar)[[idx]]))
      )
      colnames(mat) <- dst.dwt.columns
      
      # variable name
      mat[["variable"]] <- varcol
      # sequence, 1 to the length of this level of the dwt variable (V or W)
      mat[["seq"]] <- 1:length(attr(project.dwt, dwtvar)[[idx]])
      # project ID
      mat[["pid"]] <- pid
      # wavelet coefficient filter (always: haar)
      mat[["coefficient"]] <- attr(attr(project.dwt, "filter"), "wt.name")
      # the current level
      mat[["level"]] <- idx
      # the value of the dwt variable of this level
      mat[["value"]] <- attr(project.dwt, dwtvar)[[idx]]
      # reverse level
      mat[["revlevel"]] <- levelss + 1 - idx

      assign(dwtdf, rbind(get(dwtdf, envir=.GlobalEnv), mat), envir=.GlobalEnv)
    }

    filtername <- attr(attr(project.dwt, "filter"), "wt.name")
    filename <- paste(filtername, timecol, dwtvar, varcol, pid, "dwt.csv", sep="_")
    
    df <- get(dwtdf, envir=.GlobalEnv)
    df <- subset(df, df[["variable"]] == varcol)
    df <- df[dst.dwt.columns]
    
    write.csv2(df, file=paste(output.folder, filename, sep="/"))
  }
}

handleProject <- function(project.data, pid, timecols, makeplot){
  for(timecol in timecols){
    timevarcols <- varcols[[timecol]]

    for(varcol in timevarcols){
      calculateColumnDWT(project.data, varcol, pid, timecol, makeplot)
    }
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

# Aggregate project transformations per type
for(timecol in timecols){
  timevarcols <- varcols[[timecol]]

  for(dwtvar in dwtvars){
    for(varcol in timevarcols){
      dst.dwt.vars <- as.data.frame(
        matrix(ncol=length(dst.dwt.columns), nrow=0)
      )
      colnames(dst.dwt.vars) <- dst.dwt.columns

      for(pid in pids){
        print(paste("Time", timecol, "variable", varcol, "dwt", dwtvar, "project", pid))

        dwtdf <- paste("dst.dwt", dwtvar, timecol, sep=".")
        filename <- paste("haar", timecol, dwtvar, varcol, pid, "dwt.csv", sep="_")
        input.filename <- paste(output.folder, filename, sep="/")

        if(!file.exists(input.filename)){
          warning(paste("No such file:", input.filename))
          next
        }

        df <- read.csv2(input.filename)
        df <- df[dst.dwt.columns]

        try(
          assign("dst.dwt.vars", rbind(get("dst.dwt.vars", envir=.GlobalEnv), df), envir=.GlobalEnv)
        )
      }

      output.filename <- paste("factsForAnalysis", dwtvar, timecol, varcol, "csv", sep=".")
      write.csv2(dst.dwt.vars, file=paste(output.folder, output.filename, sep="/"))
    }
  }
}

print("Done")

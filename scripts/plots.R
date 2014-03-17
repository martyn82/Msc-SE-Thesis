# This script will plot the projects' evolution data.
# It takes all projects in the dataset and plots the metrics to following dimensions:
#
# Time series                | Variable          | Description
# ---------------------------+-------------------+--------------------------------------------
# Date                       | LOC               | LOC per date shows project size
#                            | Commit LOC Churn  | LOC churn per date shows project activity
#                            | Active developers | Active devs per date shows project team size
# ---------------------------+-------------------+--------------------------------------------
# Active developers          | Commit LOC Churn  | LOC churn per team size shows productivity
# ---------------------------+-------------------+--------------------------------------------

library("hash")

output <- "scripts/output/plots"

if(!file.exists(output)){
  dir.create(output)
}

dataset <- read.csv2("data/factsForAnalysis.csv")

idcol <- "Project.Id"
namecol <- "Project.Name"
timecols <- c("Age.Days", "Active.Developers")

varcols <- hash(keys=timecols)
varcols[["Age.Days"]] <- c("LOC", "Commit.LOC.Churn", "Active.Developers")
varcols[["Active.Developers"]] <- c("Commit.LOC.Churn")

projects <- unique(dataset[[idcol]])

for(pid in projects) {

  for (timecol in timecols){
    timevarcols <- varcols[[timecol]]
    
    for(varcol in timevarcols){
      project <- subset(dataset, dataset[[idcol]]==pid, select=c(namecol, timecol, varcol))
      project.name <- as.character(unique(project[[namecol]]))

      # Using SUM as aggregate function for Commit.LOC.Churn per Active.Developers is a flawed measure for team performance
      aggregate.fn <- sum

      project.aggregated <- aggregate(project[[varcol]] ~ project[[timecol]], project, aggregate.fn)
      project.aggregated <- setNames(project.aggregated, c(timecol, varcol))

      if(timecol == "Date"){
        project.sorted <- project.aggregated[order(as.Date(project.aggregated[[timecol]])), ]
        project.times <- as.Date(project.sorted[[timecol]])
      }
      else {
        project.sorted <- project.aggregated[order(as.numeric(project.aggregated[[timecol]])), ]
        project.times <- as.numeric(project.sorted[[timecol]])
      }

      project.vars <- as.numeric(project.sorted[[varcol]])
      value.mean <- mean(project.vars)

      jpeg(paste(output, "/", timecol, ".", varcol, ".", project.name, ".", "jpg", sep=""))

      plot(
        project.times,
        project.vars,
        type="l",
        main=project.name,
        xlab=timecol,
        ylab=varcol
      )

      lines(project.times, rep(value.mean, times=length(project.times)))

      dev.off()
    }
  }
}

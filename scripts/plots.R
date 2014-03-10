library("hash")

data.frame <- read.csv2("tools/csv/converted.csv")

idcol <- "ProjectId"
namecol <- "ProjectName"
timecols <- c("Date", "Active.Developers")

varcols <- hash(keys=timecols)
varcols[["Date"]] <- c("LOC", "Commit.LOC.Churn", "Active.Developers")
varcols[["Active.Developers"]] <- c("Commit.LOC.Churn")

projects <- unique(data.frame[[idcol]])

for(pid in projects) {
  for (timecol in timecols){
    for(varcol in varcols[[timecol]]){
      project <- subset(data.frame, data.frame[[idcol]]==pid, select=c(namecol, timecol, varcol))
      project.sorted <- project[order(project[[timecol]], project[[varcol]]),]

      if(timecol == "Date"){
        project.times <- as.Date(project.sorted[[timecol]])
      }
      else {
        project.times <- as.numeric(project.sorted[[timecol]])
      }

      project.name <- unique(project.sorted[[namecol]])
      project.vars <- as.numeric(project.sorted[[varcol]])

      value.last <- project.vars[end(project.vars)][1]
      value.max <- max(project.vars)
      value.mean <- mean(project.vars)

      jpeg(paste("plots/", timecol, ".", varcol, ".", project.name, ".", "jpg", sep=""))

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

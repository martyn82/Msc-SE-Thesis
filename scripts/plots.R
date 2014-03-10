
data.frame <- read.csv2("tools/csv/converted.csv")
timecol <- "Date"
idcol <- "ProjectId"
namecol <- "ProjectName"
varcols <- c("LOC", "Commit.LOC.Churn", "Active.Developers")
projects <- unique(data.frame[[idcol]])

for(pid in projects) {
  for(varcol in varcols){
    project <- subset(data.frame, data.frame$ProjectId==pid, select=c(namecol, timecol, varcol))
    project.times <- as.Date(project[[timecol]])
    project.name <- unique(project[[namecol]])
    project.vars <- as.numeric(project[[varcol]])
  
    value.last <- project.vars[end(project.vars)][1]
    value.max <- max(project.vars)
    value.mean <- mean(project.vars)
  
    mark <- "O"
  
    if(value.last == value.max){
      mark <- "N"
    }
  
    jpeg(paste("plots/", timecol, ".", varcol, ".", project.name, ".", mark, ".jpg", sep=""))
  
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

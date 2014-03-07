
data.frame <- read.csv2("tools/csv/converted.csv")
timecol <- "Date"
varcol <- "LOC"
idcol <- "ProjectId"
namecol <- "ProjectName"
projects <- unique(data.frame[[idcol]])
ordertimefn <- as.numeric

cm <- 0

for(pid in projects) {
  project <- subset(data.frame, data.frame$ProjectId==pid, select=c(namecol, timecol, varcol))
  project.times <- as.Date(project[[timecol]])
  project.vars <- as.numeric(project[[varcol]])
  project.name <- unique(project[[namecol]])
  
  value.last <- project.vars[end(project.vars)][1]
  value.max <- max(project.vars)
  value.mean <- mean(project.vars)
  
  if(value.mean > value.last){
    cm <- cm + 1
    print(project.name)
  }
  
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

print(cm)
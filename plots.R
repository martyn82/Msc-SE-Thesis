
data.frame <- read.csv2("tools/csv/converted.csv")
timecol <- "Date"
varcol <- "LOC"
idcol <- "ProjectId"
namecol <- "ProjectName"
projects <- unique(data.frame[[idcol]])
ordertimefn <- as.numeric

for(pid in projects) {
  project <- subset(data.frame, data.frame$ProjectId==pid, select=c(namecol, timecol, varcol))
  project.times <- as.Date(project[[timecol]])
  project.vars <- as.numeric(project[[varcol]])
  project.name <- unique(project[[namecol]])
  
  jpeg(paste("plots/", timecol, ".", varcol, ".", project.name, ".jpg", sep=""))
  
  plot(
    project.times,
    project.vars,
    type="l",
    main=project.name,
    xlab=timecol,
    ylab=varcol
  )
  
  dev.off()
}
# Performs discrete wavelet transform on software project's evolution data.
# Input: 
# 	CommitsWithStatistics.csv - File with commit data for projects
# 	Columns in the file: 
# 		CommitId;Date;DeveloperId;ProjectId;Active Developers;Commit LOC Added;Commit LOC Churn;Commit LOC Modified;Commit LOC Removed;Cumulative Developers;Cumulative LOC Added;Cumulative LOC Churn;Cumulative LOC Modified;Cumulative LOC Removed;LOC;Relative Date Progress;Relative LOC Churn Progress;Relative Team Size;Files;Year;Month;Day
# Output:
# 	wavelet_cum

library("wavelets")
library("chron")
library("zoo")
library("multicore")

folder.root <- "."
folder.proc <- paste(folder.root, "output", "wavelet_cum", sep="/")
folder.plots <- paste(folder.root, "output", "plots", sep="/")

if(!(file.exists(folder.proc))) {
  dir.create(folder.proc)
}

if(!file.exists(folder.plots)){
  dir.create(folder.plots)
}

my.csv.data <- read.csv2 (file=paste("data/factsForAnalysis.csv", sep="/"),  na.strings = "NA")

colnames(my.csv.data)

#interesting_colnames <- c("Active.Developers", "Commit.LOC.Added", "Commit.LOC.Churn", "Commit.LOC.Modified", "Commit.LOC.Removed", "Cumulative.Developers", "Cumulative.LOC.Added", "Cumulative.LOC.Churn", "Cumulative.LOC.Modified", "Cumulative.LOC.Removed", "LOC", "Relative.Date.Progress", "Relative.LOC.Churn.Progress", "Relative.Team.Size", "Files", "Commits")
interesting_colnames <- c("LOC")
#fill_locf <- c("Cumulative.Developers", "Cumulative.LOC.Added", "Cumulative.LOC.Churn", "Cumulative.LOC.Modified", "Cumulative.LOC.Removed", "LOC", "Relative.Date.Progress", "Relative.LOC.Churn.Progress", "Relative.Team.Size", "Files")
fill_locf <- c("LOC")
#fill_zero <- c("Active.Developers", "Commit.LOC.Added", "Commit.LOC.Churn", "Commit.LOC.Modified", "Commit.LOC.Removed")
fill_zero <- c()
#aggregation_max <- c("Cumulative.Developers", "Cumulative.LOC.Added", "Cumulative.LOC.Churn", "Cumulative.LOC.Modified", "Cumulative.LOC.Removed", "Relative.Date.Progress", "Relative.LOC.Churn.Progress")
aggregation_max <- c()
#aggregation_avg <- c("Active.Developers", "Commit.LOC.Added", "Commit.LOC.Churn", "Commit.LOC.Modified", "Commit.LOC.Removed", "LOC", "Relative.Team.Size", "Files")
aggregation_avg <- c("LOC")
#?ts
pids <- unique(my.csv.data[["Project.Id"]])

# dwt values
my.data.dwt.colnames = c("seq", "variable", "pid", "coefficient", "level", "value", "revlevel")
my.data.dwt.W.Age.Months <- matrix(nrow=0,ncol=length(my.data.dwt.colnames))
my.data.dwt.W.Age.Months <- as.data.frame(my.data.dwt.W.Age.Months)
colnames(my.data.dwt.W.Age.Months) <- my.data.dwt.colnames
my.data.dwt.V.Age.Months <- matrix(nrow=0,ncol=length(my.data.dwt.colnames))
my.data.dwt.V.Age.Months <- as.data.frame(my.data.dwt.V.Age.Months)
colnames(my.data.dwt.V.Age.Months) <- my.data.dwt.colnames

# my.data.dwt.W.Cumulative.LOC.Churn <- matrix(nrow=0,ncol=length(my.data.dwt.colnames))
# my.data.dwt.W.Cumulative.LOC.Churn <- as.data.frame(my.data.dwt.W.Cumulative.LOC.Churn)
# colnames(my.data.dwt.W.Cumulative.LOC.Churn) <- my.data.dwt.colnames
# my.data.dwt.V.Cumulative.LOC.Churn <- matrix(nrow=0,ncol=length(my.data.dwt.colnames))
# my.data.dwt.V.Cumulative.LOC.Churn <- as.data.frame(my.data.dwt.V.Cumulative.LOC.Churn)
# colnames(my.data.dwt.V.Cumulative.LOC.Churn) <- my.data.dwt.colnames

#timecol <- "Date"
calculateColumnDWT <- function(project.data, current_col, pid, timecol) {
  timeorderbyfn <- as.numeric
  unitcoef <- 1000
  if(timecol %in% c("Age.Months")) {
    #timeorderbyfn <- as.Date
    unitcoef <- 1
  }
  aggrfn <- max
  if(current_col %in% aggregation_avg) {
    aggrfn <- function(x) { mean(na.omit(x)) }
  }
  aggrfn0 <- aggrfn
  cc <- current_col
  if(current_col %in% c("Commits")) {
    aggrfn <- sum
    aggrfn0 <- function (x) { length(na.omit(x)) }
    cc <- "Project.Id"
  }

  project.data.zoo <- zoo(project.data[[cc]], order.by=timeorderbyfn(project.data[[timecol]]))
#  plot(project.data.zoo)
  project.data.zoo.idx <- index(project.data.zoo)
  project.data.zoo.vals <- aggregate(project.data.zoo, project.data.zoo.idx, aggrfn0)
  project.data.zoo2 <- zoo(project.data.zoo.vals, order.by=unique(project.data.zoo.idx))
  
  project.data.time <- 1:10
  if(timecol %in% c("Date")) {
    project.data.time <- seq(start(project.data.zoo2), end(project.data.zoo2), by="days")
  } else { 
    project.data.time <- min(project.data[[timecol]]):max(project.data[[timecol]])
  }
  aggr.result <- c(1:10)
  if(current_col %in% fill_locf) {
    aggr.result <- na.locf(project.data.zoo2, xout=project.data.time)
  } else {
    aggr.result <- rep(0, length(project.data.time))
    aggr.result[index(project.data.zoo2) - project.data.time[1] + 1] <- project.data.zoo2
    aggr.result <- zoo(aggr.result, order.by=project.data.time)
    #aggr.result <- na.fill(project.data.zoo2, 0, xout=project.data.time)
  }
  
  project.data.weeks <- ceiling((as.numeric(project.data.time) / unitcoef))
  project.data.weeks.vals <- aggregate(aggr.result, by=project.data.weeks, aggrfn)

  project.data.weeks.zoo <- zoo(project.data.weeks.vals, order.by=unique(project.data.weeks))
#  plot(project.data.weeks.zoo)

  project.data.weeks.dwt <- dwt(as.numeric(project.data.weeks.vals), filter="haar")

  for(dwtcoef in c("V", "W")){
    plots.filename <- paste(paste(timecol, current_col, dwtcoef, pid, sep="_"), "jpg", sep=".")
    jpeg(paste(folder.plots, plots.filename, sep="/"))
    
    if(dwtcoef == "V"){
      try(
        plot(project.data.weeks.dwt, plot.W=FALSE)
      )
    }else{
      try(
        plot(project.data.weeks.dwt, plot.V=FALSE)
      )
    }
    
    dev.off()
  }

  #plot(project.data.weeks.dwt)

	#plot.dwt.multiple(project.data.weeks.dwt, levels=list(c(1),c(1,2,3)))
	#plot.dwt(project.data.weeks.dwt)
	#plot(project.data.weeks.dwt, prot.W=FALSE, plot.V=FALSE)
	#figure98.wt.filter("haar")
	#figure108.wt.filter("haar")
#	plot(as.numeric(attr(project.data.weeks.dwt,"series")), type="l")
#	for(i in c(1:length(attr(project.data.weeks.dwt,"V")))) {
#		lines(rep(attr(project.data.weeks.dwt,"V")[[i]], each=(2^i))/(2^i), col=(i+1))
#	}

	# pdw.dwt.levels <- length(attr(project.data.weeks.dwt,"V"))
	# layout(matrix(rev(1:(pdw.dwt.levels)), ncol=1), 1, (c(rep(2,pdw.dwt.levels-1),pdw.dwt.levels)))
	# layout.show(pdw.dwt.levels)
	# par(mar=c(3,3,0,3))
	# plot(as.numeric(attr(project.data.weeks.dwt,"series")), type="l")
	# par(mar=c(0,3,0,3))
	# for(i in c(1:(pdw.dwt.levels-1))) {
		# plot(as.numeric(attr(project.data.weeks.dwt,"V")[[i]]), type="l", lab=c(1,1,1), ylab=paste("V[", i, "]",sep=""), ann=FALSE)
		# points(as.numeric(attr(project.data.weeks.dwt,"W")[[i]]), ylim=c(min(attr(project.data.weeks.dwt,"W")[[i]]),max(attr(project.data.weeks.dwt,"W")[[i]])))
	# }
	
  for(dwtvar in c("V", "W")) {
    dwtdf <- paste("my.data.dwt", dwtvar, timecol, sep=".")

    levelss <- length(attr(project.data.weeks.dwt, dwtvar))
    for(idx in 1:levelss) {
      if(length(attr(project.data.weeks.dwt, dwtvar)[[idx]]) > 2)
      {
        tmpM <- matrix(ncol=length(my.data.dwt.colnames), nrow=length(attr(project.data.weeks.dwt, dwtvar)[[idx]]))
        tmpM <- as.data.frame(tmpM)
        colnames(tmpM) <- my.data.dwt.colnames
        tmpM[["variable"]] <- current_col
        tmpM[["seq"]] <- 1:length(attr(project.data.weeks.dwt, dwtvar)[[idx]])
        tmpM[["pid"]] <- pid
        tmpM[["coefficient"]] <- attr(attr(project.data.weeks.dwt,"filter"),"wt.name")
        tmpM[["level"]] <- idx
        tmpM[["value"]] <- attr(project.data.weeks.dwt, dwtvar)[[idx]]
        tmpM[["revlevel"]] <- levelss + 1 - idx

        assign(dwtdf, rbind(get(dwtdf), tmpM))
      }
    }
    write.csv2(get(dwtdf), file=paste(folder.proc, paste(attr(attr(project.data.weeks.dwt,"filter"),"wt.name"), timecol, dwtvar, current_col, pid, "dwt.csv", sep="_"), sep="/"))
  }

}


#pid <- 15
#current_col <- "LOC"
for(pid in pids)
{
  for(current_col in interesting_colnames) {
    print(paste("project", pid, "variable", current_col))
    project.data <- subset(my.csv.data, my.csv.data[["Project.Id"]] == pid)
#try(calculateColumnDWTByDate(project.data, current_col, pid))
#try(calculateColumnDWTByChurn(project.data, current_col, pid))
    #p1 <- parallel(try(calculateColumnDWT(project.data, current_col, pid, "Cumulative.LOC.Churn")))
    #p2 <- parallel(
      try(calculateColumnDWT(project.data, current_col, pid, "Age.Months"))
    #)
    #collect(list(p1, p2))
  }
}

for(timecol in c("Age.Months")){ #for(timecol in c("Age.Months", "Cumulative.LOC.Churn")) {
  for(dwtvar in c("V", "W")) {
    for(current_col in interesting_colnames) {
      assign("my.data.dwt.W.Age.Months", my.data.dwt.V.Age.Months)
      for(pid in pids) {
        print(paste("A.. project", pid, "variable", current_col, "coef", dwtvar))
        dwtdf <- paste("my.data.dwt", dwtvar, timecol, sep=".")
        dfn <- paste(folder.proc, paste("haar", timecol, dwtvar, current_col, pid, "dwt.csv", sep="_"), sep="/")
        if(file.exists(dfn)) {
          try(assign("my.data.dwt.W.Age.Months", rbind(get("my.data.dwt.W.Age.Months", envir = .GlobalEnv), read.csv2(dfn)), envir = .GlobalEnv))
        }
      }
      write.csv2 (my.data.dwt.W.Age.Months, file=paste(folder.proc, paste("factsForAnalysis.dwt", dwtvar, timecol, current_col, "csv", sep="."), sep="/"))
    }
  }
}


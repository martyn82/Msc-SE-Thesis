# identifies similar sequences in DWT data

folder.root <- "."
folder.proc <- paste(folder.root, "wavelet_cum", sep="/")

# pids <- c(15, 17,18,121,126,137,138,139,140,146,147,149,156,163,168,169,170,172,251,252,257,258,302,321,323,324)
alloweddeviation <- 0.005
minseq <- 3
minrevlevel <- ceiling(log(minseq)/log(2))
maxsearchseq <- 65
pids_complete <- c()
filter_name <- "haar"
#interesting_colnames <- c("Active.Developers", "Commit.LOC.Added", "Commit.LOC.Churn", "Commit.LOC.Modified", "Commit.LOC.Removed", "Cumulative.Developers", "Cumulative.LOC.Added", "Cumulative.LOC.Churn", "Cumulative.LOC.Modified", "Cumulative.LOC.Removed", "LOC", "Relative.Date.Progress", "Relative.LOC.Churn.Progress", "Relative.Team.Size", "Files", "Commits")
interesting_colnames <- c("LOC")

findSimilarSequences <- function(timecol, targetseq, pid, searchseq, searchrevlevel, pid0, current_column, dwtvar, revlevel) {
  if(length(searchseq) >= minseq & length(targetseq) >= minseq) {
    for(i in c(1:(length(searchseq) - minseq + 1))) {
 #     matches <- 0
			ss2 <- 1
      if(pid == pid0 & searchrevlevel == revlevel & i <= length(targetseq) - minseq)
				ss2 <- i + 1
      for(j in c(ss2:(length(targetseq) - minseq + 1))) {
				mlen <- ((if(length(searchseq) - i < length(targetseq) - j) { length(searchseq) - i } else {length(targetseq) - j}) + 1)
				
				a0 <- (searchseq[i] - searchseq[i+1]) / (targetseq[j] - targetseq[j+1])
				if(is.na(a0) | is.infinite(a0) | a0 <= 0.0)
					next
				a1 <- 1.0/a0
				b0 <- searchseq[i] - (targetseq[j] * a0)
				b1 <- targetseq[j] - (searchseq[i] * a1)
				divdifdev0 <- abs(((targetseq[j:(j+mlen-1)] * a0 + b0) / searchseq[i:(i+mlen-1)]) - 1.0)
				divdifdev1 <- abs(((searchseq[i:(i+mlen-1)] * a1 + b1) / targetseq[j:(j+mlen-1)]) - 1.0)
				
				for(k in c(3:length(divdifdev0))) {
					if(is.na(divdifdev0[k]) | is.infinite(divdifdev0[k]) | divdifdev0[k] > alloweddeviation | is.na(divdifdev1[k]) | is.infinite(divdifdev1[k]) | divdifdev1[k] > alloweddeviation | is.nan(divdifdev0[k]) | is.nan(divdifdev1[k]))
						break
					if(k > minseq) {
						writeLines(paste(timecol, current_column, pid0, searchrevlevel, i, pid, revlevel, j, k, format(max(c(divdifdev0[1:k],divdifdev1[1:k])), decimal.mark=","), paste(format(searchseq[i:(i+k-1)], decimal.mark=","), collapse="-"), format(a0, decimal.mark=","), format(b0, decimal.mark=","), sep = ";"), con=simfile)
						}
					}
      }
    }
  }
}

for(timecol in c("Age.Months")){ #, "Cumulative.LOC.Churn")) {
  for(dwtvar in c("V", "W")) {
  #for(dwtvar in c("V")) {
    for(current_col in interesting_colnames) {
			if(current_col == timecol)
				next
      simfile <- file(paste(folder.proc, paste("haar", timecol, dwtvar, current_col,"similar.csv", sep="_"), sep="/"), open="w")
      open(simfile)
      writeLines(paste("Time col", "Variable", "pid0", "pid0 revlevel", "pid0 startseq", "pid", "pid revlevel", "pid startseq", "length", "Max. deviation", "Seq", "a0", "b0", sep = ";"), con=simfile)

      print(paste(folder.proc, paste("factsForAnalysis.dwt", dwtvar, timecol, current_col, "csv", sep="."), sep="/"))
      my.data <- read.csv2 (paste(folder.proc, paste("factsForAnalysis.dwt", dwtvar, timecol, current_col, "csv", sep="."), sep="/"), header=TRUE)

      pids <- unique(my.data[["pid"]])
			#pids <- c(15, 17,18,121,126,137,138,139,140,146,147,149,163,168,169,170,251,252,257,324,339,340,344,350,351,352,354,355,356,357,358,359,360,361,362,367,368,369,370,371,373,375,376,378,379,380,381,382,383,384,385,386,387) # 258,323,302,321,353,363,365,372,374
			# No matches: 357; 370 web; 373 AviSynthLexer; 375 VDMHomePage
			#pids <- c(15, 17,18,121,126,137,138,139,140,146,147,149,163,168,169,170,251,252,257,324,339,340,344,367,368,369,371,385,378,379,381)

      if(length(pids) > 0) {
				for(i in c(1:length(pids))) {
					#search pid
					pid0 <- pids[i]
					my.data.pid0 <- my.data[my.data[["pid"]] == pid0,]
					pid0levels <- max(as.numeric(my.data.pid0[["revlevel"]]))
					print(paste(timecol, dwtvar, current_col, pid0, pid0levels))
					if(minrevlevel <= pid0levels) {
						for( i.l in c(minrevlevel:pid0levels)) {
							pid0.values <- as.numeric(my.data.pid0[my.data.pid0[["revlevel"]] == i.l, "value"])
							if(length(pid0.values) < maxsearchseq) {
								for(pid in pids[i:length(pids)]) {
									my.data.pid <- my.data[my.data[["pid"]] == pid,]
									pidlevels <- max(as.numeric(my.data.pid[["revlevel"]]))
									if(minrevlevel <= pidlevels) {
										for(j.l in c(minrevlevel:pidlevels)) {
											pid.values <- as.numeric(my.data.pid[my.data.pid[["revlevel"]] == j.l, "value"])
											if(length(pid.values) < maxsearchseq) {
												try(findSimilarSequences(timecol, pid.values, pid, pid0.values, i.l, pid0, current_col, dwtvar, j.l))
											}
										}
									}
								}
							}
						}
					}
				}
      }

      flush(simfile)
      close(simfile)
    }
  }
}


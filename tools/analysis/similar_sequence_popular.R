# Finds most popular reoccurring patterns in DWT data.

folder.root <- "."
folder.proc <- paste(folder.root, "output", "similar_sequence", sep="/")
file.sim.data <- paste(folder.root, "output", "sim.data.csv", sep="/")

projects.data <- read.csv2("data/factsForAnalysis.csv")
dead.projects.data <- read.csv2("output/deadProjectsValidated.csv")
dead.projects.data <- subset(dead.projects.data, dead.projects.data$confirmed.dead=="TRUE")

#allpids <- c( 15, 17, 18,121,126,137,138,139,140,146,147,149,163,168,169,170,257,324,339,340,344,350,351,352,354,355,356,357,358,359,360,361,362,367,368,369,370,371,373,375,376,378) # 251,252 # 258,323,302,321
#pids <- c( 15, 17, 18,121,126,137,138,139,140,146,147,149,163,168,169,170,257,324,339,340,344,367,368,369,371,385,378,379,381)
#deadpids <- c(15,17,18,339,340,344,350,351,352,354,355,356,357,358,359,360,361,362,367,368,369,370,371,373,375,376,378)#251,252)
#deadpids <- c(15,17,18,339,340,344,367,368,369,371,385,378,379,381)#251,252)

allpids <- unique(projects.data[["Project.Id"]])
deadpids <- unique(dead.projects.data[["pid"]])

# No matches: 357; 370 web; 373 AviSynthLexer; 375 VDMHomePage
# Not in OLAP: 350-363, 376
alivepids <- allpids[!(allpids %in% deadpids)]

#my.projects.data <- read.csv2 (file=paste(folder.root,"projects.csv", sep="/"),  na.strings = "NA")

minocc <- 3
filter_name <- "haar"
#interesting_colnames <- c("Active.Developers", "LOC.Churn", "LOC")
interesting_colnames <- c("LOC.Churn")

simfile <- file(paste(folder.root, paste("haar", "similar", "Age.Months", "LOC.Churn.csv", sep="_"), sep="/"), open="w")
open(simfile)
writeLines(paste("Time col", "DWT_var", "Variable", "pid0", "pid0 revlevel", "pid0 startseq", "length", "Max. deviation", "occurrances", "Project count", "Project list", "seq", "Project id-ss-rl", "HasDead", "HasAlive", "HasMixed", "Seq Id", sep = ";"), con=simfile)

tm1 <- proc.time() - proc.time()
tm2 <- tm1
tm3 <- tm1
tm3_5 <- tm1
tm4 <- tm1
tm5 <- tm1

sim.seqs.data <- as.data.frame(matrix(nrow=0,ncol=9,dimnames=list(NULL,c("time", "var", "coef","pid","pid.revlevel","pid.startseq","length","count", "issub"))))

#timecol <- "Date"
#dwtvat <- "V"
#current_col <- "Files"
#simdata_rel <- simdata[1:10,]

timecols <- c("Age.Months")

for(timecol in timecols) {
  for(dwtvar in c("V", "W")) {
#  for(dwtvar in c("V")) {
    for(current_col in interesting_colnames) {
		if(current_col == timecol)
			next;
			
      simdata <- read.csv2(paste(folder.proc, paste("haar", timecol, dwtvar, current_col, "similar.csv", sep="_"), sep="/"))
			
			#simdata[["pid0.endseq"]] <- simdata[["pid0.startseq"]] + simdate[["length"]]
			#simdata[["pid0.absstart"]] <- 2^(my.project.data[as.character(simdata[["pid0"]]) ,paste("Levels", simdata[["Time.col"]], sep=".")] - simdata[["pid0.revlevel"]])simdata[["pid0.startseq"]] + simdate[["length"]]
			
			
      print(paste(folder.proc, paste("haar", timecol, dwtvar, current_col, "similar.csv", sep="_"), sep="/"))
			
      while(dim(simdata)[1] > minocc) {
				pid <- simdata[1, "pid0"]
				revlevel <- simdata[1, "pid0.revlevel"]
				startseq <- simdata[1, "pid0.startseq"]
				length0 <- simdata[1, "length"]
	      lstm <- proc.time()
				
	      simdataidx_tf <- (simdata[["Variable"]] == current_col) & (simdata[["Time.col"]] == timecol) & (simdata[["pid0"]] == pid) & (simdata[["pid0.revlevel"]] == revlevel) & (simdata[["pid0.startseq"]] == startseq) & (simdata[["length"]] == length0)
				occs <- sum(as.numeric(simdataidx_tf))
	      lstm1 <- proc.time()
				lstm2 <- proc.time()
				lstm3 <- proc.time()
				lstm4 <- proc.time()
					
				if(occs > minocc) {
					lstm1 <- proc.time()
					simdata_rel <- simdata[simdataidx_tf, ]
					print(paste("length(simdata_rel)", dim(simdata_rel)[1]))
					lstm2 <- proc.time()
					
					simdata_rel_pids <- unique(c(simdata_rel[["pid0"]],simdata_rel[["pid"]]))
					hasDead <- max(simdata_rel_pids %in% deadpids)
					hasAlive <- max(simdata_rel_pids %in% alivepids)
					hasMixed <- hasDead & hasAlive
					seq_id <- dim(sim.seqs.data)[1] + 1
					
					sim.seqs.data[seq_id,] <- c(timecol, current_col, dwtvar, pid, revlevel, startseq, length0, occs, 0)
					
					if(seq_id > 1) {
						pid01 <- pid
						if((sim.seqs.data[(seq_id - 1), "pid"] == pid01) & (sim.seqs.data[(seq_id - 1), "pid.revlevel"] == revlevel) & (sim.seqs.data[(seq_id - 1), "pid.startseq"] == (startseq - 1)) & (sim.seqs.data[(seq_id - 1), "length"] > length0)) {
							if(sim.seqs.data[seq_id - 1, "count"] == occs) {
								sim.seqs.data[seq_id, "issub"] <- 1
							} else {
								sim.seqs.data[seq_id, "issub"] <- 0.5
							}
						}
						sim.seqs.data[((sim.seqs.data[["time"]] == timecol) & (sim.seqs.data[["coef"]] == dwtvar) & (sim.seqs.data[["var"]] == current_col) & (sim.seqs.data[["pid"]] == pid01) & (sim.seqs.data[["pid.revlevel"]] == revlevel) & (sim.seqs.data[["pid.startseq"]] == startseq) & (sim.seqs.data[["count"]] != occs) & (sim.seqs.data[["length"]] < length0)), "issub"] <- 0.5
						sim.seqs.data[((sim.seqs.data[["time"]] == timecol) & (sim.seqs.data[["coef"]] == dwtvar) & (sim.seqs.data[["var"]] == current_col) & (sim.seqs.data[["pid"]] == pid01) & (sim.seqs.data[["pid.revlevel"]] == revlevel) & (sim.seqs.data[["pid.startseq"]] == startseq) & (sim.seqs.data[["count"]] == occs) & (sim.seqs.data[["length"]] < length0)), "issub"] <- 1
					
						for(sri in c(1:dim(simdata_rel)[1])) {
							sim.seqs.data[((sim.seqs.data[["time"]] == timecol) & (sim.seqs.data[["coef"]] == dwtvar) & (sim.seqs.data[["var"]] == current_col) & (sim.seqs.data[["pid"]] == simdata_rel[sri,"pid"]) & (sim.seqs.data[["pid.revlevel"]] == simdata_rel[sri,"pid.revlevel"]) & (sim.seqs.data[["pid.startseq"]] < simdata_rel[sri,"pid.startseq"]) & (sim.seqs.data[["count"]] != occs) & ((as.numeric(sim.seqs.data[["pid.startseq"]]) + as.numeric(sim.seqs.data[["length"]])) >= (startseq + length0))), "issub"] <- 0.5
							sim.seqs.data[((sim.seqs.data[["time"]] == timecol) & (sim.seqs.data[["coef"]] == dwtvar) & (sim.seqs.data[["var"]] == current_col) & (sim.seqs.data[["pid"]] == simdata_rel[sri,"pid"]) & (sim.seqs.data[["pid.revlevel"]] == simdata_rel[sri,"pid.revlevel"]) & (sim.seqs.data[["pid.startseq"]] < simdata_rel[sri,"pid.startseq"]) & (sim.seqs.data[["count"]] == occs) & ((as.numeric(sim.seqs.data[["pid.startseq"]]) + as.numeric(sim.seqs.data[["length"]])) >= startseq + length0)), "issub"] <- 1
						
							sim.seqs.data[((sim.seqs.data[["time"]] == timecol) & (sim.seqs.data[["coef"]] == dwtvar) & (sim.seqs.data[["var"]] == current_col) & (sim.seqs.data[["pid"]] == simdata_rel[sri,"pid"]) & (sim.seqs.data[["pid.revlevel"]] == simdata_rel[sri,"pid.revlevel"]) & (sim.seqs.data[["pid.startseq"]] == simdata_rel[sri,"pid.startseq"]) & (sim.seqs.data[["count"]] != occs) & (sim.seqs.data[["length"]] < length0)), "issub"] <- 0.5
							sim.seqs.data[((sim.seqs.data[["time"]] == timecol) & (sim.seqs.data[["coef"]] == dwtvar) & (sim.seqs.data[["var"]] == current_col) & (sim.seqs.data[["pid"]] == simdata_rel[sri,"pid"]) & (sim.seqs.data[["pid.revlevel"]] == simdata_rel[sri,"pid.revlevel"]) & (sim.seqs.data[["pid.startseq"]] == simdata_rel[sri,"pid.startseq"]) & (sim.seqs.data[["count"]] == occs) & (sim.seqs.data[["length"]] < length0)), "issub"] <- 1
							sim.seqs.data[((sim.seqs.data[["time"]] == timecol) & (sim.seqs.data[["coef"]] == dwtvar) & (sim.seqs.data[["var"]] == current_col) & (sim.seqs.data[["pid"]] == simdata_rel[sri,"pid"]) & (sim.seqs.data[["pid.revlevel"]] == simdata_rel[sri,"pid.revlevel"]) & (sim.seqs.data[["pid.startseq"]] == simdata_rel[sri,"pid.startseq"]) & (sim.seqs.data[["count"]] == occs) & (sim.seqs.data[["length"]] == length0)), "issub"] <- 0.1
						}	
					}
					
	      
					writeLines(paste(
						timecol, 
						dwtvar, 
						current_col, 
						pid, 
						revlevel, 
						startseq, 
						length0, 
						format(max(simdata_rel[["Max..deviation"]]), decimal.mark=","), 
						occs,
						length(simdata_rel_pids),
						paste(simdata_rel_pids, collapse="|"), 
						simdata_rel[["Seq"]][1],
						paste(unique(apply((simdata_rel[,c("pid", "pid.revlevel", "pid.startseq")]), 1, paste, collapse="-")), collapse="|"),
						hasDead,
						hasAlive,
						hasMixed,
						seq_id,
						sep = ";"),
						con=simfile)
					print(paste("simdata size:", dim(simdata)[1]))
	      }
	      simdata <- simdata[(!simdataidx_tf),]
	      lstm4 <- proc.time()
	      tm1 <- tm1 + lstm1-lstm
	      tm2 <- tm2 + lstm2-lstm1
	      tm3 <- tm3 #+ lstm3-lstm2
	      tm3_5 <- tm3_5 #+ lstm3_5-lstm3
	      tm4 <- tm4 + lstm4- lstm2 #lstm3_5
      }
  	      print("match 1 time")
	      print(tm1)
	      print("subset 1 time")
	      print(tm2)
	      print("match+subset 2 time")
	      print(tm3)
	      print("match 3 time")
	      print(tm3_5)
	      print("subset 3 time")
	      print(tm4)
	      lstm4 <- proc.time()
	      gc()
	      lstm5 <- proc.time()
	      tm5 <- tm5 + lstm5 - lstm4
	      print("gc time")
	      print(tm5)
    }
  }
}

flush(simfile)
close(simfile)

write.csv2(sim.seqs.data, file=file.sim.data)

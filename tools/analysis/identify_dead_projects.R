# This script will identify dead projects based on set criteria.

dead.eval.months <- 12
dead.threshold.LOC <- 0
dead.threshold.LOC.Churn <- 0
dead.threshold.Active.Developers <- 0

input.file <- paste("data", "factsForAnalysis.csv", sep="/")
input.data <- read.csv2(input.file)

output.file <- paste("output", "deadProjects.csv", sep="/")
output.cols <- c("pid", "end.date", "by.loc", "by.churn", "by.devs")

pids <- unique(input.data$Project.Id)
dead.pids <- as.data.frame(
  matrix(
    ncol=length(output.cols),
    nrow=0
  )
)
colnames(dead.pids) <- output.cols
dead.pids.index <- 0

for(pid in pids){
  project.data <- subset(input.data, input.data$Project.Id==pid)
  project.data.ordered <- project.data[with(project.data, order(-as.numeric(project.data$Age.Months))), ]
  project.data.last_months <- project.data.ordered[1:dead.eval.months, ]

  project.data.LOC.Churn <- sum(project.data.last_months$LOC.Churn) # If dead, the sum of churn will be low
  project.data.Active.Developers <- max(project.data.last_months$Active.Developers) # If dead, the max of active developers will be low
  project.data.LOC <- project.data.last_months[1, "LOC"] # If dead, the LOC diff will be low

  project.is_dead.by.LOC <- all(project.data.last_months$LOC == project.data.LOC) #(project.data.LOC <= dead.threshold.LOC)
  project.is_dead.by.LOC.Churn <- (project.data.LOC.Churn <= dead.threshold.LOC.Churn)
  project.is_dead.by.Active.Developers <- (project.data.Active.Developers <= dead.threshold.Active.Developers)
  
  if(project.is_dead.by.LOC | project.is_dead.by.LOC.Churn | project.is_dead.by.Active.Developers){
    dead.pids.index <- dead.pids.index + 1
    dead.pids[dead.pids.index, ] <- c(
      pid,
      as.character(max(as.Date(project.data.last_months$Date))),
      format(project.is_dead.by.LOC),
      format(project.is_dead.by.LOC.Churn),
      format(project.is_dead.by.Active.Developers)
    )
  }
}

print(paste("Found", dead.pids.index, "dead projects"))
write.csv2(dead.pids, file=output.file)

# This script takes data/projectsWithCoverage.csv and outputs a file containing project IDs and names that
# have a coverage from a given minimum up to 100%.

coverage.min <- 1 # percentage divided by 100

input.file <- "data/projectsWithCoverage.csv"
output.file <- "data/projectsWithCoverageForSample.csv"

output.cols <- c("project_id", "project_name", "coverage")
output.df <- as.data.frame(
  matrix(ncol=length(output.cols), nrow=0)
)
colnames(output.df) <- output.cols

print("Reading input file...")
input.df <- read.csv2(input.file)

pids <- unique(input.df[["project_id"]])
c <- 1
total <- length(pids)

for(pid in pids){
  print(
    paste("Processing project ID", pid, "(", c, "/", total, ")...")
  )

  project.data <- subset(input.df, input.df[["project_id"]]==pid)
  project.data.coverage <- project.data[["coverage"]]

  if(project.data.coverage < coverage.min){
    next
  }
  
  row <- data.frame(
    project_id=pid,
    project_name=project.data[["project_name"]],
    coverage=project.data.coverage
  )
  
  output.df <- rbind(output.df, row)
  
  c <- c + 1
}

print("Writing to file...")
write.csv2(output.df, file=output.file)

print("Done.")

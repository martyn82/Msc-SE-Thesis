# This script takes data/projectsWithCoverage.csv and outputs a file containing project IDs and names that
# have a coverage from a given minimum up to 100%.

coverage.min <- 1 # percentage divided by 100
points.min <- 12  # minimum amount of data points

input.file <- "data/projectsWithCoverage.csv"
output.file <- "data/projectsWithCoverageForSample.csv"

output.cols <- c("project_id", "project_name", "coverage", "points")
output.df <- as.data.frame(
  matrix(ncol=length(output.cols), nrow=0)
)
colnames(output.df) <- output.cols

print("Reading input file...")
input.df <- read.csv2(input.file)

pids <- unique(input.df[["project_id"]])
total <- length(pids)
stats.selected <- 0
stats.skipped.coverage <- 0
stats.skipped.size <- 0
c <- 0

for(pid in pids){
  c <- c + 1

  print(
    paste("Processing project ID", pid, "(", c, "/", total, ")")
  )
  
  project.data <- subset(input.df, input.df[["project_id"]]==pid)
  project.data.coverage <- project.data[["coverage"]]
  project.data.size <- project.data[["sample_size"]]

  if(project.data.coverage < coverage.min){
    stats.skipped.coverage <- stats.skipped.coverage + 1
    next
  }
  
  if(project.data.size < points.min){
    stats.skipped.size <- stats.skipped.size + 1
    next
  }

  row <- data.frame(
    project_id=pid,
    project_name=project.data[["project_name"]],
    coverage=project.data.coverage,
    points=project.data.size
  )
  
  output.df <- rbind(output.df, row)
  stats.selected <- stats.selected + 1
}

print("Writing to file...")
write.csv2(output.df, file=output.file)

print(paste("Projects selected:", stats.selected))
print(paste("Projects skipped:", stats.skipped.coverage, "(low coverage)", stats.skipped.size, "(small)"))
print(paste("Total projects:", total))
print("Done.")

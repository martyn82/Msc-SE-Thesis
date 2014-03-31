# This script validates the projects data by calculating the relative evolution coverage.
# It takes monthlyFactsAfterCleaningWithMetaDataAndId as input.

input.file <- "data/monthlyFactsAfterCleaningWithMetaDataAndId.csv"
output.file <- "data/projectsWithCoverage.csv"

print(
  paste("Reading input:", input.file)
)
input.df <- read.csv2(input.file, sep=",")

pids <- unique(input.df[["project_id"]])

output.cols <- c("project_id", "project_name", "data_size", "sample_size", "coverage")
output.df <- as.data.frame(
  matrix(ncol=length(output.cols), nrow=0)
)
colnames(output.df) <- output.cols

total <- length(pids)

print(
  paste("Calculates evolution coverage for", total, "projects")
)

c <- 1

for(pid in pids){
  print(
    paste("Calculating coverage for project", pid, "(", c, "/", total, ")...")
  )

  project.data <- subset(input.df, input.df[["project_id"]]==pid)
  project.data.months <- project.data[["age_in_months"]]
  
  # months.seq is the time span of the project data
  project.data.months.seq <- min(project.data.months):max(project.data.months)
  # months.size is the number of months
  project.data.months.size <- length(project.data.months.seq)
  
  # project.data.size is the number of data points (months) available in the data set
  project.data.size <- length(project.data[["project_id"]])
  
  # the coverage is the ratio of the number of data points divided by the time span in months
  project.data.coverage <- project.data.size / project.data.months.size

  row <- data.frame(
    project_id=pid,
    project_name=unique(project.data[["project_name_fact"]]),
    data_size=project.data.months.size,
    sample_size=project.data.size,
    coverage=project.data.coverage
  )

  output.df <- rbind(output.df, row)

  c <- c + 1
}

print("Writing to output file...")
write.csv2(output.df, file=output.file)

print("Done.")

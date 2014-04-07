# This scripts takes as input data/monthlyFactsAfterCleaningWithMetaDataAndId.csv,
# and as source tools/csv/sample.csv. It will generate as output data/factsForAnalysis.csv containing
# aggregated data from the input filtered with the source sample, therefore, keeping only the projects
# that exist in sample.csv.

input.file <- "data/monthlyFactsAfterCleaningWithMetaDataAndId.csv"
source.file <- "tools/csv/sample.csv"

output.file <- "data/factsForAnalysis.csv"

print("Reading input data...")

input.df <- read.csv2(input.file, sep=",")
source.df <- read.csv2(source.file, sep=",")

output.cols <- c("Project.Id", "Project.Name", "Date", "Age.Months", "LOC", "Active.Developers", "LOC.Churn")
output.df <- as.data.frame(
  matrix(ncol=length(output.cols), nrow=0),
  stringsAsFactors=FALSE
)
colnames(output.df) <- output.cols

pids <- unique(source.df[["new.projects.id"]])
total <- length(pids)
p <- 1
c <- 1

for(pid in pids){
  project.data <- subset(input.df, input.df[["project_id"]]==pid)
  source.rowcount <- nrow(project.data)
  
  if(source.rowcount == 0){
    warning(
      paste("No data for project ID", pid)
    )
  }

  for(i in 1:source.rowcount){
    row <- project.data[i, ]
    
    project.id <- row$project_id
    project.name <- as.character(row$project_name_fact)
    
    year <- row$year_fact
    month <- row$month_fact
    day <- "01" # default day value because it is missing in the monthly data from Ohloh
    
    fact.date <- paste(year, month, day, sep="-")
    fact.age <- as.numeric(row$age_in_months)
    fact.contributors <- row$contributors_fact
    
    fact.loc <- sum(
      as.numeric(c(
        row$loc_fact,
        row$comments_fact,
        row$blanks_fact
      ))
    )
    
    fact.churn.loc <- sum(
      as.numeric(c(
        row$loc_added_fact,
        row$comments_added_fact,
        row$blanks_added_fact,
        row$loc_deleted_fact,
        row$comments_deleted_fact,
        row$blanks_deleted_fact
      ))
    )
    
    output.df[c, ] <- c(
      project.id,
      project.name,
      fact.date,
      fact.age,
      fact.loc,
      fact.contributors,
      fact.churn.loc
    )
    
    print(paste("Processed row", i, "/", source.rowcount))
    
    c <- c + 1
  }
  
  print(paste("Processed project ID", pid, "(", p, "/", total, ")"))
  p <- p + 1
}

print("Writing output data...")
write.csv2(output.df, file=output.file, row.names=FALSE)

print("Done.")

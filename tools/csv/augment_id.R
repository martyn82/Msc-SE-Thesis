# This script will take monthlyFactsAfterCleaningWithMetaData.csv as input,
#   and ValidatedProjectNamesListWithIds.csv as data source.
# and augments the data frame with a column "project_id" and fill it with the ID of the project.

input.file <- "data/monthlyFactsAfterCleaningWithMetaData.csv"
source.file <- "data/ValidatedProjectNamesListWithIds.csv"
output.file <- "data/monthlyFactsAfterCleaningWithMetaDataAndId.csv"

print("Reading data set...")

input.df <- read.csv2(input.file, sep=",")
source.df <- read.csv2(source.file)

output.cols <- colnames(input.df)

output.df <- as.data.frame(
  matrix(ncol=length(output.cols), nrow=0)
)

source.namecol <- "project_name"
input.namecol <- "project_name_fact"

project.names <- unique(input.df[[input.namecol]])

total <- length(project.names)
c <- 1

for(project.name in project.names){
  print(
    paste("Augmenting rows for project", project.name, "(", c, "/", total, ")...")
  )
  
  source.project <- subset(source.df, source.df[[source.namecol]]==project.name)
  pid <- as.numeric(source.project[["project_id"]])
  input.project <- subset(input.df, input.df[[input.namecol]]==project.name)

  row <- data.frame(
    project_id=pid,
    project_name_fact=input.project[["project_name_fact"]],
    main_language_fact=input.project[["main_language_fact"]],
    year_fact=input.project[["year_fact"]],
    month_fact=input.project[["month_fact"]],
    loc_added_fact=input.project[["loc_added_fact"]],
    loc_deleted_fact=input.project[["loc_deleted_fact"]],
    comments_added_fact=input.project[["comments_added_fact"]],
    comments_deleted_fact=input.project[["comments_deleted_fact"]],
    blanks_added_fact=input.project[["blanks_added_fact"]],
    blanks_deleted_fact=input.project[["blanks_deleted_fact"]],
    commits_fact=input.project[["commits_fact"]],
    contributors_fact=input.project[["contributors_fact"]],
    loc_fact=input.project[["loc_fact"]],
    comments_fact=input.project[["comments_fact"]],
    blanks_fact=input.project[["blanks_fact"]],
    comment_ratio_fact=input.project[["comment_ratio_fact"]],
    cumulative_commits_fact=input.project[["cumulative_commits_fact"]],
    abs_loc_growth=input.project[["abs_loc_growth"]],
    ind_loc_growth=input.project[["ind_loc_growth"]],
    age_in_months=input.project[["age_in_months"]],
    age_in_years=input.project[["age_in_years"]]
  )

  output.df <- rbind(output.df, row)

  c <- c + 1
}

print("Writing results to file...")
write.csv2(output.df, file=output.file)

print("Done.")

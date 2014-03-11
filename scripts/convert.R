# This script takes the output of OhlohAnalytics, enriched with project ID, and
# shrinks the data to the minimum required for the analysis scripts.

# the complete data set
dataset <- read.csv2("data/monthlyFactsAfterCleaningWithMetaDataAndId.csv", header=TRUE, sep=",")

# source columns
src.cols <- c(
  "project_id",                      # 'ProjectId'
  "project_name_fact",               # 'ProjectName'
  "year_fact",                       # year and month together form 'Date'
    "month_fact",
  "loc_fact",                        # loc, comments, and blanks aggregated to 'LOC'
    "comments_fact",
    "blanks_fact",
  "loc_added_fact",                  # all added and deleted facts aggregated to 'Commit.LOC.Churn'
    "loc_deleted_fact",
    "comments_added_fact",
    "comments_deleted_fact",
    "blanks_added_fact",
    "blanks_deleted_fact",
  "contributors_fact",              # 'Active.Developers'
  "age_in_months"                   # to calculate 'Age.Days'
)

# destination columns
dst.cols <- c(
  "Project.Id",
  "Project.Name",
  "Date",
  "Age.Days",
  "LOC",
  "Active.Developers",
  "Commit.LOC.Churn"
)

# Avg. 30.4 days per month, divided by 7 multiplied by 5 working days per week ~22.
monthdays <- 22

# source data selection
src.data <- subset(dataset, select=src.cols)
src.rowcount <- nrow(src.data)

# make destination data frame
dst.data <- data.frame(
  matrix(
    ncol=length(dst.cols),
    nrow=src.rowcount,
    dimnames=list(NULL, dst.cols)
  ),
  stringsAsFactors=FALSE
)

if(src.rowcount == 0){
  stop("No rows found in source data set.")
}

for(i in 1:src.rowcount){
  row <- src.data[i, ]

  project.id <- row$project_id
  project.name <- as.character(row$project_name_fact)

  year <- row$year_fact
  month <- row$month_fact
  day <- "01" # default day value because it is missing in the monthly data from Ohloh

  fact.date <- paste(year, month, day, sep="-")
  fact.age <- as.numeric(row$age_in_months) * monthdays
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

  dst.data[i, ] <- c(
    project.id,
    project.name,
    fact.date,
    fact.age,
    fact.loc,
    fact.contributors,
    fact.churn.loc
  )

  print(paste("Processed row", i, "/", src.rowcount))
}

write.csv2(dst.data, file="data/factsForAnalysis.csv", row.names=FALSE)

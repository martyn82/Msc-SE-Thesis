# This script filters the masterdata.txt for the sample selection based on projects existing in data/projectsWithCoverageForSample.csv

input.file <- "tools/csv/masterdata.txt"
source.file <- "data/projectsWithCoverageForSample.csv"

output.file <- "tools/csv/masterdata_filtered.csv"

input.df <- read.csv2(input.file, sep="\t")
source.df <- read.csv2(source.file)

output.df <- input.df[!input.df$id %in% source.df$project_id, ]

write.table(output.df, file=output.file, sep="\t", row.names=FALSE)
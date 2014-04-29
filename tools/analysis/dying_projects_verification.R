# This is a throw-away script that extracts projects identified as dying that have a relative decrease of -100% in
# commits and contributors.

projects.dead.file <- paste("output", "deadProjectsValidated.csv", sep="/")
projects.dead <- read.csv2(projects.dead.file)

projects.dying.file <- paste("output", "dyingProjectsValidated.csv", sep="/")
projects.dying <- read.csv2(projects.dying.file)

projects.dead.confirmed <- subset(projects.dead, projects.dead$confirmed.dead=="TRUE")

projects.dead.pids <- unique(as.numeric(projects.dead.confirmed$pid))
projects.dying.pids <- unique(as.numeric(projects.dying$pid))

projects.missing <- c()
projects.sub <- as.data.frame(
  matrix(
    ncol=dim(projects.dying)[2],
    nrow=0
  )
)

for(pid in projects.dying.pids){
  project.id <- pid
  project.rows <- subset(projects.dying, projects.dying$pid==project.id)

  is.missing <- (
    is.na(project.rows$year.contributors)
    | is.na(project.rows$year.commits)
    | is.na(project.rows$month.contributors)
    | is.na(project.rows$month.commits)
    | is.na(project.rows$year.contributors.change)
    | is.na(project.rows$year.commits.change)
  )
  
  if(is.missing){
    projects.missing <- rbind(projects.missing, pid)
    next
  }

  project.row <- subset(
    project.rows,
    project.rows$max.revlevel==max(project.rows$max.revlevel)
    & project.rows$dead.count==max(project.rows$dead.count)
  )

  projects.sub <- rbind(projects.sub, project.row)
}

rm(project.row)
rm(project.rows)
rm(is.missing)
rm(pid)
rm(project.id)

projects.sub <- projects.sub[order(projects.sub$pid), ]
projects.sub$X <- NULL

if(length(projects.missing)>0){
  warning(paste("Missing", length(projects.missing), "project's data..."))
}

projects.dying.verified <- subset(
  projects.sub,
  projects.sub$year.contributors.change == -1
  & projects.sub$year.commits.change == -1
)

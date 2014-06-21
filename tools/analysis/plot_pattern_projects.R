#pattern.ids <- c(15661)
pattern.ids <- c(6)
max.plots <- 3
xlab <- "time (index)"
ylab <- "LOC (coefficient)"

sequences.similar <- read.csv2("output/haar_similar_Age.Months_LOC.csv")

if (!exists("sequences.occurrences")) {
  sequences.occurrences <- read.csv2("output/similar_sequence/haar_Age.Months_V_LOC_similar.csv")
}

for (id in pattern.ids) {
  pattern.sequence <- sequences.similar[sequences.similar$Seq.Id == id, ]
  pattern.sequence <- as.numeric(sub(",", ".", unlist(strsplit(as.character(pattern.sequence$seq), split="-", fixed=T)), fixed=T))

  plot(
    x=0:(length(pattern.sequence) - 1),
    y=pattern.sequence,
    type="l",
    xlab=xlab,
    ylab=ylab,
    main=paste("pattern #", id, "in project #", as.numeric(pattern.metadata$pid))
  )
  
  pattern.similar <- sequences.similar[sequences.similar$Seq.Id == id, ]
  pattern.occurrences <- unlist(strsplit(as.character(pattern.similar$Project.id.ss.rl), split="|", fixed=T))
  pids <- list()

  for (i in 1:length(pattern.occurrences)) {
    occurrence <- as.numeric(unlist(strsplit(as.character(pattern.occurrences[i]), split="-", fixed=T)))
    occurrence.pid <- occurrence[1]
    occurrence.revlevel <- occurrence[2]
    occurrence.startseq <- occurrence[3]

    if (occurrence.pid %in% pids) {
      next
    }

    occurrence.sequence <- sequences.occurrences[sequences.occurrences$pid0 == occurrence.pid
                                             & sequences.occurrences$pid0.revlevel == occurrence.revlevel
                                             & sequences.occurrences$pid0.startseq == occurrence.startseq
                                             & sequences.occurrences$length == pattern.similar$length
                                             & sequences.occurrences$pid0 != sequences.occurrences$pid, ]

    if (nrow(occurrence.sequence) == 0) {
      next
    }

    occurrence.sequence <- occurrence.sequence[1, ]
    occurrence.sequence <- as.numeric(sub(",", ".", unlist(strsplit(as.character(occurrence.sequence$Seq), split="-", fixed=T)), fixed=T))

    plot(
      x=0:(length(occurrence.sequence) - 1),
      y=occurrence.sequence,
      type="l",
      xlab=xlab,
      ylab=ylab,
      main=paste("occurrence in project #", occurrence.pid)
    )

    pids[length(pids) + 1] <- occurrence.pid

    if (length(pids) == max.plots) {
      break
    }
  }

  rm(i, occurrence, pattern.occurrences, occurrence.pid, occurrence.revlevel, occurrence.startseq, pids)
}
#rm(pattern.metadata, pattern.similar, pattern.occurrences, pattern.sequence)
rm(pattern.ids, max.plots, id)
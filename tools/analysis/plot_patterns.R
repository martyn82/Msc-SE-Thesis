library(plyr)
library(zoo)

#common.patterns <- sequences.metadata[sequences.metadata$seq.id %in% ..., ]

property <- "max.loc.diff"
stats <- count(common.patterns[[property]])
stats <- stats[order(-stats$freq), ]

count <- 4

for(n in 1:count){
  stat <- stats[n, ]
  pats <- common.patterns[common.patterns[[property]] == stat$x, ]
  title <- paste("pattern A", n, sep="") # " (", stat$freq, ")", sep="")

  pats.lines <- strsplit(as.character(pats$loc.seq), split="|", fixed=T)
  plot(
    x=index(unlist(pats.lines[1])),
    y=unlist(pats.lines[1]),
    type="l",
    xlab="time (index)",
    ylab="LOC",
    main=title
  )

  #for(i in 2:length(pats.lines)){
  #  lines(x=index(unlist(pats.lines[i])), y=unlist(pats.lines[i]))
  #}
}

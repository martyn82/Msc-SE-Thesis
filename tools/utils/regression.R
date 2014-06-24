for (i in 1:nrow(tmp)) {
  tmp.pat <- tmp[i,]
  sid <- as.numeric(tmp.pat$seq.id)
  tmp.seq <- as.numeric(unlist(strsplit(as.character(tmp.pat$loc.seq), split="|", fixed=T)))
  model <- data.frame(x.plot=1:length(tmp.seq), y.plot=tmp.seq)
  
  p <- ggplot(model, aes(x=x.plot, y=y.plot)) +
    geom_line() +
    geom_smooth() +
    xlab("time (index)") +
    ylab("LOC") +
    ggtitle(paste("pattern #", sid))

  print(p)
}
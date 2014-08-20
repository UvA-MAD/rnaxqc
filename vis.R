library(ggplot2)

boxplot.rnaxqc<- function(count_design, group_factor) {
  .e <- environment()
  p <- ggplot(count_design, environment=.e,
              aes(factor(sampleid), count, fill=factor(count_design[, group_factor])))
  p <- p + geom_boxplot(alpha=0.6, outlier.size=1)
  p <- p + guides(fill=guide_legend(title=group_factor))
  p <- p + labs(x="sample", y="counts")
  p <- p + ggtitle(paste("grouping by", group_factor))
  p <- p + theme(axis.text.x = element_text(angle=45, hjust=1))

  return(p)
}
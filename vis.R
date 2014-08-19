library(ggplot2)

boxplot.rnaxqc<- function(countTable, designTable, group_factor) {
  moltenTable <- melt(countTable)
  names(moltenTable) <- c("seqid", "sampleid", "count")

  # get read of X in sample name
  # (R does not like numbers as column names and prefixes them with "X"
  # when importing with read.table() )
  moltenTable$sampleid <- str_replace_all(moltenTable$sampleid, "^X", "")

  # merge melted table with design table to be able to color plots by
  # grouping factors from design file
  mergedTable <- merge(moltenTable, designTable, by="sampleid")

  .e <- environment()
  ggplot(mergedTable, environment=.e,
              aes(factor(sampleid), count, fill=factor(mergedTable[, group_factor])))
  p <- p + geom_boxplot(alpha=0.6, outlier.size=1)
  p <- p + guides(fill=guide_legend(title=group_factor))
  p <- p + labs(x="sample", y="counts")
  p <- p + ggtitle(paste("grouping by", group_factor))
  p <- p + theme(axis.text.x = element_text(angle=45, hjust=1))

  return(p)
}
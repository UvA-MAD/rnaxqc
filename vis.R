library(ggplot2)

sample_tooltip <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.null(x$sampleid)) return(NULL)
  paste0("<b>", x$sampleid, "</b>")
}

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

densplot.rnaxqc <- function(count_design, group_factor) {
  count_design %>%
    ggvis(~count, stroke = ~sampleid) %>%
    group_by(sampleid) %>%
    layer_densities(fillOpacity := 0.01, strokeOpacity := 0.5, stroke.hover := "red") %>%
    add_tooltip(sample_tooltip, "hover")
}

heatmap1.rnaxqc <- function(counts) {
  correlation <- cor(counts[, 2:ncol(counts)])
  heatmap(correlation)
}

heatmap2.rnaxqc <-function(counts){
  heatmap(as.matrix(counts), cexCol=0.7, labRow=NA)
}

maplot.points.rnaxqc <- function(species_ma) {
    p <- ggplot(species_ma)
    p <- p + geom_point(aes(A, M), alpha=0.05)
    p <- p + facet_wrap(~ sample_name, ncol=2)
    p <- p + labs(x="A", y="M")
    p <- p +  theme(panel.background=element_rect(fill="white", colour="white"))
    return(p)
}

maplot.hexbin.rnaxqc <- function(species_ma) {
  p <- ggplot(species_ma, aes(A, M))
  p <- p + stat_binhex(binwidth = c(0.5, 0.5))
  p <- p + facet_wrap(~ sample_name, ncol=2)
  p <- p + labs(x="A", y="M")
  p <- p + scale_fill_gradientn(trans="log2", colours = rainbow(7))
  return(p)
}
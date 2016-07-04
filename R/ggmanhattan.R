#' Plot a manhattan plot using ggplot2.
#'
#' @param data
#' @param SNP
#' @param chr
#' @param bp
#' @param P
#' @param logP
#' @param build
#' @param theme_base
#' @param scale_color
#' @export
ggmanhattan <- function(data, SNP = "SNP", chr = "CHR", bp = "BP", P = "P", logP = TRUE, build = 'hg19',
                        significance = c(5.0e-8), ylim = NULL, lead_snps = NULL,
                        theme_base = theme_publication(),
                        scale_color = scale_colour_dichromatic(),
                        highlight = NULL, highlight_col = c("mediumblue", "deeppink")) {
  requireNamespace('ggplot2')

  if (is.null(data[[bp]])) {
    stop("NULL BP")
  }
  if (is.null(data[[P]])) {
    stop("NULL P")
  }
  if (is.function(theme_base)) {
    theme_base = theme_base()
  }
  if (is.function(scale_color)) {
    scale_color = scale_color()
  }

  conv = .convert2posX(data[[chr]], data[[bp]], build)
  data$x = conv$posX
  data$color = as.factor(data[[chr]])

  data$y = if (logP) -log10(data[[P]]) else data[[P]]

  plt = ggplot(data, aes(x, y, color = color)) + geom_point() +
          geom_hline(yintercept = -log10(significance), linetype = "dashed") +
          scale_x_continuous(breaks = conv$breaks, labels = conv$labels) +
          theme_base + scale_color +
          theme(legend.position = "none") +
          xlab(conv$xlabel) + ylab(expression(-log[10](italic(P))))

  if (!is.null(highlight)) {
    if (!is.list(highlight)) highlight = list(highlight)
    for (i in 1:length(highlight)) {
      plt = plt + geom_point(data = subset(data, data[[SNP]] %in% highlight[[i]]), color = highlight_col[i])
    }
  }

  if (!is.null(ylim)) {
    plt = plt + coord_cartesian(ylim = ylim)
    if (!is.null(lead_snp)) {
      lead_snp = subset(data, lead_snp %in% data[[snp]])
      plt = plt + geom_text(data = lead_snp, y = Inf, label = "▲", color = "red", size = 3, vjust = -0.1) +
                  geom_text(aes(label = lead_snp[[P]]), data = lead_snp, y = Inf, color = "red", size = 3, vjust = -1.1)
      plt = plt + theme(plot.margin = margin(18, 6, 6, 6))
      gt = ggplot_gtable(ggplot_build(plt))
      gt$layout$clip[gt$layout$name == "panel"] <- "off"
      grid::grid.draw(gt)
      return(gt)
    }
  }
  plt
}

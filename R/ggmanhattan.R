#' Plot a manhattan plot using ggplot2.
#'
#' @param data
#' @param chr
#' @param bp
#' @param P
#' @param logP
#' @param build
#' @param theme_base
#' @param scale_color
#' @export
ggmanhattan <- function(data, chr = "CHR", bp = "BP", P = "P",  logP = TRUE, build = 'hg19',
                        theme_base = theme_publication(),
                        scale_color = scale_color_traditional()) {
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
          geom_hline(yintercept = -log10(5e-8)) +
          scale_x_continuous(breaks = conv$breaks, labels = conv$labels) +
          theme_base + scale_color +
          theme(legend.position = "none") +
          xlab(conv$xlabel) + ylab("-log10 P")
  plt
}

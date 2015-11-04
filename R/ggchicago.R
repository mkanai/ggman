#' Plot a chicago plot using ggplot2.
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
ggchicago <- function(data1, data2, chr = "CHR", bp = "BP", P = "P",  logP = TRUE, build = 'hg19',
                      significance = c(5.0e-8),
                      theme_base = theme_publication(),
                      scale_color = scale_color_traditional()) {
  requireNamespace('ggplot2')

  if (is.null(data1[[bp]]) || is.null(data2[[bp]])) {
    stop("NULL BP")
  }
  if (is.null(data1[[P]]) || is.null(data2[[P]])) {
    stop("NULL P")
  }
  if (is.function(theme_base)) {
    theme_base = theme_base()
  }
  if (is.function(scale_color)) {
    scale_color = scale_color()
  }


  conv1 = .convert2posX(data1[[chr]], data1[[bp]], build)
  data1$x = conv1$posX
  data1$color = as.factor(data1[[chr]])
  data1$y = if (logP) -log10(data1[[P]]) else data1[[P]]

  conv2 = .convert2posX(data2[[chr]], data2[[bp]], build)
  data2$x = conv2$posX
  data2$color = as.factor(data2[[chr]])
  data2$y = if (logP) log10(data2[[P]]) else -data1[[P]]

  breaks = if (length(conv1$breaks) > length(conv2$breaks)) conv1$breaks else conv2$breaks
  labels = if (length(conv1$labels) > length(conv2$labels)) conv1$labels else conv2$labels
  xlabel = ifelse(nchar(conv1$xlabel) > nchar(conv2$xlabel), conv1$xlabel, conv2$xlabel)

  data = rbind(subset(data1, select = c("x", "y", "color")), subset(data2, select = c("x", "y", "color")))

  plt = ggplot(data, aes(x, y, color = color)) + geom_point() +
    geom_hline(yintercept = c(0, -log10(significance), log10(significance))) +
    scale_x_continuous(breaks = breaks, labels = labels) +
    theme_base + scale_color +
    theme(legend.position = "none") +
    xlab(xlabel) + ylab("-log10 P")
  plt
}

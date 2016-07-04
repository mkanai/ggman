#' Plot a qq plot using ggplot2.
#'
#' @param x
#' @param logP
#' @param theme_base
#' @export
gq <- function(x, logP = TRUE, theme_base = theme_publication()) {
  if (is.function(theme_base)) {
    theme_base = theme_base()
  }

  x = x[!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)]
  if (logP) {
    x = -log10(x[x<1 & x>0])
  }

  data = data.frame(observed = sort(x, decreasing = T), expected = -log10(ppoints(length(x))))
  plt = ggplot(data, aes(expected, observed)) +
          geom_point() +
          geom_abline(slope = 1) +
          xlab(expression(Expected~~-log[10](italic(P)))) +
          ylab(expression(Observed~~-log[10](italic(P)))) +
          theme_base
  plt
}
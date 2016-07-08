#' custom ggplot2 scale_colour for a manhattan plot
#'
#' @export
scale_colour_dichromatic = function(values = c("grey30", "grey60")) {
  requireNamespace('ggplot2')
  values = rep(values, 13)
  scale_color_manual(values = values)
}

scale_color_traditional = scale_colour_dichromatic

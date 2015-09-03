#' custom ggplot2 scale_colour for a manhattan plot
#'
#' @export
scale_colour_traditional = function() {
  requireNamespace('ggplot2')
  traditional = rep(c("#000000", "#FF0000", "#008B00", "#0000FF", "#454545", "#EE00EE", "#009ACD", "#EE7600"), 4)
  scale_color_manual(values = traditional)
}

scale_color_traditional = scale_colour_traditional

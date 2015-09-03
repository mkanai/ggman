#' custom ggplot2 theme for publication
#'
#' @param base_size
#' @param base_family
#' @param ... further arguments passed to \code{theme_classic}.
#' @export
theme_publication = function(base_size = 16, base_family = "Helvetica", ...) {
  requireNamespace('ggplot2')
  thm = theme_classic(base_size = base_size, base_family = base_family, ...)
  thm + theme(axis.line.x = element_blank(),
              axis.ticks.x = element_blank())
}
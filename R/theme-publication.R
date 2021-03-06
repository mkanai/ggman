#' custom ggplot2 theme for publication
#'
#' @param base_size
#' @param base_family
#' @param ... further arguments passed to \code{theme_classic}.
#' @export
theme_publication = function(base_size = 16, base_family = "Helvetica", ...) {
  requireNamespace('ggplot2')
  thm = theme_bw(base_size = base_size, base_family = base_family, ...)
  thm + theme(axis.line = element_line(),
              panel.border = element_blank(),
              panel.background  = element_blank(),
              panel.grid.major  = element_blank(),
              panel.grid.minor  = element_blank())
}

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
ggmanhattan <- function(data, SNP = "SNP", chr = "CHR", bp = "BP", P = "P", P_char = NULL, logP = TRUE, build = 'hg19',
                        significance = c(5.0e-8), ylim = NULL,
                        sparsify = TRUE, sparsify_threshold = 0.05, sparsify_nloci = 1e4,
                        lead_snp = NULL, annotate_snp = NULL,
                        theme_base = theme_publication(),
                        scale_color = scale_colour_dichromatic(),
                        highlight = NULL, highlight_col = c("mediumblue", "deeppink"),
                        plot.grid = FALSE,
                        expand.x = c(0.03, 0.03), expand.y = c(0.03, 0.03)) {
  requireNamespace('ggplot2')

  idx = match(c(SNP, chr, bp, P), colnames(data))
  if (sum(is.na(idx)) > 0) {
    stop(paste(c(SNP, chr, bp, P)[which(is.na(idx))], "not found.", sep = " "))
  }
  colnames(data)[idx] = c("SNP", "CHR", "BP", "P")

  if (is.null(data$BP)) {
    stop("NULL BP")
  }
  if (is.null(data$P)) {
    stop("NULL P")
  }
  if (is.null(data$SNP) & (!is.null(lead_snp) | !is.null(annotate_snp))) {
    stop("NULL SNP")
  }
  if (is.function(theme_base)) {
    theme_base = theme_base()
  }
  if (is.function(scale_color)) {
    scale_color = scale_color()
  }

  conv = .convert2posX(data$CHR, data$BP, build)
  data$x = conv$posX
  data$color = as.factor(data$CHR)

  data$y = if (logP) -log10(data$P) else data$P
  if (sparsify) {
    idx = which(data$y < -log10(sparsify_threshold))
    idx = sample(idx, length(idx) - sparsify_nloci)
    data = data[-idx,]
  }

  message(sprintf("Plotting %d points...", nrow(data)))

  plt = ggplot(data, aes(x, y, color = color)) + geom_point() +
          geom_hline(yintercept = -log10(significance), linetype = "dashed") +
          scale_x_continuous(breaks = conv$breaks, labels = conv$labels, expand = expand.x) +
          scale_y_continuous(expand = expand.y) +
          theme_base + scale_color +
          theme(axis.text.x = element_text(size = rel(0.75)), legend.position = "none") +
          xlab(conv$xlabel) + ylab(expression(-log[10](italic(P))))

  if (!is.null(lead_snp)) {
    lead_snp = subset(data, data$SNP %in% lead_snp)
  }

  if (!is.null(annotate_snp)) {
    lsnp = if(!is.null(ylim)) subset(lead_snp, y < ylim[2]) else lead_snp
    plt = plt + geom_text(data = lsnp, aes(label = data$SNP), color = "black", angle = 90, hjust = -0.1)
  }

  if (!is.null(highlight)) {
    if (!is.list(highlight)) highlight = list(highlight)
    for (i in 1:length(highlight)) {
      plt = plt + geom_point(data = subset(data, data$SNP %in% highlight[[i]]), color = highlight_col[i])
      if (!is.null(lead_snp)) {
        lead_snp$color[lead_snp$SNP %in% highlight[[i]]] = i
      }
    }
    lead_snp$color = factor(lead_snp$color, levels = 1:length(highlight))
  }


  if (!is.null(ylim)) {
    plt = plt + coord_cartesian(ylim = ylim)
    print(lead_snp)
    print(ylim)
    if (!is.null(lead_snp)) {
      lead_snp = subset(lead_snp, y >= ylim[2])
      scale_color_highlight = if(!is.null(highlight)) scale_color_manual(values = highlight_col) else scale_color
      if (is.null(P_char)) {
        lead_snp$label = if(logP) stringr::str_c(sprintf("\u25BA"), sprintf("%.1e",lead_snp$P), sep = "") else sprintf("\u25BA")
      } else if (P_char != FALSE) {
        lead_snp$label = stringr::str_c(sprintf("\u25BA"), lead_snp[[P_char]], sep = "")
      } else {
        lead_snp$label = sprintf("\u25BA")
      }
      plt_annot = ggplot(lead_snp) +
                    geom_text(aes(x, 0, color = color, label = label), size = 3, vjust = 0.5, angle = 90) +
                    scale_x_continuous(limits = c(0, max(data$x)), expand = expand.x) +
                    scale_color_highlight +
                    theme_void() + theme(plot.margin = margin(6,6,-2,6), legend.position = "none")
      g1 = ggplotGrob(plt_annot)
      g2 = ggplotGrob(plt + theme(plot.margin = margin(-2,6,6,6)))
      maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5])
      g1$widths[2:5] <- as.list(maxWidth)
      g2$widths[2:5] <- as.list(maxWidth)
      g = gridExtra::arrangeGrob(g1, g2, heights = c(1, 11))
      if (plot.grid) {
        grid::grid.newpage()
        grid::grid.draw(g)
      }
      return(g)
    }
  } else if (!is.null(annotate_snp)) {
    plt = plt + coord_cartesian(ylim = c(0, ceiling(max(data$y) + 1)))
  }
  plt
}

.convert2posX <- function(chr, bp, build) {
  if (length(chr) != length(bp)) {
    stop("SIZE DIFFER.");
  }

  n_chr = length(unique(chr))
  if (n_chr == 1) {
    return (list(posX = bp,
                 breaks = ggplot2::waiver(),
                 labels = ggplot2::waiver(),
                 xlabel = paste("Chromosome", chr[1])))
  }

  max_chr = max(chr)
  if (build == "hg19") {
    return (list(posX = ..convert2posX(chr, bp, .hg19),
                 breaks = .hg19.breaks[1:max_chr],
                 labels = .labels[1:max_chr],
                 xlabel = "Chromosome"))
  } else {
    posX = numeric(max_chr)
    breaks = numeric(max_chr)
    for (i in 1:max_chr) {
      size = max(bp[chr == i]) * 1.1
      posX[i + 1] = posX[i] + size
      breaks[i] = posX[i] + size / 2
    }
    return (list(posX = ..convert2posX(chr, bp, posX),
                 breaks = breaks[1:max_chr],
                 labels = .labels[1:max_chr],
                 xlabel = "Chromosome"))
  }
}

.labels = c(as.character(seq(22)), "X", "Y", "XY", "MT")

# hg19 chromosome sizes
.hg19 = c(0,           # chr1
          249250621,
          492449994,
          690472424,
          881626700,
          1062541960,
          1233657027,
          1392795690,
          1539159712,
          1680373143,  # chr10
          1815907890,
          1950914406,
          2084766301,
          2199936179,
          2307285719,
          2409817111,
          2500171864,
          2581367074,
          2659444322,
          2722469842,  # chr20
          2781598825,
          2829728720,  # chr22
          2881033286,  # chrX (w/o PAR)
          3033334811,  # chrY (w/o PAR)
          3089739342,  # chrXY (PAR1 & PAR2)
          3095677412)

.hg19.breaks = c(124625310,
                 370850307,
                 591461209,
                 786049562,
                 972084330,
                 1148099493,
                 1313226358,
                 1465977701,
                 1609766427,
                 1748140516,
                 1883411148,
                 2017840353,
                 2142351240,
                 2253610949,
                 2358551415,
                 2454994487,
                 2540769469,
                 2620405698,
                 2690957082,
                 2752034333,
                 2805663772,
                 2855381003,
                 2957184048,
                 3061537076,
                 3092708377,
                 3095685697)
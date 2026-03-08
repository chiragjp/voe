#' Plotting functions for VoE results
#'
#' Volcano plots (2D hexbin and contour) for visualizing the Vibration of
#' Effects, showing hazard ratios vs -log10(p-value).
#'
#' @name plot_vibration
#' @importFrom rlang .data
NULL

#' @keywords internal
CBB_PALETTE <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#' 2D hexbin volcano plot for Cox VoE
#'
#' @param vibObj A VoE result object (output of [conductVibration()] or
#'   [conductVibrationSample()]).
#' @param factor_num Factor level to plot (default 1).
#' @param nbins Number of hexbin bins (default 20).
#' @return A [ggplot2::ggplot()] object.
#' @keywords internal
vib2d_cox <- function(vibObj, factor_num = 1, nbins = 20) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("Package 'ggplot2' is required for plotting. Install it with install.packages('ggplot2').")
	}

	vibFrame <- vibObj$vibFrame
	yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm = TRUE)
	xRange <- range(vibFrame$HR, na.rm = TRUE)
	probs <- c(0.01, 0.5, 0.99)
	hProbs <- c(0.01, 0.5, 0.99)
	subFrame <- subset(vibFrame, vibFrame$factor_level == factor_num)
	subFrame$factor_level <- NULL
	estLevel <- statPerK(subFrame)
	estLevel$HR <- exp(estLevel$estimate)
	pQuant <- quantilesPvalue(subFrame, probs)
	hQuant <- quantilesHR(subFrame, hProbs)

	RHR <- round(hQuant[3, "HR"] / hQuant[1, "HR"], 2)
	RPvalue <- round(-log10(pQuant[1, "pvalue"]) + log10(pQuant[3, "pvalue"]), 2)
	p <- ggplot2::ggplot(subFrame, ggplot2::aes(.data$HR, -log10(.data$pvalue)))
	if (sum(colnames(subFrame) == "has_variable")) {
		p <- p + ggplot2::geom_hex(ggplot2::aes(colour = factor(.data$has_variable)), bins = nbins) +
			ggplot2::scale_fill_gradientn(name = "", colours = c("blue", "yellow"))
	} else {
		p <- p + ggplot2::geom_hex(bins = nbins) +
			ggplot2::scale_fill_gradientn(name = "", colours = c("blue", "yellow"))
	}
	p <- p + ggplot2::geom_point(data = estLevel, color = "red", shape = 1) +
		ggplot2::geom_line(data = estLevel, color = "red")
	p <- p + ggplot2::geom_text(ggplot2::aes(.data$HR, -log10(.data$pvalue), label = .data$k, vjust = -1),
	                            data = estLevel, color = "black")
	pQuant$x <- max(subFrame$HR)
	p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = -log10(.data$pvalue), alpha = .4),
	                             linetype = "dashed", data = pQuant)
	p <- p + ggplot2::geom_text(ggplot2::aes(x = .data$x, y = -log10(.data$pvalue),
	                                         label = round(.data$probs * 100, 2), vjust = -.2), data = pQuant)

	hQuant$y <- max(c(-log10(subFrame$pvalue), -log10(0.05)))
	p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = .data$HR, alpha = .4),
	                             linetype = "dashed", data = hQuant)
	p <- p + ggplot2::geom_text(ggplot2::aes(x = .data$HR, y = .data$y,
	                                         label = round(.data$probs * 100, 2), hjust = -.1, vjust = -.1), data = hQuant)
	p <- p + ggplot2::geom_hline(yintercept = -log10(0.05))
	p <- p + ggplot2::scale_x_continuous(limits = xRange) + ggplot2::scale_y_continuous(limits = yRange)
	p <- p + ggplot2::ggtitle(sprintf("RHR = %.02f\nRP = %.02f", RHR, RPvalue))
	p <- p + ggplot2::xlab("Hazard Ratio") + ggplot2::ylab("-log10(pvalue)") + ggplot2::theme_bw()
	return(p)
}

#' Contour volcano plot for Cox VoE
#'
#' @param vibObj A VoE result object.
#' @param factor_num Factor level to plot (default 1).
#' @param alpha Point transparency (default 1).
#' @return A [ggplot2::ggplot()] object.
#' @keywords internal
vibcontour_cox <- function(vibObj, factor_num = 1, alpha = 1) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("Package 'ggplot2' is required for plotting. Install it with install.packages('ggplot2').")
	}

	vibFrame <- vibObj$vibFrame
	subFrame <- subset(vibObj$vibFrame, vibObj$vibFrame$factor_level == factor_num)
	contourData <- getContoursForPctile(subFrame)
	subFrame$factor_level <- NULL
	yRange <- range(c(-log10(.05), -log10(vibFrame$pvalue)), na.rm = TRUE)
	xRange <- range(vibFrame$HR, na.rm = TRUE)
	probs <- c(0.01, 0.5, 0.99)
	hProbs <- c(0.01, 0.5, 0.99)
	estLevel <- statPerK(subFrame)
	estLevel$HR <- exp(estLevel$estimate)
	pQuant <- quantilesPvalue(subFrame, probs)
	hQuant <- quantilesHR(subFrame, hProbs)
	RHR <- round(hQuant[3, "HR"] / hQuant[1, "HR"], 2)
	RPvalue <- round(-log10(pQuant[1, "pvalue"]) + log10(pQuant[3, "pvalue"]), 2)

	p <- ggplot2::ggplot(subFrame, ggplot2::aes(x = .data$HR, y = -log10(.data$pvalue)))
	if (sum(colnames(subFrame) == "has_variable")) {
		p <- p + ggplot2::geom_point(ggplot2::aes(colour = factor(.data$has_variable)), alpha = alpha) +
			ggplot2::scale_colour_manual(values = CBB_PALETTE)
	} else {
		p <- p + ggplot2::geom_point(alpha = alpha)
	}
	p <- p + ggplot2::geom_contour(data = contourData$densityData,
	                               ggplot2::aes(x = .data$x, y = .data$y, z = .data$z),
	                               breaks = contourData$levels, linewidth = .3)
	p <- p + ggplot2::geom_point(data = estLevel, color = "red", shape = 1) +
		ggplot2::geom_line(data = estLevel, color = "red")
	p <- p + ggplot2::geom_text(ggplot2::aes(.data$HR, -log10(.data$pvalue), label = .data$k, vjust = -1),
	                            data = estLevel, color = "black")
	pQuant$x <- max(subFrame$HR)
	p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = -log10(.data$pvalue), alpha = .4),
	                             linetype = "dashed", data = pQuant)
	p <- p + ggplot2::geom_text(ggplot2::aes(x = .data$x, y = -log10(.data$pvalue),
	                                         label = round(.data$probs * 100, 2), vjust = -.2), data = pQuant)

	hQuant$y <- max(c(-log10(subFrame$pvalue), -log10(0.05)))
	p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = .data$HR, alpha = .4),
	                             linetype = "dashed", data = hQuant)
	p <- p + ggplot2::geom_text(ggplot2::aes(x = .data$HR, y = .data$y,
	                                         label = round(.data$probs * 100, 2), hjust = -.1, vjust = -.1), data = hQuant)

	p <- p + ggplot2::geom_hline(yintercept = -log10(0.05))
	p <- p + ggplot2::scale_x_continuous(limits = xRange) + ggplot2::scale_y_continuous(limits = yRange)
	p <- p + ggplot2::ggtitle(sprintf("RHR = %.02f\nRP = %.02f", RHR, RPvalue))
	p <- p + ggplot2::xlab("Hazard Ratio") + ggplot2::ylab("-log10(pvalue)") + ggplot2::theme_bw()
	return(p)
}

#' Identify models containing a specific adjustment variable
#'
#' Adds a `has_variable` column to `vibObj$vibFrame` indicating whether each
#' model includes the adjustor at position `adjustment_num`.
#'
#' @param vibObj A VoE result object.
#' @param adjustment_num Index of the adjustment variable to flag.
#' @return The modified VoE result object with `has_variable` column added.
#' @export
find_adjustment_variable <- function(vibObj, adjustment_num = 1) {
	vibFrame <- vibObj$vibFrame
	combinations <- vibObj$combinations
	ks <- unique(vibFrame$k)
	vibFrame[, "has_variable"] <- 0
	for (ii in seq_along(ks)) {
		k <- ks[ii]
		adjusters <- combinations[[ii]]
		combIndex <- which(apply(adjusters, 2, function(arr) { sum(arr == adjustment_num) }) == 1)
		if (length(combIndex)) {
			vibFrame[vibFrame$k == k & (vibFrame$combination_index %in% combIndex), "has_variable"] <- 1
		}
	}
	vibObj$vibFrame <- vibFrame
	return(vibObj)
}

#' Plot VoE for Cox models
#'
#' Creates a volcano plot of hazard ratios vs -log10(p-value) from a VoE
#' analysis. Two plot types are available: hexbin (`"bin"`) and contour
#' (`"contour"`).
#'
#' @param vibObj A VoE result object (output of [conductVibration()] or
#'   [conductVibrationSample()]).
#' @param type Plot type: `"bin"` for hexbin or `"contour"` for contour plot.
#' @param factor_num Factor level to plot (default 1).
#' @param adjustment_num If not `NA`, highlights models that include this
#'   adjustment variable index.
#' @param ... Additional arguments passed to the plotting function.
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_vibration_cox <- function(vibObj, type = c("bin", "contour"),
                                factor_num = 1, adjustment_num = NA, ...) {
	if (length(type) > 1) {
		type <- type[1]
	}

	if (!is.na(adjustment_num)) {
		vibObj <- find_adjustment_variable(vibObj, adjustment_num)
	}

	if (type == "bin") {
		return(vib2d_cox(vibObj, factor_num, ...))
	} else if (type == "contour") {
		return(vibcontour_cox(vibObj, factor_num, ...))
	}

	return(NULL)
}

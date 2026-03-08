#' Post-processing and summary statistics for VoE results
#'
#' Functions to compute summary statistics from vibration output, including
#' quantiles, relative hazard ratios (RHR), and relative p-values (RP).
#'
#' @name post_process
NULL

#' Median estimate and p-value
#' @param subFrame A vibration data frame subset.
#' @return A one-row data frame with `estimate` and `pvalue`.
#' @keywords internal
meanEstimate <- function(subFrame) {
	pval <- stats::median(subFrame$pvalue)
	hr <- stats::median(subFrame$estimate)
	return(data.frame(estimate = hr, pvalue = pval))
}

#' Mean Manhattan distance
#'
#' Computes pairwise Manhattan distances, normalizes by absolute values,
#' and returns the mean as a percentage.
#'
#' @param arr A numeric vector.
#' @return Mean relative Manhattan distance (percent).
#' @keywords internal
mean_manhattan <- function(arr) {
	dd <- as.matrix(stats::dist(arr, method = "manhattan"))
	dd <- dd / abs(arr)
	mean(dd[upper.tri(dd)]) * 100
}

#' CDF of p-values at selected thresholds
#' @param subFrame A vibration data frame subset.
#' @param pvalues Numeric vector of p-value thresholds.
#' @return A data frame with columns `pvalue`, `cdf`, and `number`.
#' @keywords internal
cdfPerPvalue <- function(subFrame, pvalues = c(10^(-10:-2), .02, .03, .04, .05, .06, .07, .08, .09, .1)) {
	Fn <- stats::ecdf(subFrame$pvalue)
	data.frame(pvalue = pvalues, cdf = Fn(pvalues), number = Fn(pvalues) * nrow(subFrame))
}

#' Quantiles of p-values
#' @param subFrame A vibration data frame subset.
#' @param probs Probability quantiles.
#' @return A data frame with columns `probs` and `pvalue`.
#' @export
quantilesPvalue <- function(subFrame, probs = c(0.01, .25, 0.5, .75, 0.99)) {
	qs <- stats::quantile(subFrame$pvalue, probs)
	data.frame(probs = probs, pvalue = qs)
}

#' Quantiles of hazard ratios
#' @param subFrame A vibration data frame subset.
#' @param probs Probability quantiles.
#' @return A data frame with columns `probs` and `HR`.
#' @export
quantilesHR <- function(subFrame, probs = c()) {
	qs <- stats::quantile(subFrame$estimate, probs)
	data.frame(probs = probs, HR = exp(qs))
}

#' Quantiles of estimates
#' @param subFrame A vibration data frame subset.
#' @param probs Probability quantiles.
#' @return A data frame with columns `probs` and `estimate`.
#' @export
quantilesEstimate <- function(subFrame, probs) {
	qs <- stats::quantile(subFrame$estimate, probs)
	data.frame(probs = probs, estimate = qs)
}

#' Summary statistics per k
#'
#' Computes the median estimate and median p-value for each value of k
#' (number of adjustors).
#'
#' @param vibFrame A vibration data frame.
#' @return A data frame with columns `k`, `estimate`, and `pvalue`.
#' @export
statPerK <- function(vibFrame) {
	estLevel <- data.frame()
	ks <- sort(unique(vibFrame$k))
	for (ii in ks) {
		subFrame <- subset(vibFrame, vibFrame$k == ii)
		mn <- meanEstimate(subFrame)
		estLevel <- rbind(estLevel, data.frame(k = ii, estimate = mn$estimate, pvalue = mn$pvalue))
	}
	estLevel
}

#' Summary statistics per k and factor level
#' @param vibFrame A vibration data frame.
#' @return A data frame with columns `k`, `estimate`, `pvalue`, and
#'   `factor_level`.
#' @export
statPerKandFactor <- function(vibFrame) {
	levs <- unique(vibFrame$factor_level)
	estLevels <- data.frame()
	for (ii in levs) {
		subFrame <- subset(vibFrame, vibFrame$factor_level == ii)
		estLevel <- statPerK(subFrame)
		estLevel$factor_level <- ii
		estLevels <- rbind(estLevels, estLevel)
	}
	return(estLevels)
}

#' Summarize VoE results
#'
#' Computes RHR (relative hazard ratio), RP (relative p-value), quantiles,
#' CDF of p-values, and per-k summaries. Optionally identifies the
#' best-BIC model.
#'
#' @param vibFrame A vibration data frame (e.g., `vib$vibFrame`).
#' @param bicFrame Optional BIC data frame (e.g., `vib$bicFrame`).
#' @return A list with components `summary`, `pvalue_cdf`, and `summary_per_k`.
#' @export
summary_vibration <- function(vibFrame, bicFrame = NULL) {
	levs <- unique(vibFrame$factor_level)
	summaryFrame <- data.frame()
	pvalue_cdf <- data.frame()
	bestMod <- NULL
	if (!is.null(bicFrame)) {
		combInd <- bicFrame[which.min(bicFrame[, 2]), 3]
		bestK <- bicFrame[which.min(bicFrame[, 2]), 4]
		bestMod <- subset(vibFrame, vibFrame$k == bestK & vibFrame$combination_index == combInd)
	}
	for (ii in levs) {
		subFrame <- subset(vibFrame, vibFrame$factor_level == ii)

		hrs <- quantilesHR(subFrame, probs = c(.01, .5, .99))
		ps <- quantilesPvalue(subFrame, probs = c(.01, .5, .99))
		hr_01 <- hrs[1, "HR"]
		hr_50 <- hrs[2, "HR"]
		hr_99 <- hrs[3, "HR"]
		p_01 <- ps[1, "pvalue"]
		p_50 <- ps[2, "pvalue"]
		p_99 <- ps[3, "pvalue"]
		RHR <- hr_99 / hr_01
		vibP <- -log10(p_01) + log10(p_99)
		frm <- data.frame(HR_01 = hr_01, HR_50 = hr_50, HR_99 = hr_99,
		                   pvalue_01 = p_01, pvalue_50 = p_50, pvalue_99 = p_99,
		                   rHR = RHR, rPvalue = vibP, factor_level = ii)
		if (!is.null(bestMod)) {
			bestSub <- subset(bestMod, bestMod$factor_level == ii)
			frm$HR_bic <- bestSub[1, "HR"]
			frm$pvalue_bic <- bestSub[1, "pvalue"]
		}

		summaryFrame <- rbind(summaryFrame, frm)
		cdfPerP <- cdfPerPvalue(subFrame)
		cdfPerP$factor_level <- ii
		pvalue_cdf <- rbind(pvalue_cdf, cdfPerP)
	}

	perK <- statPerKandFactor(vibFrame)

	return(list(summary = summaryFrame, pvalue_cdf = pvalue_cdf, summary_per_k = perK))
}

#' Summarize VoE results by stratum
#'
#' Applies [summary_vibration()] to each stratum in the vibration data frame.
#'
#' @param vibFrame A vibration data frame with a `stratum` column.
#' @return A list with components `summary`, `pvalue_cdf`, and `summary_per_k`,
#'   each containing a `stratum` column.
#' @export
summary_vibration_stratum <- function(vibFrame) {
	strata <- unique(vibFrame$stratum)
	summaryFrame <- data.frame()
	summary_per_k <- data.frame()
	pvalue_cdf <- data.frame()
	for (ii in strata) {
		perStrat <- summary_vibration(subset(vibFrame, vibFrame$stratum == ii))
		perStrat$summary[, "stratum"] <- ii
		perStrat$summary_per_k[, "stratum"] <- ii
		perStrat$pvalue_cdf[, "stratum"] <- ii
		summaryFrame <- rbind(summaryFrame, perStrat$summary)
		summary_per_k <- rbind(summary_per_k, perStrat$summary_per_k)
		pvalue_cdf <- rbind(pvalue_cdf, perStrat$pvalue_cdf)
	}

	return(list(summary = summaryFrame, pvalue_cdf = pvalue_cdf, summary_per_k = summary_per_k))
}

#' Compute contour levels for VoE density plot
#'
#' Uses 2D kernel density estimation to compute contour levels at specified
#' percentiles.
#'
#' @param vib A vibration data frame with `HR` and `pvalue` columns.
#' @param pctiles Percentile levels for contours.
#' @return A list with `levels` and `densityData`.
#' @export
getContoursForPctile <- function(vib, pctiles = seq(.05, .95, by = .05)) {
	dens <- MASS::kde2d(vib$HR, -log10(vib$pvalue), n = 200)
	dx <- diff(dens$x[1:2])
	dy <- diff(dens$y[1:2])
	sz <- sort(dens$z)
	c1 <- cumsum(sz) * dx * dy
	levels <- sapply(pctiles, function(x) {
		stats::approx(c1, sz, xout = 1 - x)$y
	})
	densityData <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
	return(list(levels = levels, densityData = densityData))
}

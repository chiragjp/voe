#' Vibration of Effects
#'
#' Core functions for computing the Vibration of Effects (VoE).
#'
#' @name vibration
#' @references
#' Patel CJ, Burford B, Ioannidis JPA. Assessment of vibration of effects due
#' to model specification can demonstrate the instability of observational
#' associations. *Journal of Clinical Epidemiology*. 2015;68(9):1046-1058.
NULL

#' Convert integer to binary vector
#' @param x Integer to convert.
#' @param n Length of resulting binary vector.
#' @return Integer vector of 0s and 1s.
#' @keywords internal
int_to_binary_vector <- function(x, n) {
	as.integer(intToBits(x))[1:n]
}

#' Fit a model
#'
#' Dispatches to [lm()], [survival::coxph()], or [glm()] based on family.
#'
#' @param form A formula.
#' @param data A data frame.
#' @param family One of `"gaussian"`, `"cox"`, or `"binomial"`.
#' @param ... Additional arguments passed to the model fitting function.
#' @return A fitted model object.
#' @export
run_model <- function(form, data, family = "gaussian", ...) {
	args <- list(form, data = data, ...)
	if (family == "gaussian") {
		args$model <- FALSE
		return(do.call(stats::lm, args))
	}
	if (family == "cox") {
		args$model <- FALSE
		args$x <- FALSE
		args$y <- FALSE
		return(do.call(survival::coxph, args))
	}
	if (family == "binomial") {
		args <- list(form, data, family = stats::binomial(), model = FALSE, ...)
		return(do.call(stats::glm, args))
	}
}

#' Compute VoE for a fixed number of adjustors
#'
#' Fits all combinations of `k` covariates from `adjustby` added to
#' `base_formula` and collects coefficient estimates, p-values, and BIC.
#'
#' @param base_formula A formula specifying the base model (exposure + forced
#'   covariates).
#' @param dataFrame A data frame containing all variables.
#' @param adjustby A formula or character vector of candidate adjustment
#'   variables.
#' @param k Number of adjustors to include in each model.
#' @param family One of `"gaussian"`, `"binomial"`, or `"cox"`.
#' @param print_progress Logical; print progress to console.
#' @param ... Additional arguments passed to [run_model()].
#' @return A list with components `vibration`, `bic`, `k`, `combinations`,
#'   `family`, `base_formula`, and `adjust`.
#' @export
conductVibrationForK <- function(base_formula, dataFrame, adjustby, k = 1,
                                  family = c("gaussian", "binomial", "cox"),
                                  print_progress = TRUE, ...) {
	initFrame <- function(nrows, ncols) {
		matrix(NA, nrows, ncols)
	}

	addToBase <- function(base_formula, adjustingVariables) {
		form <- base_formula
		if (length(adjustingVariables)) {
			addStr <- stats::as.formula(sprintf("~ . + %s", paste(adjustingVariables, collapse = "+")))
			form <- stats::update.formula(base_formula, addStr)
		}
		return(form)
	}

	variablename <- attr(stats::terms(base_formula), "term.labels")[1]
	varname <- all.vars(stats::as.formula(sprintf("~%s", variablename)))
	if (print_progress) print(varname)

	if (inherits(adjustby, "formula")) {
		adjustby <- attr(stats::terms(adjustby), "term.labels")
	}
	n <- length(adjustby)
	varComb <- utils::combn(n, k)
	retFrame <- NULL
	retFrameCounter <- 1
	bicFrame <- NULL
	for (ii in 1:ncol(varComb)) {
		if (print_progress) cat(sprintf("%i/%i\n", ii, ncol(varComb)))

		adjustingVariables <- adjustby[varComb[, ii]]
		strComb <- paste(sort(varComb[, ii]), collapse = ",")
		form <- addToBase(base_formula, adjustingVariables)
		if (print_progress) print(form)

		est <- tryCatch(
			run_model(form, dataFrame, family, ...),
			error = function(err) {
				message(err)
				return(NULL)
			}
		)

		if (!is.null(est)) {
			frm <- stats::coef(summary(est))
			bicMod <- getBIC(est)
			rowIndex <- grep(varname, rownames(frm))
			nLevels <- length(rowIndex)

			if (length(rowIndex) & is.null(retFrame)) {
				nrows <- ncol(varComb) * nLevels
				ncols <- ncol(frm)
				retFrame <- initFrame(nrows, ncols + 2)
				bicFrame <- initFrame(ncol(varComb), 3)
				colnames(retFrame) <- c(colnames(frm), "combination_index", "factor_level")
				colnames(bicFrame) <- c("edf", "bic", "combination_index")
			}

			bicFrame[ii, "combination_index"] <- ii
			bicFrame[ii, "edf"] <- bicMod[1]
			bicFrame[ii, "bic"] <- bicMod[2]

			for (jj in 1:length(rowIndex)) {
				retFrame[retFrameCounter, 1:ncol(frm)] <- frm[rowIndex[jj], ]
				retFrame[retFrameCounter, ncol(frm) + 1] <- ii
				retFrame[retFrameCounter, ncol(frm) + 2] <- jj
				retFrameCounter <- retFrameCounter + 1
			}
		}
	}
	return(list(vibration = retFrame, bic = bicFrame, k = k,
	            combinations = varComb, family = family,
	            base_formula = base_formula, adjust = adjustby))
}

#' Compute BIC for a model
#' @param mod A fitted model object.
#' @return Numeric vector with effective degrees of freedom and BIC.
#' @keywords internal
getBIC <- function(mod) {
	return(stats::extractAIC(mod, k = log(mod$nevent)))
}

#' Recompute p-values from z-statistics
#'
#' Some p-values are numerically zero due to large test statistics; this
#' recomputes them from the normal distribution.
#'
#' @param allData A data frame with z-statistic and p-value columns.
#' @param zStatColName Name of the z-statistic column.
#' @param pValColName Name of the p-value column.
#' @return The data frame with recomputed p-values.
#' @keywords internal
recomputePvalue <- function(allData, zStatColName, pValColName) {
	zeroPval <- !is.na(allData[, pValColName]) & (allData[, pValColName] == 0)
	allData[zeroPval, pValColName] <- stats::pnorm(abs(allData[zeroPval, zStatColName]), lower.tail = FALSE) * 2
	return(allData)
}

#' Exhaustive Vibration of Effects
#'
#' Fits all possible covariate adjustment subsets (from `kMin` to `kMax`
#' adjustors) and collects the resulting coefficient estimates, p-values,
#' and BIC values.
#'
#' @param base_formula A formula specifying the base model.
#' @param dataFrame A data frame containing all variables.
#' @param adjustby A formula or character vector of candidate adjustment
#'   variables.
#' @param family One of `"gaussian"`, `"binomial"`, or `"cox"`.
#' @param kMin Minimum number of adjustors (default 1).
#' @param kMax Maximum number of adjustors (default `length(adjustby) - 1`).
#' @param print_progress Logical; print progress to console.
#' @param ... Additional arguments passed to [run_model()].
#' @return A list with components `vibFrame`, `bicFrame`, `combinations`,
#'   `adjust`, `family`, and `base_formula`.
#' @export
conductVibration <- function(base_formula, dataFrame, adjustby,
                              family = c("gaussian", "binomial", "cox"),
                              kMin = NULL, kMax = NULL,
                              print_progress = TRUE, ...) {
	if (is.null(kMin)) {
		kMin <- 1
	}
	if (is.null(kMax)) {
		n <- length(attr(stats::terms(adjustby), "term.labels"))
		kMax <- n - 1
	}
	cat(sprintf("running models; k start:%i, k stop:%i\n", kMin, kMax))
	retFrame <- list()
	ii <- 1
	for (k in kMin:kMax) {
		vib <- conductVibrationForK(base_formula, dataFrame, adjustby, k, family, print_progress, ...)
		retFrame[[ii]] <- vib
		ii <- ii + 1
	}
	ret <- gatherFrames(retFrame)
	return(ret)
}

#' Sampling-Based Vibration of Effects
#'
#' Instead of fitting all 2^n - 1 covariate subsets, draws a random sample of
#' `n_models` adjustment combinations. Supports multi-core parallelization via
#' [parallel::mclapply()].
#'
#' Falls back to [conductVibration()] automatically when `n_models` exceeds the
#' total number of possible subsets.
#'
#' @inheritParams conductVibration
#' @param n_models Number of random covariate subsets to sample (default 1000).
#' @param n_cores Number of cores for parallel execution (default 1).
#' @return A list with components `vibFrame`, `bicFrame`, `combinations`,
#'   `adjust`, `family`, `base_formula`, `n_models_requested`, and
#'   `n_models_fit`.
#' @export
conductVibrationSample <- function(base_formula, dataFrame, adjustby,
                                    family = c("gaussian", "binomial", "cox"),
                                    n_models = 1000, n_cores = 1,
                                    print_progress = TRUE, ...) {
	family <- match.arg(family)

	adjustby_original <- adjustby
	if (inherits(adjustby, "formula")) {
		adjustby <- attr(stats::terms(adjustby), "term.labels")
	}
	n_adj <- length(adjustby)

	total_combos <- 2^n_adj - 1
	if (n_models >= total_combos) {
		if (print_progress) cat(sprintf("n_models (%i) >= 2^%i - 1 (%i); falling back to exhaustive enumeration\n",
		                                 n_models, n_adj, total_combos))
		return(conductVibration(base_formula, dataFrame, adjustby_original, family,
		                         print_progress = print_progress, ...))
	}

	variablename <- attr(stats::terms(base_formula), "term.labels")[1]
	varname <- all.vars(stats::as.formula(sprintf("~%s", variablename)))
	if (print_progress) cat(sprintf("Sampling %i of %i possible models for %s\n", n_models, total_combos, varname))

	sampled_indices <- sample.int(total_combos, n_models)

	addToBase <- function(base_formula, adjustingVariables) {
		if (length(adjustingVariables) == 0) return(base_formula)
		addStr <- stats::as.formula(sprintf("~ . + %s", paste(adjustingVariables, collapse = "+")))
		stats::update.formula(base_formula, addStr)
	}

	fit_one <- function(idx) {
		bits <- int_to_binary_vector(sampled_indices[idx], n_adj)
		selected <- adjustby[bits == 1L]
		k <- length(selected)
		form <- addToBase(base_formula, selected)

		est <- tryCatch(
			run_model(form, dataFrame, family, ...),
			error = function(err) {
				message(err)
				return(NULL)
			}
		)

		if (is.null(est)) return(NULL)

		frm <- stats::coef(summary(est))
		bicMod <- getBIC(est)
		rowIndex <- grep(varname, rownames(frm))
		if (length(rowIndex) == 0) return(NULL)

		list(
			coefs = frm[rowIndex, , drop = FALSE],
			colnames_coefs = colnames(frm),
			bic_edf = bicMod[1],
			bic_val = bicMod[2],
			k = k,
			combo_idx = idx,
			n_levels = length(rowIndex)
		)
	}

	indices <- seq_len(n_models)
	if (n_cores > 1) {
		if (print_progress) cat(sprintf("Using %i cores\n", n_cores))
		results <- parallel::mclapply(indices, fit_one, mc.cores = n_cores)
	} else {
		if (print_progress) {
			results <- lapply(indices, function(idx) {
				if (idx %% 100 == 0) cat(sprintf("%i/%i\n", idx, n_models))
				fit_one(idx)
			})
		} else {
			results <- lapply(indices, fit_one)
		}
	}

	results <- Filter(Negate(is.null), results)
	if (length(results) == 0) {
		warning("No models converged")
		return(NULL)
	}

	coef_ncols <- ncol(results[[1]]$coefs)
	coef_colnames <- results[[1]]$colnames_coefs

	total_rows <- sum(vapply(results, function(r) r$n_levels, integer(1)))
	vibFrame <- matrix(NA, nrow = total_rows, ncol = coef_ncols + 2)
	colnames(vibFrame) <- c(coef_colnames, "combination_index", "factor_level")

	bicFrame <- matrix(NA, nrow = length(results), ncol = 4)
	colnames(bicFrame) <- c("edf", "bic", "combination_index", "k")

	vib_row <- 1
	for (ii in seq_along(results)) {
		r <- results[[ii]]
		bicFrame[ii, "edf"] <- r$bic_edf
		bicFrame[ii, "bic"] <- r$bic_val
		bicFrame[ii, "combination_index"] <- r$combo_idx
		bicFrame[ii, "k"] <- r$k

		for (jj in 1:r$n_levels) {
			vibFrame[vib_row, 1:coef_ncols] <- r$coefs[jj, ]
			vibFrame[vib_row, coef_ncols + 1] <- r$combo_idx
			vibFrame[vib_row, coef_ncols + 2] <- jj
			vib_row <- vib_row + 1
		}
	}

	k_col <- bicFrame[match(vibFrame[, "combination_index"], bicFrame[, "combination_index"]), "k"]
	vibFrame <- cbind(vibFrame, k = k_col)

	vibFrame <- harmonizeFrame(vibFrame, family)
	vibFrame <- recomputePvalue(vibFrame, "z", "pvalue")

	return(list(vibFrame = vibFrame, bicFrame = bicFrame,
	            combinations = NULL, adjust = adjustby,
	            family = family, base_formula = base_formula,
	            n_models_requested = n_models,
	            n_models_fit = length(results)))
}

#' Gather vibration results across k values
#' @param returnFrames List of results from [conductVibrationForK()].
#' @return A matrix of vibration results with a `k` column appended.
#' @keywords internal
gatherVibration <- function(returnFrames) {
	nrows <- vapply(returnFrames, function(x) nrow(x$vibration), integer(1))

	retFrame <- matrix(nrow = sum(nrows), ncol = ncol(returnFrames[[1]]$vibration) + 1)
	colnames(retFrame) <- c(colnames(returnFrames[[1]]$vibration), "k")

	startIndex <- 1
	for (ii in seq_along(returnFrames)) {
		ncols <- ncol(returnFrames[[ii]]$vibration)
		retFrame[startIndex:(startIndex + nrows[ii] - 1), 1:ncols] <- returnFrames[[ii]]$vibration
		retFrame[startIndex:(startIndex + nrows[ii] - 1), ncols + 1] <- returnFrames[[ii]]$k
		startIndex <- startIndex + nrows[ii]
	}
	return(retFrame)
}

#' Gather BIC results across k values
#' @param returnFrames List of results from [conductVibrationForK()].
#' @return A matrix of BIC results with a `k` column appended.
#' @keywords internal
gatherVibrationBIC <- function(returnFrames) {
	nrows <- vapply(returnFrames, function(x) nrow(x$bic), integer(1))

	retFrame <- matrix(nrow = sum(nrows), ncol = ncol(returnFrames[[1]]$bic) + 1)
	colnames(retFrame) <- c(colnames(returnFrames[[1]]$bic), "k")

	startIndex <- 1
	for (ii in seq_along(returnFrames)) {
		ncols <- ncol(returnFrames[[ii]]$bic)
		retFrame[startIndex:(startIndex + nrows[ii] - 1), 1:ncols] <- returnFrames[[ii]]$bic
		retFrame[startIndex:(startIndex + nrows[ii] - 1), ncols + 1] <- returnFrames[[ii]]$k
		startIndex <- startIndex + nrows[ii]
	}
	return(retFrame)
}

#' Standardize column names for vibration output
#' @param vibFrame A vibration results matrix.
#' @param family Model family.
#' @return Character vector of standardized column names.
#' @keywords internal
column_headers <- function(vibFrame, family) {
	existingColnames <- colnames(vibFrame)
	if (family == "cox") {
		isRobust <- grep("robust", existingColnames)
		if (length(isRobust)) {
			return(c("estimate", "HR", "se", "robust_se", "z", "pvalue", "combination_index", "factor_level", "k"))
		} else {
			return(c("estimate", "HR", "se", "z", "pvalue", "combination_index", "factor_level", "k"))
		}
	} else if (family == "gaussian") {
		existingColnames[1] <- "estimate"
		existingColnames[length(existingColnames) - 4] <- "pvalue"
		return(existingColnames)
	} else if (family == "binomial") {
		existingColnames[1] <- "estimate"
		existingColnames[length(existingColnames) - 4] <- "pvalue"
		return(existingColnames)
	}
	return(existingColnames)
}

#' Harmonize vibration frame column names and add derived columns
#' @param vibFrame A vibration results matrix.
#' @param family Model family.
#' @return A data frame with standardized column names.
#' @keywords internal
harmonizeFrame <- function(vibFrame, family) {
	vibFrame <- as.data.frame(vibFrame)
	colnames(vibFrame) <- column_headers(vibFrame, family)
	if (family %in% c("binomial")) {
		vibFrame$HR <- exp(vibFrame$estimate)
	}
	return(vibFrame)
}

#' Gather and harmonize all vibration results
#' @param returnFrames List of results from [conductVibrationForK()].
#' @return A list with `vibFrame`, `bicFrame`, `combinations`, `adjust`,
#'   `family`, and `base_formula`.
#' @keywords internal
gatherFrames <- function(returnFrames) {
	bic <- gatherVibrationBIC(returnFrames)
	vibration <- gatherVibration(returnFrames)
	combinations <- list()
	for (ii in seq_along(returnFrames)) {
		combinations[[ii]] <- returnFrames[[ii]]$combinations
	}
	family <- returnFrames[[1]]$family
	base_formula <- returnFrames[[1]]$base_formula
	adjust <- returnFrames[[1]]$adjust

	vibration <- harmonizeFrame(vibration, family)
	vibration <- recomputePvalue(vibration, "z", "pvalue")
	return(list(vibFrame = vibration, bicFrame = bic, combinations = combinations,
	            adjust = adjust, family = family, base_formula = base_formula))
}


#' Cut numeric vector into factor vector
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' This function cuts a numeric vector into factors
#' based on specified breaks or categories.
#' If the input vector is not numeric, the function delegates
#' to the base R `cut` function.
#'
#' @param x A numeric vector to be cut into factors.
#' @param at A numeric vector specifying the breakpoints or categories
#' for cutting the vector. If a single value is provided,
#' the function will create breaks using the same method as `[base::cut]`.
#' If multiple values are provided, they are treated as specific breaks.
#' @param label Optional labels for the resulting factor levels.
#' If not provided, labels will be automatically generated based on the breaks.
#' @param ... Additional arguments to be passed to `[base::cut]`
#' if `x` is not numeric.
#'
#'
#' @family Data Management
#' @return A factor representing the cut vector
#' with factor levels assigned based on the breaks or categories.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' cut(x, 2)
#' @export
cut <- function(x, at, label = NULL, ...) {
	var_name <- rlang::expr_text(substitute(x))

	if (is.numeric(x)) {

		# get min, max values
		vmin <- min(x, na.rm = TRUE)
		vmax <- max(x, na.rm = TRUE)

		if (length(at) == 1L) {
			## Here, I implemented the same breaks function from base::cut
			brk <- create_breaks(x, at)
			brk <- brk[-c(1, length(brk))]
		} else {
			brk <- check_infinite(x, at)
		}

		brk <- brk[(brk > vmin) & (brk < vmax)]
		decimal <- max(sapply(brk, count_decimal), na.rm = TRUE)

		if (is.null(label)) {
			label <- paste0(c(vmin, brk), "-", c(brk - 1/10^decimal, vmax))
		}

		new_x <- base::cut.default(x, breaks = c(vmin, brk, vmax), labels = label,
											include.lowest = TRUE, right = FALSE, ...)
	} else {
		new_x <- base::cut(x, at, label, ...)
	}

	# copy label
	labelled::copy_labels(x, new_x)
}


# helper ------------------------------------------------------------------

check_infinite <- function(x, at) {
	ifelse(is.infinite(at), ifelse(at < 0, min(x), max(x)), at)
}


count_decimal <- function(x) {
	if (abs(x - round(x)) > .Machine$double.eps^0.5) {
		nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
	} else {
		return(0)
	}
}
create_breaks <- function(x, breaks) {
	if (is.na(breaks) || breaks < 2L || !is.numeric(breaks)) {
		rlang::abort("Invalid arugment `at`")
	}

	nb <- as.integer(breaks + 1)
	dx <- diff(rx <- range(x, na.rm = TRUE))
	if (dx == 0) {
		dx <- if (rx[1L] != 0)
			abs(rx[1L])
		else 1
		breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
											length.out = nb)
	}
	else {
		breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
		breaks[c(1L, nb)] <- c(rx[1L] - dx/1000, rx[2L] +
													 	dx/1000)
	}
	breaks
}

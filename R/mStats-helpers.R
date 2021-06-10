

# Helpers -----------------------------------------------------------------



#' @export
## if data is not data frame, then stop
.check_dataframe <- function(data, data_name) {
  if (!is.data.frame(data)) {
    stop(paste0(" `", data_name, "` must be a data frame."), call. = FALSE)
  }
}


#' @export
## check whether variables are there in the dataset
.check_vars <- function(data, vars_name) {
  sapply(vars_name, function(z) {
    if (!(z %in% names(data))) {
      stop(paste0(" `", z, "` cannot be found."), call. = FALSE)
    }
  })
}


#### retrieve information for variables                          ----------


#' @export
.get_vars_label <- function(data, vars_name) {
  ## number of characters allowed in the variable label
  char_num <- 30
  vars_label <- sapply(vars_name, function(z) {
    var       <- data[[z]]
    var_label <- attr(var, "label")[1]
    var_label <- ifelse(is.null(var_label), "", var_label)
    var_label <- ifelse(nchar(var_label) > char_num,
                        paste0(strtrim(var_label, char_num), "..."),
                        var_label)
  })

  return(vars_label)
}


#' @export
.get_vars_type <- function(data, vars_name) {
  data_df <- data.frame(data)
  vars_type <- sapply(vars_name, function(z) {
    var       <- data[[z]]
    var_class <- class(var)
    var_class <- ifelse(length(var_class) > 1, var_class[3], var_class)
  })

  return(vars_type)
}


#### add lines to the data frame for messaging                   ----------


#' @export
.add_hline <- function(data, pos = 1) {

  chr_num_vars <- sapply(names(data), function(z) {
    z <- ifelse(is.na(z), "<NA>", z)
    max(nchar(z), na.rm = TRUE)
  })
  chr_num_obs  <- sapply(data, function(z)
    max(ifelse(is.na(nchar(z)), 0, nchar(z)), na.rm = TRUE))
  cols_len     <- sapply(data.frame(rbind(chr_num_vars, chr_num_obs)), max)

  hline <- sapply(cols_len, function(z) {
    paste0(rep("-", z), collapse = "")
  })
  names(hline) <- names(data)

  out <- rbind(hline, data[pos:nrow(data), ])
  row.names(out) <- NULL

  return(out)
}


#' @export
.add_vline <- function(data, pos = 1, symbol = "|") {
  row_num <- nrow(data)
  vline   <- rep("|", row_num)

  data_after <- data[, pos:ncol(data)]
  data_before <- data[, 1:(pos-1)]
  if (pos == 1) {
    data_before <- data[, 0]
    out <- cbind(vline, data_after, vline)
    names(out)[2] <- names(data)[1]
    names(out)[c(1, ncol(out))] <- symbol
  } else {
    out <- cbind(vline, data_before, vline, data_after, vline)
    names(out)[2:pos] <- names(data)[1:(pos-1)]
    names(out)[c(1, pos+1, ncol(out))] <- symbol
  }

  return(out)
}


#' @export
.add_hv_lines <- function(data, hpos = 1, vpos = 1, vsymbol = "|", hsymbol = "+") {
  out <- .add_hline(data, hpos)
  out <- .add_vline(out, vpos, vsymbol)

  vars_name   <- names(out)
  hline       <- out[hpos, ]
  hline[, c(1, vpos + 1, ncol(out))] <- "+"

  out[hpos, ] <- vars_name
  out <- rbind(out[1, ], hline, out[2:nrow(out), ], hline)
  names(out) <- hline
  row.names(out) <- NULL

  return(out)
}

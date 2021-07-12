

#' @export
.dots <- function(args, args_name) {
  args <- args[-1]
  dots <- names(args) %in% args_name # name is in the args_name
  dots <- args[!dots]

  return(as.character(dots))
}


#' @export
.check_dots <- function(data, dots) {
  vars_name <- names(data)
  dots  <- do.call(
    c,
    lapply(dots, function(z) {
      boo <- grepl(":", z) ## have colon
      if (boo) {
        vars_colon <- unlist(strsplit(z, split = ":"))
        .check_vars(data, vars_colon)

        vars_colon <- paste0("^", vars_colon, "$")
        vars_colon <- grep(vars_colon[1],
                           vars_name):grep(vars_colon[2], vars_name)
        vars_name[vars_colon]
      } else {
        z
      }
    })
  )
  vars_name <- unique(dots)
  .check_vars(data, vars_name)

  return(vars_name)
}



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



#' @export
.get_vars_label <- function(data, vars_name, char_num = 260L) {
  vars_label <- sapply(vars_name, function(z) {
    var       <- data[[z]]
    var_label <- attr(var, "label")[1]
    var_label <- ifelse(is.null(var_label), "", var_label)
    var_label <- ifelse(nchar(var_label) > char_num,
                        paste0(strtrim(var_label, char_num), "..."),
                        var_label)
  })

  attr(vars_label, "names") <- NULL
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
  names(out) <- names(data)
  row.names(out) <- NULL

  return(out)
}



#' @export
.add_vline <- function(out, pos = 1, vsymbol = "|") {
  row_num <- nrow(out)
  vline   <- rep("|", row_num)
  if (pos == 1) {
    output <- cbind(vline, out[, (pos:ncol(out))])
    names(output)[pos] <- vsymbol
  } else {
    output <- cbind(out[, 1:(pos-1)], vline, out[, (pos:ncol(out))])
    names(output) <- base::append(names(out), vsymbol, pos-1)
  }

  return(output)
}


#' @export
.add_hv_lines <- function(out, hpos = 1, vpos = 1, jsymbol = "+", vsymbol = "|") {
  out <- .add_hline(out, hpos)
  out <- .add_vline(out, vpos, vsymbol)

  out[1, vpos] <- jsymbol
  out[nrow(out)+1, ] <- out[1, ]

  return(out)
}


#' @export
.format_tab <- function(out) {
  vars <- out[[1]]
  pos  <- c((1:nrow(out))[!(vars == "")], nrow(out) + 1) + 1
  end_pos <- diff(pos)
  pos <- pos[-length(pos)]
  end_pos <- end_pos + pos - 1
  seq_num <- mapply(`:`, pos, end_pos)
  if (is.matrix(seq_num)) {
    seq_num <- lapply(1:ncol(seq_num), function(z) seq_num[, z])
  }

  out <- .add_hv_lines(out, 1, 3)
  out <- do.call(
    rbind,
    lapply(seq_num, function(z) {
      dash  <- out[1, ]
      out   <- out[z, ]
      row_n <- nrow(out)
      out   <- rbind(dash, out[-row_n, ], dash, out[row_n, ])
      out[nrow(out)-1, 1] <- ""
      row.names(out) <- NULL
      out
    })
  )
  out[nrow(out)+1, ] <- out[1, ]

  return(out)
}



#' @export
.print_header <- function(out, y_name) {
  out_name <- names(out)
  col <- grepl("variable|Level|\\|", out_name)
  p1 <- paste(c(" ", paste(out[1, col], collapse = " "), " "), collapse = "")

  col <- grepl("variable|Level|\\||Pr", out_name)
  if (any(grepl("row\\(\\%\\)|col\\(\\%\\)", out_name))) {
    col_n <- length(col)
    col[(col_n-2):(col_n-1)] <- TRUE
  }

  r_char <- nchar(paste(out[1, !col], collapse = " "))
  y_char <- nchar(y_name)

  cat(rep("", nchar(p1) + ((r_char/2) - (y_char/2))), y_name, "\n")
  cat("", paste(out[1, ], collapse = " "), "\n")
}



#' @export
.print_vars_label <- function(data, vars_name) {
  lapply(vars_name, function(z) {
    vars_label <- .get_vars_label(data, z)
    if (vars_label != "") {
      message(" (", z, ": ", vars_label, ")")
    }
  })
}


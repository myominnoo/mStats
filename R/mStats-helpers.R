

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


  attr(vars_label, "names") <- NULL
  return(vars_label)
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
  names(out) <- names(data)
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
  names(out)[-c(1, pos + 1, ncol(out))] <- names(data)

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


#' @export
.formatOutput <- function(out) {
  vars_pos <- c(as.numeric(row.names(out)[!(out$Variable == "")]), nrow(out) + 1) + 2
  end_pos <- diff(vars_pos)
  vars_pos <- vars_pos[-length(vars_pos)]
  end_pos <- end_pos + vars_pos - 1
  seq_num <- mapply(`:`, vars_pos, end_pos)
  if (length(vars_pos) == 1) {
    seq_num <- list(as.numeric(seq_num))
  }

  out     <- .add_hv_lines(out, 1, 3, "|")
  out_add <- do.call(
    rbind,
    lapply(seq_num, function(z) {
      out <- out[z, ]
      row_n <- nrow(out)
      out[row_n + 1, ] <- out[row_n, ]
      out[row_n, ]     <- names(out)
      out[row_n, 2]    <- ""
      out <- rbind(out, names(out))
      row.names(out)   <- NULL
      out
    })
  )
  out <- rbind(out[1:2, ], out_add)

  return(out)
}


#### get names of variables within three dots                    ----------
#' @export
.enquote <- function(args, args_name) {
  data      <- eval(args$data)
  args      <- args[-1]
  boo       <- names(args) %in% args_name # name is in the args_name
  vars_name <- args[!boo]

  return(as.character(vars_name))
}


#' @export
.unquote <- function(data, vars_name) {
  .vars_name <- names(data)
  vars_name  <- do.call(
    c,
    lapply(vars_name, function(z) {
      boo <- grepl(":", z) ## have colon
      if (boo) {
        vars_colon <- unlist(strsplit(z, split = ":"))
        .check_vars(data, vars_colon)

        vars_colon <- paste0("^", vars_colon, "$")
        vars_colon <- grep(vars_colon[1],
                           .vars_name):grep(vars_colon[2], .vars_name)
        .vars_name[vars_colon]
      } else {
        z
      }
    })
  )
  vars_name <- unique(vars_name)
  .check_vars(data, vars_name)

  return(vars_name)
}


#### Printing functions                                      ----------
.print_header <- function(out, y_name) {
  outline  <- paste(names(out), collapse = " ")
  textarea <- paste(
    names(out)[-c(1:4, ncol(out):(ncol(out)-3))], collapse = " "
  )
  negarea  <- paste(names(out)[c(1:4)], collapse = " ")
  textarea <- paste(
    rep(" ", nchar(negarea) + (nchar(textarea) - nchar(y_name)) / 2),
    collapse = ""
  )

  message("\n", textarea, y_name)
}

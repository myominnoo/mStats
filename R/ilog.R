#' @title Simple function for logging console in R
#'
#' @description
#' ilog is a function to log console interactively which is inspired by Stata Software. This
#' function captures outputs from console in a local environment which is running behind
#' the scene. Hence, be advised that clearing the workspace of removing environments can
#' make it stop working.
#'
#' The prompt '>' and continue '+' can be restored by typing this in the console: options
#' (prompt = "> ", continue = "+ ").
#'
#' @param logfile A character, denoted by the name of desired log file which follows by an
#' extension ".txt".
#' @param append A logical value, indicating whether the log file is appended or not.
#' @seealso isum, igroup, ixtab, irestart
#' @keywords log console, save outputs, save console
#' @example
#'
#' ilog("myfirstlog.txt", append = F)
#' isum(infert)
#' str(infert)
#' isum(iris)
#' str(iris)
#' isum(AirPassengers) # not logging the output when errors appear.
#' ilog.close()
#'

#' @export
ilog <- function(logfile = "mylog.txt", append = FALSE) {
  # create a global environment which can be accessed outside
  logging.env <<- new.env()
  # <-- create a connection file -->
  if(!append) {
    # <--- if logfile exists & replace is TRUE, remove logfile --->
    if(file.exists(logfile)) {
      unlink(logfile)
    }
  }
    # write log file
    con <- file(logfile, open ='a')
    logging.env$logfile <- logfile

  if(isOpen(con)) {
    logging.env$con.close <- FALSE
  } else {
    logging.env$con.close <- TRUE
    if(append) {
      open(con, open='a')
    } else {
      open(con, open='w')
    }
  }

  logging.env$con <- con
  logging.env$cmd <- TRUE
  logging.env$res <- TRUE
  logging.env$first <- TRUE

  logging.env$con.out <- textConnection(NULL, open='a')
  sink(logging.env$con.out, split=TRUE)

  logging.env$prompt <- unlist(options('prompt'))
  logging.env$continue <- unlist(options('continue'))

  options(prompt = paste('log', logging.env$prompt, sep = ''),
          continue = paste('log', logging.env$continue,sep = '') )

  # writing log info
  sink(logfile, append = TRUE, split = TRUE)
  if(append) {
    cat(paste0("(note ", getwd(), "/", logfile, " appended)"))
  } else {cat(paste0("(note ", getwd(), "/", logfile, " replace)"))}
  cat(paste('\n\n'))
  cat(paste0('    log: ', getwd(), "/", logfile))
  cat(paste0('\nopen on: ', Sys.time(),'\n'))
  cat(paste0(rep('.', 40)))
  cat(paste('\n\n'))
  sink()

  addTaskCallback(ilogtxt, name = "ilogtxt")
  invisible(NULL)
}

#' @rdname  ilog
#' @export
ilog.close <- function() {
  removeTaskCallback(id = "ilogtxt")
  if(!logging.env$con.close) {
    close(logging.env$con)
  }
  options( prompt = logging.env$prompt,
           continue = logging.env$continue )
  if(logging.env$res) {
    sink()
    close(logging.env$con.out)
    closeAllConnections()
  }
  # writing log info
  sink(logging.env$logfile, append = TRUE, split = TRUE)
  cat(paste0(rep('.', 40)))
  cat(paste0('\n\n     log: ', getwd(), "/", logging.env$logfile))
  cat(paste0('\nclosed on: ', Sys.time(),'\n\n'))
  sink()
  evalq(rm(list=ls()), envir = logging.env )
  invisible(NULL)
}

ilogtxt <- function(cmd, res, s, vis) {
  if(logging.env$first) {
    logging.env$first <- FALSE
    if( logging.env$res ) {
      sink()
      close(logging.env$con.out)
      logging.env$con.out <- textConnection(NULL, open='a')
      sink(logging.env$con.out, split=TRUE)
    }
  } else {
    if(logging.env$cmd){
      cmdline <- deparse(cmd)
      cmdline <- gsub('    ', paste("\n", logging.env$continue, sep =''),
                      cmdline)
      cmdline <- gsub('}', paste("\n", logging.env$continue,"}", sep =''),
                      cmdline)
      cat(logging.env$prompt, cmdline, "\n", sep = '',
          file=logging.env$con)
    }
    if(logging.env$res) {
      tmp <- textConnectionValue(logging.env$con.out)
      if(length(tmp)) {
        cat(tmp, sep='\n', file = logging.env$con)
        sink()
        close(logging.env$con.out)
        logging.env$con.out <- textConnection(NULL, open='a')
        sink(logging.env$con.out, split=TRUE)
      }
    }
  }
  TRUE
}

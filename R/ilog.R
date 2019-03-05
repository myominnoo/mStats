#' @title Echo a copy of console to log file
#'
#' @description
#' \code{ilog} allows to echo a copy of your console to log file. 
#' 
#' @param logfile Name of desired log file in \code{.txt} format
#' @param append specify if the log file will be appended or not
#' @details 
#' \code{ilog} is a two-step function that allows you a record of your console. 
#' A log is a file containing what you type and console output. If a name is not 
#' specified, then \code{ilog} will use the name \code{<unnamed>.txt}.
#' 
#' \code{ilog} opens a log file and \code{ilog.close} close the file. 
#' 
#' \strong{Warnings:}
#' 
#' Be reminded that clearing the environments while logging distrubs the 
#' process. If such happens, console prompt will be stuck at \code{log> }. 
#' Run the \code{commands} below to revert the console prompt back to normal.
#' 
#' options(prompt = \code{"> "}, continue = \code{"+ "})
#' 
#' \strong{Acknowledgement:}
#' \code{ilog} and \code{ilog.close} are inspriations from \code{STATA} software. 
#' 
#' @seealso \code{\link{idetach}}, \code{\link{clear}}, 
#' @keywords log console, save outputs, save console
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' ilog("myfirstlog.txt")
#' str(infert)
#' itab(infert$education)
#' str(iris)
#' isum(iris$Sepal.Length)
#' ilog.close()
#' 
#' ## appending the file. 
#' ilog("myfirstlog.txt", append = TRUE)
#' summary(infert)
#' summary(iris)
#' ilog.close()

#' @export
ilog <- function(logfile = "<unnamed>.txt", append = FALSE) {
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
  } else {cat(paste0("(note: ", getwd(), "/", logfile, " replace)"))}
  cat(paste0('\n', '    log: ', getwd(), "/", logfile, '\nopen on: ', Sys.time(),'\n'))
  cat(rep('.', 40), '\n\n')
  sink()

  addTaskCallback(ilogtxt, name = "ilogtxt")
  invisible(NULL)
}

#' @rdname  ilog
#' @export
ilog.close <- function() {
  removeTaskCallback(id = "ilogtxt")
  logging.env <- as.environment(logging.env)
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
  cat(paste0('\n\n     log: ', getwd(), "/", logging.env$logfile, '\nclosed on: ',
             Sys.time(),'\n\n'))
  sink()
  eval(rm(list= "logging.env", envir = sys.frame(-1)))
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

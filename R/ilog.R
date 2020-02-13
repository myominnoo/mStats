#' @title Echo a copy of console to log file
#'
#' @description
#' \code{ilog} echo a copy of console output to log file.
#'
#' @param logfile Name of desired log file in \code{.txt} format
#' @param append a logical value
#' @details
#' \code{ilog} is a two-step function that allows you a record of your console.
#' A log is a file containing what you type and console output. If a name is not
#' specified, then \code{ilog} will use the name \code{<unnamed>.txt}.
#'
#' \code{ilog} opens a log file and \code{ilog.close} close the file.
#'
#' \strong{Warnings:}
#'
#' Clearing the global environment while logging will interrupt the logging
#' process. In that case, console prompt will be stuck at \code{log> }.
#' Run the \code{code} below to get the console prompt back to normal.
#'
#' options(prompt = \code{"> "}, continue = \code{"+ "})
#'
#' @seealso \code{\link{clear}}
#' @keywords log, console ouput, save console, echo, copy
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' ## my first log
#' ilog("myfirstlog.txt")
#' str(infert)
#' tab(infert$education)
#' str(iris)
#' isum(iris$Sepal.Length)
#' ilog.close()
#'
#' ## appending the file.
#' ilog("myfirstlog.txt", append = TRUE)
#' summary(infert)
#' summary(iris)
#' ilog.close()
#' }

#' @export
ilog <- function(logfile = "LOG.txt", append = FALSE) {
  # create a global environment which can be accessed outside
  .logenv <<- new.env()
  # <-- create a connection file -->
  if(!append) {
    # <--- if logfile exists & replace is TRUE, remove logfile --->
    if(file.exists(logfile)) {
      unlink(logfile)
    }
  }
  # write log file
  con <- file(logfile, open ='a')
  .logenv$logfile <- logfile

  if(isOpen(con)) {
    .logenv$con.close <- FALSE
  } else {
    .logenv$con.close <- TRUE
    if(append) {
      open(con, open='a')
    } else {
      open(con, open='w')
    }
  }

  .logenv$con <- con
  .logenv$cmd <- TRUE
  .logenv$res <- TRUE
  .logenv$first <- TRUE

  .logenv$con.out <- textConnection(NULL, open='a')
  sink(.logenv$con.out, split=TRUE)

  .logenv$prompt <- unlist(options('prompt'))
  .logenv$continue <- unlist(options('continue'))

  options(prompt = paste('log', .logenv$prompt, sep = ''),
          continue = paste('log', .logenv$continue,sep = '') )

  # writing log info
  sink(logfile, append = TRUE, split = TRUE)
  cat(paste0('      log: ', getwd(), "/", logfile,
             '\n  open on: ', Sys.time(),'\n'))
  if(append) {
    cat(paste0("     note: ", "appended\n"))
  } else {cat(paste0("     note: ", "replaced\n"))}
  cat(rep('-', 70), '\n\n', sep = "")
  sink()

  addTaskCallback(ilogtxt, name = "ilogtxt")
  invisible(NULL)
}

#' @rdname  ilog
#' @export
ilog.close <- function() {
  removeTaskCallback(id = "ilogtxt")
  .logenv <- as.environment(.logenv)
  if(!.logenv$con.close) {
    close(.logenv$con)
  }
  options( prompt = .logenv$prompt,
           continue = .logenv$continue )
  if(.logenv$res) {
    sink()
    close(.logenv$con.out)
    # closeAllConnections()
  }
  # writing log info
  sink(.logenv$logfile, append = TRUE, split = TRUE)
  cat(rep('-', 70), '\n', sep = "")
  cat(paste0('      log: ', getwd(), "/", .logenv$logfile, "\n",
             'closed on: ',
             Sys.time(),'\n\n'))
  sink()
  eval(rm(list= ".logenv", envir = sys.frame(-1)))
  invisible(NULL)
}

#' @rdname  ilog
#' @param cmd command line
#' @param res result
#' @param s others
#' @param vis others
#' @export
ilogtxt <- function(cmd, res, s, vis) {
  if(.logenv$first) {
    .logenv$first <- FALSE
    if( .logenv$res ) {
      sink()
      close(.logenv$con.out)
      .logenv$con.out <- textConnection(NULL, open='a')
      sink(.logenv$con.out, split=TRUE)
    }
  } else {
    if(.logenv$cmd){
      cmdline <- deparse(cmd)
      cmdline <- gsub('    ', paste("\n", .logenv$continue, sep =''),
                      cmdline)
      cmdline <- gsub('}', paste("\n", .logenv$continue,"}", sep =''),
                      cmdline)
      cat(.logenv$prompt, cmdline, "\n", sep = '',
          file=.logenv$con)
    }
    if(.logenv$res) {
      tmp <- textConnectionValue(.logenv$con.out)
      if(length(tmp)) {
        cat(tmp, sep='\n', file = .logenv$con)
        sink()
        close(.logenv$con.out)
        .logenv$con.out <- textConnection(NULL, open='a')
        sink(.logenv$con.out, split=TRUE)
      }
    }
  }
  TRUE
}

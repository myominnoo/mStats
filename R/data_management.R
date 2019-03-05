
dup.rm <- function(x, na.rm = FALSE) {
  x <- x[duplicated(x) == FALSE]
  if (na.rm) x <- x[!is.na(x)]
  return(x)
}

label <- function(x, labels)
{
  if (is.numeric(x)) {
    x <- factor(x, labels = labels)
  } else if (is.character(x)) {
    x <- factor(x, labels = labels, ordered = FALSE)
  } else if (is.factor(x)) {
    x <- factor(unclass(x), labels = labels)
  } else if (is.logical(x)) {
    x <- factor(2 - x, labels = labels, ordered = FALSE)
  } else stop('x can not be labelled.')
  return(x)
}


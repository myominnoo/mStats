# checking packages required to work on
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  cat(paste0("\ninstalling ", paste(new.packages, " ")))
  install.packages(new.packages)
} else {cat(" ... downloading and installing 'mStats' package ... \nThank you for downloading. \nFor more information about the author, please visit https://myominnoogithub.io.\n")}

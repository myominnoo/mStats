# checking packages required to work on
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  cat(paste0("\ninstalling ", paste(new.packages, " ")))
  install.packages(new.packages, dependencies = TRUE)
} else {cat("\nThank you for downloading 'mStats' package. \nFor more information about the author, please visit https://myominnoogithub.io.\n For any comments or suggestions, please email me at dr.myominnoo@gmail.com.\n")}

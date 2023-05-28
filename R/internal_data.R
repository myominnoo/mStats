#
# ## All data from UCLA STATA
#
# # https://stats.oarc.ucla.edu/stata/modules/labeling-data/
# autolab <- haven::read_dta("https://stats.idre.ucla.edu/stat/stata/modules/autolab.dta")
#
# # https://stats.oarc.ucla.edu/stata/faq/how-can-i-detect-duplicate-observations-3/
# hsb <- haven::read_dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")
#
# # https://stats.oarc.ucla.edu/stata/faq/how-can-i-anonymize-patient-ids-and-still-be-able-to-decode-them-if-necessary/
# patient_data <- haven::read_dta("https://stats.idre.ucla.edu/stat/data/patient_data.dta")
#
# # https://stats.oarc.ucla.edu/stata/modules/collapsing-data-across-observations/
# kids <- haven::read_dta("https://stats.idre.ucla.edu/stat/stata/modules/kids.dta")
#
# # https://stats.oarc.ucla.edu/stata/modules/reshaping-data-wide-to-long/
# faminc <- haven::read_dta("https://stats.idre.ucla.edu/stat/stata/modules/faminc.dta")
#
#
#
# # write them to the pacakge as internal data
# usethis::use_data(
# 	internal = TRUE, overwrite = TRUE,
# 	autolab,
# 	hsb,
# 	patient_data,
# 	kids, faminc
# )

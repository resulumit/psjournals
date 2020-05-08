#data-raw/rawdata.R

# import
psjournals <- read.csv("data-raw/psjournals.csv", na.strings="")

# use
usethis::use_data(psjournals, overwrite = TRUE)

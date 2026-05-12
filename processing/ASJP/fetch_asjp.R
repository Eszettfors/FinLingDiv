library(devtools)
library(tidyverse)
library(readr)

if ("rcldf" %in% installed.packages()) {
  print("rcldf is installed")
} else{
  print("installing rcldf......")
  install_github("SimonGreenhill/rcldf", dependencies = TRUE)
}

library(rcldf)

# this script fetches the asjp data from zenodo and saves it as an rdf

asjp = rcldf::cldf(mdpath = "https://zenodo.org/records/16736409/files/lexibank/asjp-v21.zip")
write_rds(asjp, "data/raw/asjp_database.rds")



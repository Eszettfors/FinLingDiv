library(rcldf)
library(tidyverse)

# this script fetches the glottolog v.5.2.1 data from zenodo and saves it as an rds

glotto = rcldf::cldf(
  mdpath = "https://zenodo.org/records/15640174/files/glottolog/glottolog-cldf-v5.2.1.zip"
)


write_rds(glotto, "data/raw/glotto_database.rds")

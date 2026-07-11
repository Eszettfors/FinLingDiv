# FinLingDiv

### Description and code
This is the repository holding the code for creating the dataset: [FinLingDiv](https://zenodo.org/records/18257720), a dataset to study the spatial and temporal dynamics of linguistic diversity in Finland. The speaker data stems from statistics Finland and have been provided in `data/fin_lang_data_eng`. To replicate the construction of the datset, run the scripts in /processing/ASJP and /processing/Glottolog in the order of "fetch" and "process" to prepare the linguistic data. Then run diversity/data_prep_diversity/ to generate the datasets.

### The dataset
If you use the data, please cite the dataset at Zenodo as `Essfors, H. (2026). FinLingDiv (1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.18257720`

### Explore the data
We also publish a shiny app that can be used to explore the data. The app is hosted online at https://f39e09-hannes-essfors.shinyapps.io/FinLingDiv/, or you can run it locally from app.R

library(tidyverse)
library(stringdist)

# this script is set up to facilitate the calculation of similarity matrices through asjp

# read data asjp
df_asjp = read_csv("data/processed/asjp_wide.csv")

df_asjp = df_asjp %>%
  mutate(ISO6393 = ifelse(ISO6393 == "hbs", "srp", ISO6393))

get_concept_vector = function(ISO){
  # this function takes a language ISO code and returns a vector with concept values from ASJP
  if (!ISO %in% df_asjp$ISO6393){
    stop(error)
  }
  vec = df_asjp %>%
    filter(ISO6393 == ISO) %>%
    select(!c("lang_id", "ISO6393", "words", "completeness", "glottocode", "language", "macroarea", "family", "Latitude", "Longitude")) %>%
    t() %>%
    as.vector()
  return(vec)
}


get_mean_ldn_sim = function(v1, v2){
  # this function takes two language vectors with concepts and calculates the mean normalized levenshtein distance 
  # between them
  
  # get vector with normalized levenshtein distances
  ldn = stringdist(v1, v2, method = "lv") / pmax(nchar(v1), nchar(v2))
  
  # average ldn
  mean_ldn = mean(ldn, na.rm = TRUE)
  
  return(1 - mean_ldn)
}


get_ldn_sim_matrix = function(iso_vec){
  # function takes a vector of iso codes and returns a matrix with pairwise similarity
  
  sim_m = matrix(NA, ncol = length(iso_vec), nrow = length(iso_vec), dimnames = list(iso_vec, iso_vec))
  
  # populate upper triangl
  i = 1
  for (lang1 in iso_vec){
    for (lang2 in iso_vec[i:length(iso_vec)]){
      concept_vec_1 = get_concept_vector(lang1)
      concept_vec_2 = get_concept_vector(lang2)
      
      sim = get_mean_ldn_sim(concept_vec_1, concept_vec_2)
      
      sim_m[lang1, lang2] = sim
      sim_m[lang2, lang1] = sim
      
    }
    i = i + 1
  }
  return(sim_m)
}


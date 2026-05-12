library(tidyverse)
library(sf)
library(readr)
library(purrr)
library(stringr)
source("processing/ASJP/asjp_get_similarity.R")

# this script reads the datafiles in data, each with 5 years of data, merges and formats them to be read for analysis

data_path = "data/fin_lang_data_eng"
files = list.files(data_path)

# read files
df_list = list()
i = 1
for (file in files){
  df = read_csv(paste0(data_path, "/", file), locale = locale(encoding = "latin1"))
  df_list[[i]] = df
  i = i + 1
}

# remove unnecessary column
remove_columns = function(df){
  df = df %>%
    select(!c(Information))
  return(df)
}

rename_columns = function(df){
  df = df %>%
    rename("municipality" = Area,
           "language" = Language) %>%
    relocate(municipality, language)
  return(df)
}


df_list = df_list %>%
  lapply(remove_columns) %>%
  lapply(rename_columns)
  


# merge the dfs 
df = df_list[[1]] %>% 
  select(municipality, language)
for (i in 1:length(df_list)){
  df = df %>% 
    left_join(df_list[[i]], join_by(municipality, language))
}

# get sorted list of years
years = df %>%
  select(!c(municipality, language)) %>%
  colnames() %>%
  as.integer() %>%
  sort()

# turn year-columns into integers and replace NA with 0
df = df %>%
  mutate(across(.cols = as.character(years), as.integer)) %>%
  mutate(across(.cols = as.character(years), ~replace_na(., 0)))

# remove languages that are "other" or "unknown"
df = df %>%
  filter(!language %in% c("Unknown", "Other", "other", "unknown", "Other language"))


# remove alternative language names separated by ;
df = df %>%
  mutate(language = str_split(language, ";") %>% map_chr(1))

# add isocodes from statistic finland
iso_codes = read_delim("data/raw/language_iso.csv", delim = ";")
iso_codes = iso_codes %>%
  rename("ISO6393" = iso)

iso_codes %>%
  filter(ISO6393 == "kyr")
# adjust for Kyrgystan and tigrin

iso_codes = iso_codes %>%
  mutate(ISO6393 = case_when(ISO6393 == "kyr" ~ "kir",
                             ISO6393 == "tig" ~ "tir",
                             TRUE ~ ISO6393))


df = df %>%
  left_join(iso_codes, join_by(language))

# add language families and macroareas
glotto = read_csv("data/raw/glottolog.csv")

glotto_fam = glotto %>%
  filter(category == "Family") %>%
  select(glottocode, language) 

head(glotto_fam)

glotto = glotto %>%
  select(language, macroarea, glottocode, ISO6393, Latitude, Longitude, classification, family, aes) %>%
  filter(!is.na(ISO6393)) %>%
  rowwise() %>%
  mutate(family = str_split(classification, "/")[[1]][1])

df = glotto %>% 
  select(!language) %>%
  right_join(df, join_by(ISO6393))


# add family name
df = df %>%
  left_join(glotto_fam %>%
              rename("family_name" = language), 
            join_by("family" == "glottocode")) %>%
  relocate(ISO6393, glottocode, language, classification, family, family_name, municipality)


# pivot_long
df_long = df %>%
  pivot_longer(cols = as.character(years),
               names_to = "year",
               values_to = "speakers") %>%
  arrange(municipality, year, -speakers)

# iso code with multiple language names ?
df_long %>% distinct(ISO6393) # 161
df_long %>% distinct(language) # 164

df_long %>%
  distinct(ISO6393, language) %>%
  count(ISO6393) %>%
  filter(n > 1)

df %>%
  filter(ISO6393 %in% c("aka", "hbs", "ron"))

# aka, hbs, ron
df_long %>%
  filter(ISO6393 == "aka") %>%
  distinct(language) # go for akan
df_long %>% # go for akan
  filter(ISO6393 == "hbs") %>%
  distinct(language) # go for serbian

df_long %>%
  filter(ISO6393 == "ron") %>%
  distinct(language) # go for romanian

df_long = df_long %>%
  mutate(language = case_when(ISO6393 == "aka" ~ "Akan",
                                   ISO6393 == "ron" ~ "Romanian",
                                   ISO6393 == "hbs" ~ "Serbian",
                                   TRUE ~ language)) %>%
  mutate(ISO6393 = ifelse(language == "Serbian", "srp", ISO6393))

# adjust speaker
df_long = df_long %>%
  group_by(municipality, year, ISO6393) %>%
  summarize(glottocode = unique(glottocode),
            language = unique(language),
            classification = unique(classification),
            family = unique(family),
            family_name = unique(family_name),
            aes = unique(aes),
            macroarea = unique(macroarea),
            Latitude = unique(Latitude),
            Longitude = unique(Longitude),
            speakers = sum(speakers)) %>%
  ungroup()

# are there languages that never have any speakers?
df_long %>%
  group_by(language) %>%
  summarize(speakers = sum(speakers)) %>%
  filter(speakers == 0)

# there are 52 cases where there are never any speakers registered. Looking at statistik centralen, they only
# show up for the entire country, not single municipalities. see e.g. Abkhaz. It could be that the persons are not registered
# in a municipality.ﬁ

# remove instances with 0 speakers
df_long = df_long %>%
  filter(speakers != 0)

# reorder
df_long = df_long %>%
  relocate(municipality, year, language, ISO6393, family, macroarea, aes, speakers) %>%
  arrange(municipality, year)

# check for NA
# are there any NAs?
colSums(is.na(df_long))

df_long %>%
  distinct(language, ISO6393, family_name, macroarea, aes) %>%
  filter(is.na(family_name))

# bos, eop, hrv, lvs, nbl -> adjust manually
df_long = df_long %>% 
  mutate(family_name = case_when(ISO6393 %in% c("esp", "ido") ~ "Artificial",
                            ISO6393 == "eus" ~ "Isolate",
                            TRUE ~ family_name))

colSums(is.na(df_long))

# aes
df_long %>%
  distinct(language, ISO6393, aes)

# fix wrong aes
df_long = df_long %>%
  mutate(aes = case_when(ISO6393 %in% c("pol", "bos", "hrv", "vie") ~ 1,
                         ISO6393 == "ido" ~ 1,
                         TRUE ~ aes))

# lat longitude missing
df_long %>%
  filter(is.na(Latitude))
df_long = df_long %>%
  mutate(Latitude = ifelse(ISO6393 == "ido", 49.33, Latitude),
         Longitude = ifelse(ISO6393 == "ido", 2.81, Longitude))

nat_ts = df_long %>%
  group_by(language, ISO6393, year) %>%
  summarize(speakers = sum(speakers))

# write raw data
write_csv(df_long, "data/processed/full_time_series_speakers.csv")
write_csv(nat_ts, "data/processed/national_time_series_speakers.csv")

# generate similarity measures ----------------

df_long %>%
  filter(!ISO6393 %in% df_asjp$ISO6393) %>%
  distinct(ISO6393)

# remove ido
df_long = df_long %>%
  filter(!ISO6393 == "ido")

# pull langs from data
langs = df_long %>%
  distinct(ISO6393) %>%
  filter(ISO6393 %in% df_asjp$ISO6393) %>%
  pull()

# generate a similarity matrix based on the languages

sim_m = get_ldn_sim_matrix(langs)


# generate diversity
# for each year, generate q = 0, q = 1 and q = 2 for each municipality both naive and non naive

get_prop_vec = function(speakers){
  #takes a vector with language frequencies and turns it into a proportion vector
  prop_vec = speakers / sum(speakers)
  return(prop_vec)
}

get_richness = function(speakers){
  # takes a vector with speakers per language and calculates the richness
  speakers = speakers[speakers != 0]
  richness = length(speakers)
  return(richness)
}

get_exp_shannon = function(speakers){
  # takes a vector of proportions and calculates the exponent shannon entropy
  
  prop_vec = get_prop_vec(speakers)
  prop_vec = prop_vec[prop_vec != 0]
  log_vec = log(prop_vec)
  entropy = -sum(prop_vec*log_vec)
  return(exp(entropy))
}

get_inv_simp = function(prop_vec){
  # takes a vector of proportions and calculates the inverse simpson
  
  prop_vec = get_prop_vec(prop_vec)
  prop_vec = prop_vec[prop_vec != 0]
  squared_prop = prop_vec*prop_vec
  inv_simp = 1/sum(squared_prop)
  
  return(inv_simp)
}


get_shannon_diversity = function(speakers, sim_m){
  # calculates diversity for q = 1 ergo shannon given a vector with proportions
  prop_vec = get_prop_vec(speakers)
  
  # for each proportion, get the expected similarity to all other proportions
  expected = log(sim_m %*% prop_vec)
  
  # for each proportion, multiply by expected similarity to all other proportions
  # and derive entropy
  E = -1 * sum(prop_vec * expected)
  
  # exponentiate entropy to get diversity
  D = exp(E)
  
  return(D)
}


get_diversity_q = function(speakers, sim_m, q = 0){
  # a general function to implement diversity for any q
  
  prop_vec = get_prop_vec(speakers)
  
  # to avoid division with zero, implement shannon diversity as a special case
  if (q == 1){
    return(get_shannon_diversity(prop_vec, sim_m))
  }
  
  # get expected similarity to all other prop for each proportion
  expected = sim_m %*% prop_vec
  
  # raise the expected similarity to the power of q-1
  expected_order = expected^(q-1)
  
  # multiply the expected similarity with each proportion and take the reciprocal
  D = (sum(prop_vec * expected_order))^(1/(1-q))
  
  return(D)
}


get_naive_diversity_q = function(prop_vec, q = 0){
  # a general function to implement naive diversity for any q
  
  I = diag(length(prop_vec))
  
  # get diversity
  D = get_diversity_q(prop_vec, I, q)
  
  return(D)
}

### calculate diversities --------

# div index for each municipality
df_div = df_long %>%
  group_by(municipality, year) %>%
  summarize(richness = get_richness(speakers),
            exp_shannon = get_exp_shannon(speakers),
            inv_simpson = get_inv_simp(speakers),
            lex_div_q_0 = get_diversity_q(speakers, sim_m[ISO6393, ISO6393], q = 0),
            lex_div_q_1 = get_diversity_q(speakers, sim_m[ISO6393, ISO6393], q = 1),
            lex_div_q_2 = get_diversity_q(speakers, sim_m[ISO6393, ISO6393], q = 2))


# div index for whole of finland
div_fin = df_long %>%
  group_by(language, ISO6393, year) %>%
  summarize(speakers = sum(speakers)) %>%
  group_by(year) %>%
  summarize(richness = get_richness(speakers),
            exp_shannon = get_exp_shannon(speakers),
            inv_simpson = get_inv_simp(speakers),
            lex_div_q_0 = get_diversity_q(speakers, sim_m[ISO6393, ISO6393], q = 0),
            lex_div_q_1 = get_diversity_q(speakers, sim_m[ISO6393, ISO6393], q = 1),
            lex_div_q_2 = get_diversity_q(speakers, sim_m[ISO6393, ISO6393], q = 2))


# write data
write_csv(df_div, "data/processed/diversity_time_series.csv")
write_csv(div_fin, "data/processed/diversity_finland_time_series.csv")


#### fix geo data
geodata = read_sf("data/geodata", options = "ENCODING=latin1") %>%
  select(mncplty, geometry) %>%
  rename("municipality" = mncplty)


geo_munc = geodata %>% 
  select(municipality) %>%
  as.data.frame() %>% 
  select(municipality)

div_munc = df_div %>% 
  distinct(municipality) %>%
  pull()

geo_munc %>%
  filter(!municipality %in% div_munc)

# Pertunmaa is nowadays part of mäntyharju. Raasepori is Raseborg and Vöyri är Vörå

# edit name, finish -> swedish, Pertunmaa -> mäntyharju
geodata = geodata %>%
  mutate(municipality = case_when(municipality == "Raasepori" ~ "Raseborg",
                                 municipality == "Vöyri" ~ "Vörå",
                                 municipality == "Pertunmaa" ~ "Mäntyharju",
                                 TRUE ~ municipality))
# merge the polygons of mäntyharju and pertunmaa
geodata = geodata %>%
  group_by(municipality) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

# write the data
st_write(geodata, "data/geodata/kunta4500k_2022Polygon.shp", delete_dsn = TRUE)







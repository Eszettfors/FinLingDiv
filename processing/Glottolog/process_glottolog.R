library(tidyverse)
glotto = read_rds("data/raw/glotto_database.rds")


#languages
glotto_langs = glotto$tables$LanguageTable %>%
  select(ID, Name, Macroarea, Glottocode, ISO639P3code, Is_Isolate, Latitude, Longitude)

#classifications
classifications = glotto$tables$ValueTable %>%
  select(Language_ID, Parameter_ID, Value) %>%
  pivot_wider(names_from = Parameter_ID, values_from = Value)

# join
glotto = glotto_langs %>%
  left_join(classifications, join_by("ID" == "Language_ID"))


glotto %>%
  filter(Is_Isolate == TRUE) %>%
  nrow() # 182 isolates

# add isolate as a value
glotto = glotto %>%
  mutate(classification = case_when(Is_Isolate == TRUE ~ "Isolate",
                           TRUE ~ classification)) %>%
  rowwise() %>%
  mutate(classification = case_when(classification == "Isolate" ~ paste0("Isolate", Glottocode),
                                    TRUE ~ classification))

glotto = glotto %>%
  select(!Is_Isolate) %>%
  rename("ISO6393" = ISO639P3code, "macroarea" = Macroarea, "language" = Name, "glottocode" = Glottocode)

#### add toplevel family
glotto = glotto %>%
  mutate(family = str_split(classification, "/")[[1]][1]) %>% 
  left_join(glotto %>%
              select(glottocode, language) %>%
              rename("family_name" = language), join_by("family" == "glottocode")) %>%
  select(!family) %>%
  rename("family" = family_name)

write_csv(glotto, "data/processed/glottolog.csv")

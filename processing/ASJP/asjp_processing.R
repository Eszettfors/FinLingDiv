library(tidyverse)

# this script processes the ASJP database to make it ready for analysis. It joins the necessary column and pivots
# it wide with all lexical concepts as columns with one row per data entry.

asjp = readRDS("data/raw/asjp_database.rds")

# extract relevant tables
tabs = asjp$tables

form_tab = tabs[1][[1]]
lang_tab = tabs[2][[1]]
param_tab = tabs[3][[1]]


# subset to relevant language columns
langs = lang_tab %>% 
  select(ID, ISO639P3code, Glottocode, Glottolog_Name, Macroarea, Family, Latitude, Longitude) 


# subset to relevant value columns
values = form_tab %>%
  select(ID, Language_ID, Parameter_ID, Value)

params = param_tab %>%
  select(ID, Concepticon_Gloss)

df = values %>% 
  left_join(params, join_by("Parameter_ID" == "ID")) %>%
  left_join(langs, join_by("Language_ID" == "ID")) %>% 
  select(-ID, -Parameter_ID)


# filter to only have the 40 word list
swadesh_40 = c(
  "I",
  "THOU",
  "WE",
  "ONE",
  "TWO",
  "PERSON",
  "FISH",
  "DOG",
  "LOUSE",
  "TREE",
  "LEAF",
  "SKIN",
  "BLOOD",
  "BONE",
  "HORN (ANATOMY)",
  "EAR",
  "EYE",
  "NOSE",
  "TOOTH",
  "TONGUE",
  "KNEE",
  "HAND",
  "BREAST",
  "LIVER",
  "DRINK",
  "SEE",
  "HEAR",
  "DIE",
  "COME",
  "SUN",
  "STAR",
  "WATER",
  "STONE",
  "FIRE",
  "PATH",
  "MOUNTAIN",
  "NIGHT",
  "FULL",
  "NEW",
  "NAME"
)

df = df %>%
  filter(Concepticon_Gloss %in% swadesh_40)

completeness = 
  df %>% 
  group_by(Language_ID) %>%
  distinct(Concepticon_Gloss, ISO639P3code) %>%
  summarize(words = n()) %>% mutate(
    completeness = words / 40 * 100
  )


# restructure to one ID per row, with concepts as columns

# take into account: One ISO might be represented by multiple variants: 
# one concept might have multiple entries e.g. swedish en/ett

# if multiple values exist, take the first one
df_wide = df %>%
  pivot_wider(names_from = Concepticon_Gloss, values_from = Value, values_fn = ~first(.x))

head(df_wide)

df_wide = df_wide %>% left_join(
  completeness, join_by(Language_ID == Language_ID)) %>%
  relocate(Language_ID, ISO639P3code, words, completeness)

df_wide = df_wide %>% 
  rename("lang_id" = Language_ID, "ISO6393" = ISO639P3code, "glottocode" = Glottocode, "language" = Glottolog_Name, "macroarea" = Macroarea, "family" = Family)

# discovered error with ASJP wrongly giving Jamtska SWE as iso-code
df_wide = df_wide %>%
  mutate(ISO6393 = case_when(
    lang_id == "JAMTLANDIC" ~ "jmk",
    TRUE ~ ISO6393,
  ), language = case_when(
      lang_id == "JAMTLANDIC" ~ "Jamtlandic",
      TRUE ~ language
      ), glottocode = case_when(
        lang_id == "JAMTLANDIC" ~ "jamt1238",
        TRUE ~ glottocode))

# if multiple iso per language_ID -> keep the one with the most completeness and if there are multiple
# pick the first one -> # ISO are unique identifiers together with lang_id

# cases with no iso?
df_wide %>%
  filter(is.na(ISO6393)) #397

df_wide = df_wide %>%
  filter(!is.na(ISO6393))


# change ISO codes to match with target data
df_wide = df_wide %>%
  mutate(ISO6393 = case_when(
    ISO6393 == "hbs" ~ "srp",
    TRUE ~ ISO6393))


# some languages have more than one entry. Some cases the main variant won't be selected by going by completeness: mandarin, arabic, dutch, and turkish -> manual selection
turkish_variants_wrong = df_wide %>%
  filter(ISO6393 == "tur") %>%
  filter(!lang_id == "TURKISH") %>%
  pull(lang_id)

chinese_variants_wrong = df_wide %>%
  filter(ISO6393 == "cmn") %>%
  filter(!lang_id == "MANDARIN") %>%
  pull(lang_id)

arabic_variants_wrong = df_wide %>%
  filter(ISO6393 == "arb") %>%
  filter(!lang_id == "STANDARD_ARABIC_2") %>%
  pull(lang_id)

russian_variants_wrong = df_wide %>%
  filter(ISO6393 == "rus") %>%
  filter(!lang_id != "RUSSIAN")

dutch_variant_wrong = df_wide %>%
  filter(ISO6393 == "nld") %>%
  filter(!lang_id == "DUTCH") %>%
  pull(lang_id)

variants_to_filter = c(turkish_variants_wrong, chinese_variants_wrong, arabic_variants_wrong, dutch_variant_wrong)

df_wide = df_wide %>%
  filter(!lang_id %in% variants_to_filter)


df_wide = df_wide %>%
  group_by(ISO6393) %>% slice_max(completeness, n = 1) %>%
  group_by(ISO6393) %>% slice_head(n = 1)

# handle special signs
#2x juxtaposition = ~ -> remove + previous letter
#3x juxtaposition = $ -> remove + two previous letters
# " = glottolozation -> simply remove
# * = nasalization -> simply remove
#and https://www.researchgate.net/publication/43336388_Sound_Symbolism_in_Basic_Vocabulary
param_cols = df_wide %>% 
  ungroup() %>%
  select(!c(lang_id, ISO6393, words, completeness, language, macroarea, family)) %>% colnames()

df_wide = df_wide %>%
  mutate(across(param_cols, ~ gsub("\\*", "", .x))) %>%
  mutate(across(param_cols, ~ gsub('\\"', "", .x))) %>%
  mutate(across(param_cols, ~ gsub("..\\$", "", .x))) %>%
  mutate(across(param_cols, ~ gsub(".\\~", "", .x)))

head(df_wide)

# write 
write_csv(df_wide, "data/processed/asjp_wide.csv")



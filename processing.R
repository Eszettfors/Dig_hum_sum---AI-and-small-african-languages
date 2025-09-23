library(readr)
library(tidyverse)
library(stringr)
library(lingtypology)

df_hf = read_csv("data/raw/hugging_face_language_data.csv")

# extract english name
df_hf = df_hf %>%
  rowwise() %>%
  mutate(language = str_split(language, "\n")[[1]][1]) %>%
  select(!datasets)

nrow(df_hf) # 4692


##### fix iso ######
iso_codes = read_tsv("data/raw/iso-639-3.tab")
iso_codes = iso_codes %>%
  select(Id, Part1, Ref_Name) %>%
  rename("ISO6393" = Id, 
         "ISO6391" = Part1,
         "language" = Ref_Name)


# group by languages and merge
df_hf = df_hf %>% 
  group_by(language) %>%
  summarize(iso = first(iso),
            models = sum(models))

nrow(df_hf) #4536

lang_join = df_hf %>%
  left_join(iso_codes, join_by(language == language))


missed_langs = lang_join %>%
  filter(is.na(ISO6393))

missed_langs %>%
  print(n = 40)

lang_join = lang_join %>%
  mutate(ISO6393 = case_when(iso == "ab" ~  "abk",
                             iso == "bh" ~ "bhi",
                             iso == "div" ~ "div",
                             iso == "dgr" ~ "dgr",
                             iso == "haa" ~ "haa",
                             iso == "kci" ~ "kci",
                             iso == "kwk" ~ "kwk",
                             iso == "zmp" ~ "zmp",
                             iso == "new" ~ "new",
                             iso == "nya" ~ "nya",
                             iso == "ff" ~ "ful",
                             iso == "el" ~ "ell",
                             iso == "gn" ~ "grn",
                             iso == "ia" ~ "ina",
                             iso == "rn" ~ "run",
                             iso == "kj" ~ "kua",
                             iso == "ky" ~ "kir",
                             iso == "li" ~ "lim",
                             iso == "ms" ~ "zsm",
                             iso == "mi" ~ "mri",
                             iso == "ne" ~ "nep",
                             iso == "nd" ~ "nde",
                             iso == "ii" ~ "iii",
                             iso == "oc" ~ "oci",
                             iso == "oj" ~ "ojb",
                             iso == "cu" ~ "chu",
                             iso == "or" ~ "ory",
                             iso == "ps" ~ "pbt",
                             iso == "pi" ~ "pli",
                             iso == "nr" ~ "nbl",
                             iso == "sw" ~ "swa",
                             iso == "to" ~ "ton",
                             iso == "ug" ~ "uig",
                             TRUE ~ ISO6393))

lang_join %>%
  filter(is.na(ISO6393)) # no NA

df_hf = lang_join %>%
  group_by(ISO6393) %>%
  summarize(language = first(language),
            models = sum(models))



# adress macrolanguages
df_hf = df_hf %>%
  mutate(ISO6393 = case_when(ISO6393 == "zho" ~ "cmn",
                             ISO6393 == "ara" ~ "arb",
                             TRUE ~ ISO6393)) %>%
  group_by(ISO6393) %>%
  summarize(language = first(language),
            models = sum(models))

nrow(df_hf) # 4507 langs


###### adding macroarea
glotto = lingtypology::glottolog


glotto = glotto %>%
  select(iso, latitude, longitude, area)

df_hf = df_hf %>%
  left_join(glotto, join_by("ISO6393" == "iso"))

df_hf %>%
  filter(is.na(area))

df_hf = df_hf %>%
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude))

df_hf = df_hf %>%
  mutate(area = case_when(ISO6393 == "ara" ~ "Eurasia",
                          ISO6393 == "aze" ~ "Eurasia",
                          ISO6393 == "bal" ~ "Eurasia",
                          ISO6393 == "bik" ~ "Papunesia",
                          ISO6393 == "est" ~ "Eurasia",
                          ISO6393 == "etc" ~ "North America",
                          ISO6393 == "fas" ~ "Eurasia",
                          ISO6393 == "gba" ~ "Africa",
                          ISO6393 == "grn" ~ "South America",
                          ISO6393 == "hmn" ~ "Eurasia",
                          ISO6393 == "iku" ~ "North America",
                          ISO6393 == "kau" ~ "Africa",
                          ISO6393 == "kok" ~ "Eurasia",
                          ISO6393 == "ori" ~ "Eurasia",
                          ISO6393 == "kon" ~ "Eurasia",
                          ISO6393 == "kpe" ~ "Africa",
                          ISO6393 == "lah" ~ "Eurasia",
                          ISO6393 == "luy" ~ "Africa",
                          ISO6393 == "mlg" ~ "Africa",
                          ISO6393 == "msa" ~ "Papunesia",
                          ISO6393 == "nwx" ~ "Eurasia",
                          ISO6393 == "oji" ~ "North America",
                          ISO6393 == "orm" ~ "Africa",
                          ISO6393 == "que" ~ "South America",
                          ISO6393 == "raj" ~ "Eurasia",
                          ISO6393 == "srd" ~ "Eurasia",
                          ISO6393 == "swa" ~ "Africa",
                          ISO6393 == "syr" ~ "Eurasia",
                          ISO6393 == "yid" ~ "Eurasia",
                          ISO6393 == "zho" ~ "Eurasia",
                          TRUE ~ area),
         latitude = case_when(ISO6393 == "ara" ~ "27.96",
                          ISO6393 == "aze" ~ "40.98",
                          ISO6393 == "bal" ~ "29.38",
                          ISO6393 == "bik" ~ "13.20",
                          ISO6393 == "est" ~ "58.55",
                          ISO6393 == "etc" ~ "47.39",
                          ISO6393 == "fas" ~ "32.90",
                          ISO6393 == "gba" ~ "4.80",
                          ISO6393 == "grn" ~ "25.61",
                          ISO6393 == "hmn" ~ "25.60",
                          ISO6393 == "iku" ~ "62.17",
                          ISO6393 == "kau" ~ "11.80",
                          ISO6393 == "kok" ~ "15.27",
                          ISO6393 == "ori" ~ "18.10",
                          ISO6393 == "kon" ~ "-5.25",
                          ISO6393 == "kpe" ~ "7.93",
                          ISO6393 == "lah" ~ "30.09",
                          ISO6393 == "luy" ~ "-15.01",
                          ISO6393 == "mlg" ~ "-19.59",
                          ISO6393 == "msa" ~ "3.09",
                          ISO6393 == "nwx" ~ "27.67",
                          ISO6393 == "oji" ~ "46.57",
                          ISO6393 == "orm" ~ "8.81",
                          ISO6393 == "que" ~ "-14.09",
                          ISO6393 == "raj" ~ "24.57",
                          ISO6393 == "srd" ~ "39.06",
                          ISO6393 == "swa" ~ "-8.26",
                          ISO6393 == "syr" ~ "37.11",
                          ISO6393 == "yid" ~ "51.75",
                          ISO6393 == "zho" ~ "40.02",
                          TRUE ~ latitude),
         longitude = case_when(ISO6393 == "ara" ~ "43.85",
                              ISO6393 == "aze" ~ "46.47",
                              ISO6393 == "bal" ~ "69.06",
                              ISO6393 == "bik" ~ "123.49",
                              ISO6393 == "est" ~ "25.82",
                              ISO6393 == "etc" ~ "-77.53",
                              ISO6393 == "fas" ~ "53.30",
                              ISO6393 == "gba" ~ "14.61",
                              ISO6393 == "grn" ~ "-57.09",
                              ISO6393 == "hmn" ~ "106.40",
                              ISO6393 == "iku" ~ "-75.61",
                              ISO6393 == "kau" ~ "13.13",
                              ISO6393 == "kok" ~ "74.21",
                              ISO6393 == "ori" ~ "82.85",
                              ISO6393 == "kon" ~ "15.50",
                              ISO6393 == "kpe" ~ "-8.99",
                              ISO6393 == "lah" ~ "75.35",
                              ISO6393 == "luy" ~ "22.67",
                              ISO6393 == "mlg" ~ "47.12",
                              ISO6393 == "msa" ~ "101.70",
                              ISO6393 == "nwx" ~ "85.33",
                              ISO6393 == "oji" ~ "-84.15",
                              ISO6393 == "orm" ~ "36.74",
                              ISO6393 == "que" ~ "-71.77",
                              ISO6393 == "raj" ~ "73.68",
                              ISO6393 == "srd" ~ "9.04",
                              ISO6393 == "swa" ~ "37.62",
                              ISO6393 == "syr" ~ "42.14",
                              ISO6393 == "yid" ~ "19.42",
                              ISO6393 == "zho" ~ "116.23",
                              TRUE ~ longitude))


# filter cases with does 
df_hf %>%
  filter(is.na(latitude)) %>%
  print(n = 30)

df_hf = df_hf %>%
  filter(!is.na(latitude))


df_hf = df_hf %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>%
  rename("macroarea" = area,
         "RTC" = models)

write_csv(df_hf, "data/processed/hugging_face_models.csv")





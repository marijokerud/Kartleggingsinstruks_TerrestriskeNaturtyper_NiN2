library(readxl)
library(openxlsx)
library(tidyverse)


Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"



#GET DATA
nin.types <-read_excel(path = "Data/Naturtyper_eksport_full_med_naturtype_kode_07.10.2025.xlsx", sheet = "NIN-typer", col_names = TRUE)
Tilstand.raw <-read_excel(path = "Data/Naturtyper_eksport_full_med_naturtype_kode_07.10.2025.xlsx", sheet = "Tilstand", col_names = TRUE)
Variabel_oversettelse <-read_excel(path = "Data/Naturtyper_eksport_full_med_naturtype_kode_07.10.2025.xlsx", sheet = "Variabel_oversettelse", col_names = TRUE)

nin.types
kartleggingsinstruksen <- nin.types %>% 
  mutate(Kartleggingsmålestokk = str_replace(Kartleggingsmålestokk, "1:20.000000", "1:20.000")) %>% 
  #Seperate values in column Sone to multiple rows
  mutate(.sone_items = stringr::str_extract_all(Sone, "(?<=\\()[^)]+(?=\\))")) %>%
  # one row per extracted item (keeps other columns duplicated)
  tidyr::unnest_longer(.sone_items, values_to = "Sone_clean", keep_empty = TRUE) %>%
  # trim and drop empties (rows with no parentheses are dropped; remove this filter to keep them)
  mutate(Sone_clean = stringr::str_squish(Sone_clean)) %>% 
  distinct() %>% 
  select(Naturtype_kode, Naturtype, Kartleggingsenheter, Sone_clean) %>% 
  rename(Sone = Sone_clean) %>% 
  drop_na(Kartleggingsenheter)

# Save table as excel file
write.xlsx(as.data.frame(kartleggingsinstruksen), file = "Naturtyper_informasjon.xlsx", sheetName = "NIN-typer", col.names = TRUE, row.names = TRUE, append = FALSE)




###### TILSTAND ######
Tilstand <- Tilstand.raw %>% 
  #Seperate values in column Variabel to multiple rows
  mutate(.variabel_items = stringr::str_extract_all(Variabel, "(?<=\\()[^)]+(?=\\))")) %>%
  # one row per extracted item (keeps other columns duplicated)
  tidyr::unnest_longer(.variabel_items, values_to = "Variabel_clean", keep_empty = TRUE) %>%
  # trim and drop empties (rows with no parentheses are dropped; remove this filter to keep them)
  mutate(Variabel_clean = stringr::str_squish(Variabel_clean)) %>% 
  filter(Variabel_clean %in% c(
    "7GR-GI",
    "7JB-BA",
    "7JB-BT",
    "MdirPRSL",
    "MdirPRTK" 
  )) 
  
write.xlsx(as.data.frame(kartleggingsinstruksen), file = "Naturtyper_informasjon.xlsx", sheetName = "Tilstand", col.names = TRUE, row.names = TRUE, append = FALSE)
write

#GET DATA
nin.types <-read_excel(path = "Data/Naturtyper_eksport_full_02.10.2025_med_naturtype_kode.xlsx", sheet = "NIN-typer", col_names = TRUE)
andre.variabler <-read_excel(path = "Data/Sone_andre_variabler.xlsx", sheet = "NIN-typer", col_names = TRUE)
#Tilstand.raw <-read_excel(path = "Data/Naturtyper_eksport_full_02.10.2025_med_naturtype_kode.xlsx", sheet = "Tilstand", col_names = TRUE)
#Naturmangfold.raw <-read_excel(path = "Data/Naturtyper_eksport_full_02.10.2025_med_naturtype_kode.xlsx", sheet = "Naturmangfold", col_names = TRUE)

nin.types
nin2 <- nin.types %>% 
  mutate(Kartleggingsmålestokk = str_replace_all(Kartleggingsmålestokk, "1:\\s*", "1:"),
         Kartleggingsmålestokk = str_replace(Kartleggingsmålestokk, "1:20\\.", "1:20.000")) %>% 
  mutate(Kartleggingsenheter = str_remove_all(Kartleggingsenheter, "\\*")) %>% 
  mutate(Kartleggingsenheter = str_split(Kartleggingsenheter, ",")) %>%
  rowwise() %>%
  mutate(
    prefix = str_extract(Kartleggingsenheter[1], "^.*-"),
    Kartleggingsenheter = list(
      paste0(prefix, str_remove(Kartleggingsenheter, prefix))
    )
  ) %>%
  unnest(Kartleggingsenheter) %>%
  select(-prefix) %>% 
  mutate(Hovedtyper = str_replace_all(Hovedtyper, ", —", ""),
         Hovedtyper = str_replace_all(Hovedtyper, " —", ""))


#Seperate values in column Sone to multiple rows
andre.variabler2 <- andre.variabler %>% 
  mutate(.sone_items = stringr::str_extract_all(Sone, "(?<=\\()[^)]+(?=\\))")) %>%
  # one row per extracted item (keeps other columns duplicated)
  tidyr::unnest_longer(.sone_items, values_to = "Sone_clean", keep_empty = TRUE) %>%
  # trim and drop empties (rows with no parentheses are dropped; remove this filter to keep them)
  mutate(Sone_clean = stringr::str_squish(Sone_clean)) %>% 
  distinct() %>% 
  select(Naturtype_kode, Naturtype, Kartleggingsenheter, Sone_clean)

kartleggingsinstruksen <- nin2 %>% 
  left_join(andre.variabler2) 


# Save table as excel file
write.xlsx(as.data.frame(kartleggingsinstruksen), file = "Naturtyper_informasjon.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)


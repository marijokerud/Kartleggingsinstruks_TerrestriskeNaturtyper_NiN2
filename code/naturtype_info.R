library(readxl)
library(openxlsx)
library(tidyverse)


Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

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


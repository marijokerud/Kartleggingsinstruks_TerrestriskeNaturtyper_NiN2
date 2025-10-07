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
  

path <- "Data/Naturtyper_informasjon.xlsx"     # existing file
new_sheet <- "Tilstand"    # name of the new sheet
df <- Tilstand             # data to write

wb <- loadWorkbook(path)                 # open existing workbook
addWorksheet(wb, new_sheet)              # add a new sheet
writeData(wb, new_sheet, df)             # write data
saveWorkbook(wb, path, overwrite = TRUE) # save back to same file


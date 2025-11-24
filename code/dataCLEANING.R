library(readxl)     # if you load from xlsx
library(openxlsx)   # for write.xlsx if needed

#create a numeric column for tilstand and naturmangfold
data_clean1 <- dat2_long_4 %>%
  rename(naturmangfold_orig = naturmangfold) %>% 
  rename(tilstand_orig = tilstand) %>% 
  mutate(naturmangfold = as.numeric(substr(naturmangfold_orig, 1, 1))) %>% 
  mutate(tilstand = as.numeric(substr(tilstand_orig, 1, 1))) %>% 
  #select(-NiN_variable_code, -NiN_variable_value) %>% 
  distinct()
data_clean1


#identify duplicates
dup.naturtype <- data_clean1 |>
  dplyr::summarise(n = dplyr::n(), .by = c(identifikasjon_lokalId, hovedokosystem, kartleggingsar, lokalitetskvalitet, mosaikk, naturmangfold_orig, naturtype,
                                           naturtypekode_short, naturtype_full, tilstand_orig, region, km2, m2, maned, oppdragstaker, naturmangfold, tilstand, 
                                           NiN_variable_code)) |>
  dplyr::filter(n > 1L)


#remove duplicates
data_clean2 <- data_clean1 |>
  group_by(across(-NiN_variable_value)) %>%                 # group by all other columns
  filter(!(is.na(NiN_variable_value) & any(!is.na(NiN_variable_value)))) %>%
  ungroup()

n_removed <- nrow(data_clean1) - nrow(data_clean2)
n_removed

dup2.naturtype <- data_clean2 |>
  dplyr::summarise(n = dplyr::n(), .by = c(identifikasjon_lokalId, hovedokosystem, kartleggingsar, lokalitetskvalitet, mosaikk, naturmangfold_orig, naturtype,
                                           naturtypekode_short, naturtype_full, tilstand_orig, region, km2, m2, maned, oppdragstaker, naturmangfold, tilstand, 
                                           NiN_variable_code)) |>
  dplyr::filter(n > 1L)  

#remove a duplicate row in: 
data_clean3 <- data_clean2 %>%
  filter(
    !(identifikasjon_lokalId == "NINFP2110037887" &
        NiN_variable_code == "PRAK" &
        NiN_variable_value %in% c(0, "0"))   # handle char or numeric 0
  )


nin.all <-read_excel(path = "Data/Alle_NiN_variabler.xlsx", sheet = "Alle", col_names = TRUE)
nin.all <- nin.all %>% 
  select(-naturtypekode_short)

#the data contains variables which are defining nature type and not used in the quality assessment. Remove these variables from data set.
data_clean4 <- data_clean3 %>%
  left_join(nin.all, by = "NiN_variable_code") %>% 
  filter(Variable_type == "Tilstand" | 
         Variable_type == "Naturmangfold") %>% 
  filter(NiN_variable_code != "4DG-0" |
           naturtypekode_short == "C20") %>% 
  filter(NiN_variable_code != "4DL-0" |
           naturtypekode_short %in% c("C20", "C21", "E11_05") ) %>% 
  filter(NiN_variable_code != "7SD-0" |
           naturtypekode_short %in% c("C11_01", "C12_01", "C13", "C14", "C21", "C22", "E11_01") ) %>% 
  filter(NiN_variable_code != "7SD-NS" |
           naturtypekode_short %in% c("C10", "C11_01", "C11_02", "C11_03", "C11_04", "C11_05", "C12_01", "C12_02", "C12_03", "C12_04", "C12_05", "C13", "C14", "C22", "E11_01") )

#drop rows where tilstand = 0, svært redusert 
data_clean5 <- data_clean4 %>%
  filter(!tilstand == "0" )

###### Found out that nature type C01_Hule eiker in 2018 was classified as Semi-naturligMark in 2018, changing all C01 to HulEik.
###### Seperate old forrest from rest
###### Seperate torvmarksformer from rest
data_clean6 <- data_clean5 %>%
  mutate(hovedokosystem = if_else(
    naturtypekode_short == "C01" & hovedokosystem == "Semi-naturligMark",
    "Skog",
    hovedokosystem)
  ) %>% 
  mutate(hovedokosystem_old = hovedokosystem) %>% 
  mutate(hovedokosystem = if_else(
    naturtypekode_short == "C01" & hovedokosystem == "Skog",
    "HulEik",
    hovedokosystem)
  ) %>% 
  mutate(hovedokosystem = if_else(
    naturtypekode_short %in% c("C11_01", "C11_02", "C11_03", "C11_04", "C11_05", "C12_01", "C12_02", "C12_03", "C12_04", "C12_05") 
    & hovedokosystem == "Skog",
    "GammelSkog",
    hovedokosystem)
  ) %>% 
  mutate(hovedokosystem = if_else(
    naturtypekode_short %in% c("E15", "E15_01", "E15_01_01", "E16") 
    & hovedokosystem == "Våtmark",
    "Semi-naturligMyr",
    hovedokosystem)) %>% 
  mutate(hovedokosystem = if_else(
    naturtypekode_short %in% c("E11_01","E11_02", "E11_03", "E11_04", "E11_05", "E14_01", "E14_02", "E14_03") 
    & hovedokosystem == "Våtmark",
    "Våtmarkskog",
    hovedokosystem))



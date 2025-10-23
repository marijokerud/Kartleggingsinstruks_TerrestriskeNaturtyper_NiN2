###### Found out that nature type C01_Hule eiker was classified as Semi-naturligMark in 2018, changing to Skog.
###### And change Høstingsskog (C02) to semi-naturlig
data_clean1 <- dat2_long_4 %>%
  mutate(hovedokosystem = if_else(
    naturtypekode_short == "C01" & hovedokosystem == "Semi-naturligMark",
    "Skog",
    hovedokosystem)
  ) %>% 
  mutate(hovedokosystem = if_else(
    naturtypekode_short == "C02" & hovedokosystem == "Skog",
    "Semi-naturligMark",
    hovedokosystem)
  )

data_clean2 <- data_clean1 %>% 
  mutate(Naturmangfold= as.numeric(substr(naturmangfold, 1, 1))) %>% 
  mutate(Tilstand= as.numeric(substr(tilstand, 1, 1))) %>% 
  #select(-NiN_variable_code, -NiN_variable_value) %>% 
  distinct()
data_clean2


#identify duplicates
dup.naturtype <- data_clean2 |>
  dplyr::summarise(n = dplyr::n(), .by = c(identifikasjon_lokalId, hovedokosystem, kartleggingsar, lokalitetskvalitet, mosaikk, naturmangfold, naturtype,
                                           naturtypekode_short, naturtype_full, tilstand, region, km2, m2, maned, oppdragstaker, Naturmangfold, Tilstand, 
                                           NiN_variable_code)) |>
  dplyr::filter(n > 1L)


#remove duplicates
data_clean3 <- data_clean2 |>
  group_by(across(-NiN_variable_value)) %>%                 # group by all other columns
  filter(!(is.na(NiN_variable_value) & any(!is.na(NiN_variable_value)))) %>%
  ungroup()

n_removed <- nrow(data_clean2) - nrow(data_clean3)
n_removed

dup2.naturtype <- data_clean3 |>
  dplyr::summarise(n = dplyr::n(), .by = c(identifikasjon_lokalId, hovedokosystem, kartleggingsar, lokalitetskvalitet, mosaikk, naturmangfold, naturtype,
                                           naturtypekode_short, naturtype_full, tilstand, region, km2, m2, maned, oppdragstaker, Naturmangfold, Tilstand, 
                                           NiN_variable_code)) |>
  dplyr::filter(n > 1L)  

#remove a duplicate row in: 
data_clean4 <- data_clean3 %>%
  filter(
    !(identifikasjon_lokalId == "NINFP2110037887" &
        NiN_variable_code == "PRAK" &
        NiN_variable_value %in% c(0, "0"))   # handle char or numeric 0
  )


nin.all <-read_excel(path = "Data/Alle_NiN_variabler.xlsx", sheet = "Alle", col_names = TRUE)
nin.all <- nin.all %>% 
  select(-naturtypekode_short)

#the data contains variables which are defining nature type and not used in the quality assessment. Remove these variables from data set.
data_clean5 <- data_clean4 %>%
  left_join(nin.all, by = "NiN_variable_code") %>% 
  filter(Variable_type == "Tilstand" | 
         Variable_type == "Naturmangfold") %>% 
  filter(NiN_variable_code != "4DG-0" |
           naturtypekode_short == "C20") %>% 
  filter(NiN_variable_code != "4DL-0" |
           naturtypekode_short %in% c("C20", "C21", "E11_05") )


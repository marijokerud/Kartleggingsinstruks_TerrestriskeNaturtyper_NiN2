library(sf)
library(tidyverse)
library(units)
library(tmap)


Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

#This script is copied from https://github.com/NINAnor/NATplotter/blob/main/dataRAW.R 


# This script take the file downloaded from geoNorge and processes it for the shiny app.
# Replace the file and rerun the script when necessary.

# UPDATE 15.02.23: Updated the script to include year 2022. Several of the columns had new names!

# UPDATE 29.06.23 to get the correct m2 values

varList <- c(
  "kartleggingsår",
  "tilstand",
  "naturmangfold",
  "lokalitetskvalitet",
  "mosaikk",
  "usikkerhet",
  "hovedøkosystem",
  "oppdragstaker",
  "kriterium_nærTruet",
  "kriterium_sentralØkosystemFunksjon",
  "kriterium_spesieltDårligKartlagt",
  "kriterium_truet"
)


# path to data set
dir <- substr(getwd(), 1, 2)

path <- ifelse(
  dir == "C:",
  "R:/GeoSpatialData/Habitats_biotopes/Norway_Miljodirektoratet_Naturtyper_nin(instruks)/Original/Naturtyper - Miljodirektoratets instruks/Naturtyper_nin_0000_norge_25833_FILEGDB/Naturtyper_nin_0000_norge_25833_FILEGDB.gdb",
  "/data/R/GeoSpatialData/Habitats_biotopes/Norway_Miljodirektoratet_Naturtyper_nin(instruks)/Original/Naturtyper - Miljodirektoratets instruks/Naturtyper_nin_0000_norge_25833_FILEGDB/Naturtyper_nin_0000_norge_25833_FILEGDB.gdb"
)


# Read data ----------------------
sf::st_layers(path)
dat <- sf::read_sf(path, layer = "naturtyper_nin_omr")

# Check what years are included in the data
table(dat$kartleggingsår)
# remove 2025
dat <- dat |>
  filter(kartleggingsår != 2025)

names(dat)

# Get county delineation
path_county <- ifelse(
  dir == "C:",
  "R:/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Original/Norway_County/versjon2022/Basisdata_0000_Norge_25833_Fylker_FGDB.gdb",
  "/data/R/GeoSpatialData/AdministrativeUnits/Norway_AdministrativeUnits/Original/Norway_County/versjon2022/Basisdata_0000_Norge_25833_Fylker_FGDB.gdb"
)
# View layer names
st_layers(path_county)

# read two layers
counties <- sf::read_sf(path_county, layer = "fylke")
counties_names <- sf::read_sf(path_county, layer = "administrativenhetnavn")

# ... and combine them
counties$fylke <- counties_names$navn[match(
  counties$objid,
  counties_names$fylke_fk
)]

# View counties
tm_shape(counties) +
  tm_polygons(col = "fylke")

# Fix some naming errors
unique(counties$fylke)
counties <- counties %>%
  mutate(
    fylke = recode(
      fylke,
      "MÃ¸re og Romsdal" = "Møre og Romsdal",
      "TrÃ¸ndelag" = "Trøndelag"
    )
  )


# Finding and removing 'old' nature types that were not mapped after 2018 ---------------
# list all types
keepers_pre <- unique(dat$naturtype)
#Extract the year when these were mapped
years <- NULL
for (i in 1:length(keepers_pre)) {
  years[i] <- paste(
    sort(unique(dat$kartleggingsår[dat$naturtype == keepers_pre[i]])),
    collapse = ", "
  )
}
#Combine into one data frame
keepers_df <- data.frame(
  "Nature_type" = keepers_pre,
  "Year" = years
)
# find those only mapped in 2018
keepers <- keepers_df$Nature_type[grepl(
  "2019|2020|2021|2022|2023|2024",
  keepers_df$Year
)]
# and cut the rest
dat <- dat[dat$naturtype %in% keepers, ]


# Get fylke and region for each polygon -----------------
# This takes several minutes
dat_counties <- st_intersection(dat, counties)
# Check for boundary issues
unique(st_geometry_type(dat_counties)) # no lines introduced
nrow(dat) - nrow(dat_counties) # 70 new polygons means 140 polygons were split between counties
# find those that are duplicated
dups <- dat_counties$identifikasjon_lokalId[duplicated(
  dat_counties$identifikasjon_lokalId
)]
dups2 <- dat_counties[dat_counties$identifikasjon_lokalId %in% dups, ]
# Assign these to the county where they are biggest
# - force non-scientific numbers
options("scipen" = 100, "digits" = 4)
# calculate area
dups3 <- dups2 %>%
  mutate(
    area = units::drop_units(st_area(dups2)) / 1000,
    .after = identifikasjon_lokalId
  )
# Arrange by area and chose the one (the row) with the biggest area. That row will contain the 'fylke' that we want to assign this locality to.
dups4 <- dups3 %>%
  group_by(identifikasjon_lokalId) %>%
  arrange(desc(area), .by_group = T) %>%
  slice_head(n = 1)
# Remove all duplicates from the data set
#'%!in%' <- Negate('%in%')
dat_counties_filtered <- dat_counties %>%
  filter(!identifikasjon_lokalId %in% dups4$identifikasjon_lokalId)
# Find the duplicated localities in the data set from before the intersection
to_add <- dat %>%
  filter(identifikasjon_lokalId %in% dups4$identifikasjon_lokalId)
# Attach fylke name to those
to_add$fylke <- dups4$fylke[match(
  to_add$identifikasjon_lokalId,
  dups4$identifikasjon_lokalId
)]
# and bind with the rest of the data
dat_counties_filtered_added <- bind_rows(dat_counties_filtered, to_add)

#View(dat_counties_filtered_added[dat_counties_filtered_added$identifikasjon_lokalId %in% dups4$identifikasjon_lokalId, ])   #OK

# rename and clean
dat <- dat_counties_filtered_added
rm(
  counties,
  counties_names,
  dat_counties,
  dat_counties_filtered,
  dat_counties_filtered_added,
  dups,
  dups2,
  dups3,
  dups4,
  keepers_df,
  to_add
)

# Combine fylke into regions --------------------------------------
dat <- dat %>%
  mutate(
    region = case_when(
      fylke %in% c("Agder", "Vestfold og Telemark") ~ "Sørlandet",
      fylke %in% c("Oslo", "Innlandet", "Viken") ~ "Østlandet",
      fylke %in% c("Troms og Finnmark", "Nordland") ~ "NordNorge",
      fylke %in% c("Rogaland", "Vestland") ~ "Vestlandet",
      fylke %in% c("Møre og Romsdal", "Trøndelag") ~ "Midt-Norge"
    )
  )

#table(dat$region)

# Calculate area
# Need to divide m2 by 1e+6 to get km2
dat$m2 <- drop_units(st_area(dat))
dat$km2 <- dat$m2 / 1e+6


# Some recoding --------------------------------------------
names(dat)

dat2 <- dat %>%
  as.data.frame() %>%
  select(-SHAPE) %>%
  mutate(
    kriterium_ingenStatus = str_replace_na(kriterium_ingenStatus, "Nei")
  ) %>%
  mutate(
    kriterium_ingenStatus = str_replace(kriterium_ingenStatus, "1", "Ja")
  ) %>%
  
  mutate(kriterium_nærTruet = str_replace_na(kriterium_nærTruet, "Nei")) %>%
  mutate(kriterium_nærTruet = str_replace(kriterium_nærTruet, "1", "Ja")) %>%
  
  mutate(
    kriterium_sentralØkosystemFunksjon = str_replace_na(
      kriterium_sentralØkosystemFunksjon,
      "Nei"
    )
  ) %>%
  mutate(
    kriterium_sentralØkosystemFunksjon = str_replace(
      kriterium_sentralØkosystemFunksjon,
      "1",
      "Ja"
    )
  ) %>%
  
  mutate(
    kriterium_spesieltDårligKartlagt = str_replace_na(
      kriterium_spesieltDårligKartlagt,
      "Nei"
    )
  ) %>%
  mutate(
    kriterium_spesieltDårligKartlagt = str_replace(
      kriterium_spesieltDårligKartlagt,
      "1",
      "Ja"
    )
  ) %>%
  
  mutate(kriterium_truet = str_replace_na(kriterium_truet, "Nei")) %>%
  mutate(kriterium_truet = str_replace(kriterium_truet, "1", "Ja")) %>%
  
  mutate(across(all_of(varList), ~ na_if(., ''))) %>%
  
  mutate(
    tilstand = recode(
      tilstand,
      "sværtRedusert" = "1 - Svært redusert",
      "dårlig" = "2 - Dårlig",
      "moderat" = "3 - Moderat",
      "god" = "4 - God"
    )
  ) %>%
  mutate(
    naturmangfold = recode(
      naturmangfold,
      "lite" = "1 - Lite",
      'moderat' = "2 - Moderat",
      "stort" = "3 - Stort"
    )
  ) %>%
  mutate(
    lokalitetskvalitet = recode(
      lokalitetskvalitet,
      "sværtLavKvalitet" = "1 - Svært lav kvalitet",
      'lavKvalitet' = "2 - Lav kvalitet",
      "moderatKvalitet" = "3 - Moderat kvalitet",
      "høyKvalitet" = "4 - Høy kvalitet",
      "sværtHøyKvalitet" = "5 - Svært høy kvalitet"
    )
  )


# Melted data set  -----------------------------------------
# (one row for each nin-variable within each locality)
# This also takes 20 sec and results in 2.1 million rows
dat2_long <- tidyr::separate_rows(dat2, ninBeskrivelsesvariable, sep = ",") %>%
  separate(
    col = ninBeskrivelsesvariable,
    into = c("NiN_variable_code", "NiN_variable_value"),
    sep = "_",
    remove = F
  ) %>%
  mutate(NiN_variable_value = as.numeric(NiN_variable_value)) %>%
  filter(!str_detect(NiN_variable_code, "LKM")) %>%
  group_by(naturtype, NiN_variable_code, NiN_variable_value) %>%
  filter(n() > 2) %>%
  ungroup()
# The 'expected two pieces' warning is fine to ignore

# Remove nin-variables not recorded between 2021 and 2024 ---------------
# list all types
var_keepers_pre <- unique(dat2_long$NiN_variable_code)
#Extract the year when these were mapped
years <- NULL
for (i in 1:length(var_keepers_pre)) {
  years[i] <- paste(
    sort(unique(dat2_long$kartleggingsår[
      dat2_long$NiN_variable_code == var_keepers_pre[i]
    ])),
    collapse = ", "
  )
}
#Combine into one data frame
var_keepers_df <- data.frame(
  "variable" = var_keepers_pre,
  "Year" = years
)
# find those mapped in 2021-2024
keepers_var <- var_keepers_df$variable[grepl(
  "2021|2022|2023|2024",
  var_keepers_df$Year
)]
# and cut the rest
dat2_long <- dat2_long[dat2_long$NiN_variable_code %in% keepers_var, ]


# remove variables that end in '-' and that consist of only one symbol --------------
# These are 'obvious' mistakes
nrow(dat2_long) # 2 111 067
dat2_long_2 <- dat2_long %>%
  filter(!str_detect(NiN_variable_code, "-$"))
nrow(dat2_long_2) # 2 109 910
dat2_long_2 <- dat2_long_2 %>%
  filter(nchar(NiN_variable_code) > 1)
nrow(dat2_long_2) # 2 105 812

# Remove those with only zeros
temp <- dat2_long_2 %>%
  count(NiN_variable_code, NiN_variable_value) %>%
  pivot_wider(
    id_cols = NiN_variable_code,
    names_from = NiN_variable_value,
    values_from = n
  ) %>%
  mutate(
    rowsums = rowSums(select(., -NiN_variable_code, -'NA') != 0, na.rm = T)
  ) %>%
  filter(rowsums > 0)

dat2_long_3 <- dat2_long_2 %>%
  filter(NiN_variable_code %in% temp$NiN_variable_code)

length(unique(dat2_long_2$NiN_variable_code)) -
  length(unique(dat2_long_3$NiN_variable_code))
# This operation cut 69 variables

# Check for other weird NiN variable names ----------------------------
# There are a lot of errors in the column ninBeskrivelsesvariable
# View(table(dat2_long_3$NiN_variable_code))
# This looks better.

# Add month
dat2_long_3 <- dat2_long_3 %>%
  mutate("måned" = substr(kartleggingsdato, 5, 6))
dat2 <- dat2 %>%
  mutate("måned" = substr(kartleggingsdato, 5, 6))


# Add nature type code
# to the start of naturtype name in order to sort better
get_code <- dat2 %>%
  filter(grepl("ntyp", naturtypeKode)) %>%
  distinct(naturtype, .keep_all = T) %>%
  mutate(naturtypekode_short = str_remove(naturtypeKode, "ntyp_")) %>%
  select(naturtypekode_short, naturtype)
dat2 <- dat2 %>%
  left_join(get_code, by = "naturtype") %>%
  mutate(
    naturtype_temp = paste(naturtypekode_short, naturtype, sep = "_"),
    naturtype = paste(naturtypekode_short, naturtype, sep = "_")
  )
dat2_long_3 <- dat2_long_3 %>%
  left_join(get_code, by = "naturtype") %>%
  mutate(naturtype = paste(naturtypekode_short, naturtype, sep = "_"))

# The long data set is slow to load. I will delete some columns, see if that helps.
names(dat2_long_3)
dat2_long_3 <- dat2_long_3 %>%
  select(
    identifikasjon_lokalId,
    hovedøkosystem,
    kartleggingsår,
    kommuner,
    kriterium_nærTruet,
    kriterium_sentralØkosystemFunksjon,
    kriterium_spesieltDårligKartlagt,
    kriterium_truet,
    lokalitetskvalitet,
    mosaikk,
    naturmangfold,
    naturtype,
    NiN_variable_code,
    NiN_variable_value,
    oppdragstaker,
    tilstand,
    usikkerhet,
    objtype,
    fylke,
    region,
    km2,
    m2,
    måned
  )

# Anonymise the company names
temp <- data.frame(
  "oppdragstaker" = unique(dat2$oppdragstaker),
  "oppdragstaker_ID" = LETTERS[1:length(unique(dat2$oppdragstaker))]
)
temp$oppdragstaker2 <- paste("oppdragstaker", temp$oppdragstaker_ID, sep = "_")
temp2 <- temp$oppdragstaker2
names(temp2) <- temp$oppdragstaker

dat2 <- dat2 %>%
  mutate(oppdragstaker = recode(oppdragstaker, !!!temp2))
dat2_long_3 <- dat2_long_3 %>%
  mutate(oppdragstaker = recode(oppdragstaker, !!!temp2))

# The long data set is still slow to load. I will delete some more columns, see if that helps.
dat2_long_4 <- dat2_long_3 %>%
  rename(hovedokosystem = hovedøkosystem,
         kartleggingsar = kartleggingsår, 
         maned = måned) %>% 
  select(
    identifikasjon_lokalId,
    hovedokosystem,
    kartleggingsar,
    lokalitetskvalitet,
    mosaikk,
    naturmangfold,
    naturtype,
    NiN_variable_code,
    NiN_variable_value,
    tilstand,
    fylke,
    region,
    km2,
    m2,
    maned
  )

# Merge naturligÅpneOmråderILavlandet and naturligÅpneOmråderUnderSkoggrensa in hovedøkosystem
dat2_long_4 <- dat2_long_4 %>%  
  mutate(hovedokosystem = ifelse(hovedokosystem == "naturligÅpneOmråderUnderSkoggrensa",
                                 "NaturligÅpneOmråderUnderSkoggrensa", hovedokosystem)) %>% 
  mutate(hovedokosystem = ifelse(hovedokosystem == "naturligÅpneOmråderILavlandet",
                                 "NaturligÅpneOmråderUnderSkoggrensa", hovedokosystem)) %>% 
  mutate(hovedokosystem = ifelse(hovedokosystem == "fjell",
                                 "Fjell", hovedokosystem)) %>% 
  mutate(hovedokosystem = ifelse(hovedokosystem == "skog",
                                 "Skog", hovedokosystem)) %>% 
  mutate(hovedokosystem = ifelse(hovedokosystem == "semi-naturligMark",
                                 "Semi-naturligMark", hovedokosystem)) %>% 
  mutate(hovedokosystem = ifelse(hovedokosystem == "våtmark",
                               "Våtmark", hovedokosystem))

#Found out that nature type C01_Hule eiker was classified as Semi-naturligMark in 2018, changing to Skog.
dat2_long_4 <- dat2_long_4 %>%
  mutate(naturtypekode_short = str_extract(naturtype, "^[^_]+")) %>%  #Extract naturtype code
  mutate(hovedokosystem = if_else(
    naturtypekode_short == "C01" & hovedokosystem == "Semi-naturligMark",
    "Skog",
    hovedokosystem)
)

dat2_long_figure <- dat2_long_4 %>% 
  mutate(Naturmangfold= as.numeric(substr(naturmangfold, 1, 1))) %>% 
  mutate(Tilstand= as.numeric(substr(tilstand, 1, 1))) %>% 
  select(-NiN_variable_code, -NiN_variable_value) %>% 
  distinct()
dat2_long_figure


saveRDS(dat2, "shinyData/naturtyper.rds")
saveRDS(dat2_long_3, "shinyData/naturtyper_long.rds")
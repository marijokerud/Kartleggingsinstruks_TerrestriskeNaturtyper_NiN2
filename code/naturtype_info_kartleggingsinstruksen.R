library(readxl)
library(openxlsx)
library(tidyverse)


Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"


NiN.variable.all <- dat2_long_figure %>% 
  select(NiN_variable_code) %>% 
  distinct()

# Save table as excel file
write.xlsx(as.data.frame(NiN.variable.all), file = "data/Alle_NiN_variabler.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)


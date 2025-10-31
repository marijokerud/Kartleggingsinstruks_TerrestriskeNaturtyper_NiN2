# NaturligÅpneOmråderUnderSkoggrensa
open_areas <- data_clean6 %>% 
  filter(hovedokosystem == "NaturligÅpneOmråderUnderSkoggrensa") %>% 
  filter(NiN_variable_code %in% c("7FA", "7VR-RI",  "7SE"))
names(open_areas)

ggplot(open_areas, aes(x = naturmangfold, y = NiN_variable_value, color = hovedokosystem)) +
  geom_count() +                                   # sizes by count automatically
  scale_size_area(max_size = 10, name = "Count") + # nicer scaling
  facet_grid(.~NiN_variable_code)


ggplot(open_areas, aes(x = tilstand, y = NiN_variable_value, color = hovedokosystem)) +
  geom_count() +                                   # sizes by count automatically
  scale_size_area(max_size = 10, name = "Count") + # nicer scaling
  facet_grid(.~NiN_variable_code)


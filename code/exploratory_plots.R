library(ggplot2)

#Hovedøkosystem
hovedokosystem.data <- dat2_long_figure %>% 
  select(identifikasjon_lokalId, hovedokosystem, naturtype, naturmangfold, tilstand, Naturmangfold, Tilstand) %>% 
  group_by(hovedokosystem) %>% 
  mutate(Tilstand_mean = mean(Tilstand)) %>% 
  mutate(Tilstand_sd = sd(Tilstand)) %>% 
  mutate(Naturmangfold_mean = mean(Naturmangfold, na.rm = TRUE)) %>% 
  mutate(Naturmangfold_sd = sd(Naturmangfold, na.rm = TRUE))

#Naturtyper
naturtype.data <- dat2_long_figure %>% 
  select(identifikasjon_lokalId, hovedokosystem, naturtype, naturmangfold, tilstand, Naturmangfold, Tilstand) %>% 
  group_by(naturtype) %>% 
  mutate(Tilstand_mean = mean(Tilstand)) %>% 
  mutate(Tilstand_sd = sd(Tilstand)) %>% 
  mutate(Naturmangfold_mean = mean(Naturmangfold, na.rm = TRUE)) %>% 
  mutate(Naturmangfold_sd = sd(Naturmangfold, na.rm = TRUE))

hovedokosystem.plot <- hovedokosystem.data %>% 
  ggplot(aes(x = Naturmangfold_mean, y = Tilstand_mean,
             color = hovedokosystem)) + 
  geom_errorbar(aes(ymin = Tilstand_mean-Tilstand_sd, ymax = Tilstand_mean+Tilstand_sd)) +
  geom_errorbarh(aes(xmin = Naturmangfold_mean-Naturmangfold_sd, xmax = Naturmangfold_mean+Naturmangfold_sd))
hovedokosystem.plot

naturtype.skog <- naturtype.data %>% 
  filter(hovedokosystem == "Skog")
naturtype.våtmark <- naturtype.data %>% 
  filter(hovedokosystem == "Våtmark")
naturtype.seminaturligmark <- naturtype.data %>% 
  filter(hovedokosystem == "Semi-naturligMark")
naturtype.underskoggrensa <- naturtype.data %>% 
  filter(hovedokosystem == "NaturligÅpneOmråderUnderSkoggrensa")
naturtype.fjell <- naturtype.data %>% 
  filter(hovedokosystem == "Fjell")



naturtype.plot <- naturtype.fjell %>% 
  ggplot(aes(y = Naturmangfold_mean, x = Tilstand_mean,
             color = naturtype)) + 
  geom_errorbarh(aes(xmin = Tilstand_mean-Tilstand_sd, xmax = Tilstand_mean+Tilstand_sd)) +
  geom_errorbar(aes(ymin = Naturmangfold_mean-Naturmangfold_sd, ymax = Naturmangfold_mean+Naturmangfold_sd)) +
  scale_y_continuous(limits = c(0, 3.5), 
                     breaks = c(1, 2, 3),
                     labels = c("Lite", "Moderat", "Stort")) +
  scale_x_continuous(limits = c(0, 3.5), 
                     breaks = c(0, 1, 2, 3),
                     labels = c("Svært redusert", "Dårlig", "Moderat", "God")) +
  labs(y = "Naturmanfoldskår",
       x = "Tilstandskår",
       colour = "Naturtype"
  ) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=11),
        legend.text = element_text(color="black", size=9)) +
  theme(panel.grid.minor.x=element_blank(), #Hide all the gridlines
        panel.grid.minor.y=element_blank(), 
        panel.grid.major.x=element_line(colour = "lightgray"),
        panel.grid.major.y=element_line(colour = "lightgray")) 

ggsave("output/Tilstand-naturmangfold-fjell1.png", plot = naturtype.plot, width = 12, height = 8, dpi = 300)  

#+   theme(legend.position = "none")
naturtype.plot

Tilstand-naturmangfold-
  
names(naturtype.data)
theme(legend.position = "none")
geom_pointrange()
geom_point()
geom_errorbar()


  base + geom_crossbar()
  base + geom_pointrange()

  
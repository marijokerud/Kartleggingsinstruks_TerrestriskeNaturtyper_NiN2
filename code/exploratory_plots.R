# --- Packages ---
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(purrr)
library(ggpubr)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

# If you don't have tidyr/str_replace_all loaded already

# --- Data ---
dat.summary <- data_clean5
dat.summary <- data_clean6

# --- Helper: safe filename slug ---
slugify <- function(x) {
  x |>
    # normalize Norwegian characters
    str_replace_all("å", "a") |>
    str_replace_all("Å", "A") |>
    str_replace_all("ø", "o") |>
    str_replace_all("Ø", "O") |>
    str_replace_all("æ", "ae") |>
    str_replace_all("Æ", "Ae") |>
    # spaces & punctuation -> hyphens
    str_replace_all("[^A-Za-z0-9]+", "-") |>
    str_replace_all("-+", "-") |>
    str_replace_all("(^-|-$)", "") |>
    tolower()
}

# --- Summaries ---
# How many polygons per naturtype (kept from your code)
naturtype.summary <- dat.summary %>% 
  select(identifikasjon_lokalId, hovedokosystem, naturtype, naturtypekode_short, naturtype_full, naturmangfold, tilstand) %>% 
  group_by(naturtypekode_short) %>% 
  count(name = "n_polygons") %>%
  ungroup()

# Means/SD by hovedokosystem + naturtype (one row per naturtype within each hovedøkosystem)
naturtype_by_hovedokosystem <- dat.summary %>% 
  select(identifikasjon_lokalId, hovedokosystem, naturtype, naturtypekode_short, naturtype_full, naturmangfold, tilstand) %>% 
  group_by(hovedokosystem, naturtype, naturtypekode_short) %>% 
  summarise(
    Tilstand_mean        = mean(tilstand, na.rm = TRUE),
    Tilstand_sd          = sd(tilstand,   na.rm = TRUE),
    Naturmangfold_mean   = mean(naturmangfold, na.rm = TRUE),
    Naturmangfold_sd     = sd(naturmangfold,   na.rm = TRUE),
    n                    = dplyr::n(),
    .groups = "drop"
  ) %>% 
  # if a group has only one obs, sd is NA; set to 0 so errorbars still draw
  mutate(
    Tilstand_sd = replace_na(Tilstand_sd, 0),
    Naturmangfold_sd = replace_na(Naturmangfold_sd, 0)
  )

# Means/SD only by hovedokosystem (your earlier overview plot)
hovedokosystem_summary <- dat.summary %>% 
  select(identifikasjon_lokalId, hovedokosystem, naturmangfold, tilstand) %>% 
  group_by(hovedokosystem) %>% 
  summarise(
    Tilstand_mean = mean(tilstand, na.rm = TRUE),
    Tilstand_sd   = sd(tilstand,   na.rm = TRUE),
    Naturmangfold_mean = mean(naturmangfold, na.rm = TRUE),
    Naturmangfold_sd   = sd(naturmangfold,   na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) %>% 
  mutate(
    Tilstand_sd = replace_na(Tilstand_sd, 0),
    Naturmangfold_sd = replace_na(Naturmangfold_sd, 0)
  )

# --- Output folder ---
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# --- Styling helpers (consistent axes/labels/theme) ---
x_limits <- c(-0.1, 3.5)
y_limits <- c(0, 3.5)

base_axes <- list(
  scale_y_continuous(
    limits = y_limits,
    breaks = c(1, 2, 3),
    labels = c("Lite", "Moderat", "Stort")
  ),
  scale_x_continuous(
    limits = x_limits,
    breaks = c(0, 1, 2, 3),
    labels = c("Svært redusert", "Dårlig", "Moderat", "God")
  ),
  labs(
    x = "Tilstandskår",
    y = "Naturmangfoldskår"
  ),
  theme_bw(),
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size=14, hjust=0.5),
    axis.title.y = element_text(size=14, vjust=1),
    axis.text.x  = element_text(size=12, color="black"),
    axis.text.y  = element_text(size=12, color="black"),
    legend.title = element_text(color="black", size=11),
    legend.text  = element_text(color="black", size=9),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "lightgray"),
    panel.grid.major.y = element_line(colour = "lightgray")
  )
)

# --- Function to build per-hovedøkosystem plot (scatter with error bars & labels) ---
make_plot_for_ecosystem <- function(df_ecosys, ecosys_name) {
  ggplot(
    df_ecosys,
    aes(x = Tilstand_mean, y = Naturmangfold_mean, colour = naturtypekode_short)
  ) +
    # HORIZONTAL error bars (Tilstand): draw as segments to avoid ggstance
    geom_segment(aes(x = Tilstand_mean - Tilstand_sd,
                     xend = Tilstand_mean + Tilstand_sd,
                     y = Naturmangfold_mean,
                     yend = Naturmangfold_mean)) +
    # VERTICAL error bars (Naturmangfold)
    geom_errorbar(aes(ymin = Naturmangfold_mean - Naturmangfold_sd,
                      ymax = Naturmangfold_mean + Naturmangfold_sd)) +
    # points + labels
    geom_point(size = 2) +
    geom_text(aes(label = naturtypekode_short), size = 3, vjust = -0.7, show.legend = FALSE) +
    base_axes +
    labs(colour = "Naturtype",
         title = paste0("Tilstand vs. naturmangfold – ", ecosys_name))
}

# --- Generate & save one plot per hovedøkosystem ---
eco_list <- sort(unique(naturtype_by_hovedokosystem$hovedokosystem))

purrr::walk(
  eco_list,
  function(ec) {
    df_ec <- naturtype_by_hovedokosystem %>% filter(hovedokosystem == ec)
    if (nrow(df_ec) == 0) return(invisible(NULL))
    
    p <- make_plot_for_ecosystem(df_ec, ec)
    
    fn <- paste0("output/tilstand-naturmangfold-", slugify(ec), ".png")
    ggsave(filename = fn, plot = p, width = 12, height = 8, dpi = 300)
  }
)

# --- Optional: overall hovedøkosystem plot (means per hovedøkosystem) ---
hovedokosystem_plot <- ggplot(
  hovedokosystem_summary,
  aes(x = Naturmangfold_mean, y = Tilstand_mean, colour = hovedokosystem)
) +
  # VERTICAL error bars (Tilstand on y)
  geom_errorbar(aes(ymin = Tilstand_mean - Tilstand_sd,
                    ymax = Tilstand_mean + Tilstand_sd)) +
  # HORIZONTAL error bars (Naturmangfold on x) via segments
  geom_segment(aes(x = Naturmangfold_mean - Naturmangfold_sd,
                   xend = Naturmangfold_mean + Naturmangfold_sd,
                   y = Tilstand_mean,
                   yend = Tilstand_mean)) +
  geom_point(size = 3) +
  base_axes +
  labs(colour = "Hovedøkosystem",
       title = "Tilstand vs. naturmangfold – hovedøkosystem (gjennomsnitt)")


ggsave("output/tilstand-naturmangfold-hovedokosystem.png",
       plot = hovedokosystem_plot, width = 12, height = 8, dpi = 300)

library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(purrr)
library(vegan)

#DATA
names(dat2_long_figure)
dat <- dat2_long_figure %>%
  pivot_wider(names_from = NiN_variable_code, values_from = NiN_variable_value)

dat.PCA <- dat %>%
  select(-identifikasjon_lokalId, -kartleggingsar, -lokalitetskvalitet, -mosaikk,
         -naturmangfold, -naturtype, -naturtypekode_short, -naturtype_full, -tilstand, 
         -region, -km2, - maned, -oppdragstaker)


### run pca per hovedokosystem

models <- dat.PCA %>%
  group_by(hovedokosystem) %>%
  nest() %>%
  mutate(
    # keep numeric variables only (drop grouping column etc.)
    data_num = map(data, ~ select(.x, where(is.numeric))),
    # mean-impute NAs per group (skip all-NA columns)
    data_imp = map(data_num, ~ mutate(.x,
                                      across(everything(), ~ { m <- mean(., na.rm = TRUE)
                                      if (is.nan(m)) . else tidyr::replace_na(., m) })
    )),
    # run RDA for each group
    rda = map(data_imp, ~ vegan::rda(.x, scale = TRUE)),
    # extract scores
    site_scores    = map(rda, ~ as.data.frame(vegan::scores(.x, display = "sites")[, 1:2])),
    species_scores = map(rda, ~ as.data.frame(vegan::scores(.x, display = "species")[, 1:2])),
    # variance explained on the first components
    var_expl = map(rda, ~ {
      ev <- vegan::eigenvals(.x); ev / sum(ev)
    })
  ) %>%
  select(hovedokosystem, rda, data_imp, site_scores, species_scores, var_expl) %>%
  ungroup()



##### OLD CODE ###########
res <- vegan::rda(dat.imputed, scale = TRUE)
#load_df <- as.data.frame(res$rotation[, 1:2])
# If res is vegan::rda, use:
load_df <- as.data.frame(vegan::scores(res, display = "species")[, 1:2])

load_df <- load_df %>%
  tibble::rownames_to_column("variable") %>%
  rename(PC1 = 2, PC2 = 3)

# scale arrows to fit nicely into the score space
mult <- min(
  diff(range(output$PC1, na.rm = TRUE)) / (2 * max(abs(load_df$PC1))),
  diff(range(output$PC2, na.rm = TRUE)) / (2 * max(abs(load_df$PC2)))
)

load_df <- mutate(load_df, xend = PC1 * mult, yend = PC2 * mult) %>% 
  mutate(len = sqrt(PC1^2 + PC2^2))        # length of the arrow

thr <- quantile(load_df$len, 0.9, na.rm = TRUE)  #keep arrows longer than the 75th percentile
keep_vars <- load_df %>% filter(len >= thr) %>% pull(variable)

load_g <- scale_loadings(dat_g, load_df %>% filter(variable %in% keep_vars))

# --- 1) Loadings + axis labels ---------------------------------------------
# If res is prcomp:
if ("prcomp" %in% class(res)) {
  load_df <- as.data.frame(res$rotation[, 1:2]) |>
    tibble::rownames_to_column("variable") |>
    rename(PC1 = 2, PC2 = 3)
  var_expl <- res$sdev^2 / sum(res$sdev^2)
} else { # vegan::rda
  sp <- vegan::scores(res, display = "species")[, 1:2]
  load_df <- as.data.frame(sp) |>
    tibble::rownames_to_column("variable") |>
    rename(PC1 = 2, PC2 = 3)
  ev <- vegan::eigenvals(res); var_expl <- ev / sum(ev)
}

# scale arrows so they fit the score space (computed later per group)
scale_loadings <- function(dat, load_df) {
  mult <- min(
    diff(range(dat$PC1, na.rm = TRUE)) / (2 * max(abs(load_df$PC1))),
    diff(range(dat$PC2, na.rm = TRUE)) / (2 * max(abs(load_df$PC2)))
  )
  mutate(load_df, xend = PC1 * mult, yend = PC2 * mult)
}

# --- 2) Loop over hovedokosystem -------------------------------------------
groups <- unique(output$hovedokosystem)

for (g in groups) {
  dat_g <- output |> filter(hovedokosystem == g, !is.na(PC1), !is.na(PC2))
  
  # convex hulls per naturtypekode_short, skip groups with <3 unique points
  hulls_g <- dat_g |>
    group_by(naturtypekode_short) |>
    filter(dplyr::n_distinct(paste(PC1, PC2)) >= 3) |>
    slice(chull(PC1, PC2)) |>
    ungroup()
  
  load_g <- scale_loadings(dat_g, load_df %>% filter(variable %in% keep_vars))
  
  p <- ggplot(dat_g, aes(PC1, PC2, colour = naturtypekode_short)) +
    # filled hulls + outline
    geom_polygon(data = hulls_g,
                 aes(x= PC1, y = PC2, fill = naturtypekode_short, group = naturtypekode_short),
                 alpha = 0.2, colour = NA, inherit.aes = FALSE) +
    #geom_path(data = hulls_g,
    #          aes(x= PC1, y = PC2, group = naturtypekode_short),
    #          linewidth = 0.5, show.legend = FALSE, inherit.aes = FALSE) +
    # points (optional)
    #geom_point(size = 1.6, alpha = 0.7) +
    # biplot arrows
    geom_segment(data = load_g,
                 aes(x = 0, y = 0, xend = xend, yend = yend),
                 inherit.aes = FALSE, arrow = arrow(length = unit(0.02, "npc"))) +
    geom_text(data = load_g,
              aes(x = xend, y = yend, label = variable),
              inherit.aes = FALSE, hjust = -0.1, vjust = 0.5) +
    coord_equal() +
    labs(
      title = g,
      x = paste0("PC1 (", round(100 * var_expl[1], 1), "%)"),
      y = paste0("PC2 (", round(100 * var_expl[2], 1), "%)"),
      colour = "Naturtype", fill = "Naturtype"
    ) +
    theme_bw() +
    theme(legend.position = "right")
  
  # --- 3) Save one file per group ------------------------------------------
  fname <- paste0("PCA_", str_replace_all(g, "[^[:alnum:]]+", "_"), ".png")
  ggsave(filename = fname, plot = p, width = 7, height = 6, dpi = 300)
}

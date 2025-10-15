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

prep_numeric <- function(dat.PCA) {
  dat.PCA %>%
    select(where(is.numeric)) %>%
    # make non-finite values NA
    mutate(across(everything(), ~ ifelse(is.finite(.), ., NA_real_))) %>%
    # drop columns that are all NA
    select(where(~ sum(!is.na(.)) > 0)) %>%
    # drop zero-variance columns (after NAs removed)
    select(where(~ sd(., na.rm = TRUE) > 0)) %>%
    # drop rows that are all NA
    filter(rowSums(!is.na(.)) > 0)
}

models <- dat.PCA %>%
  group_by(hovedokosystem) %>%
  nest() %>%
  mutate(data_num = map(data, prep_numeric),
    # mean-impute remaining NAs (safe: no all-NA cols remain)
    data_imp = map(data_num, ~ mutate(.x,
                                      across(everything(), ~ replace_na(., mean(., na.rm = TRUE)))
    )),
    ok = map_lgl(data_imp, ~ nrow(.x) >= 2 && ncol(.x) >= 2),
    rda = map2(data_imp, ok, ~ if (.y) vegan::rda(.x, scale = TRUE) else NULL),
    site_scores    = map(rda, ~ if (is.null(.x)) NULL else as.data.frame(vegan::scores(.x, display = "sites")[, 1:2])),
    species_scores = map(rda, ~ if (is.null(.x)) NULL else as.data.frame(vegan::scores(.x, display = "species")[, 1:2])),
    var_expl = map(rda, ~ if (is.null(.x)) NULL else { ev <- vegan::eigenvals(.x); ev / sum(ev) })
  ) %>%
  select(hovedokosystem, ok, rda, data_imp, site_scores, species_scores, var_expl) %>%
  ungroup()

#Extract all groups at once (tidy data frames)
#Site scores (PC1/PC2) for all groups
site_scores_all <- models %>%
  filter(!vapply(rda, is.null, logical(1))) %>%   # keep groups with a model
  transmute(
    hovedokosystem,
    site_scores = lapply(site_scores, function(x) {
      as.data.frame(x)[, 1:2, drop = FALSE] %>%    # PC1, PC2
        rownames_to_column("row_id") %>%
        rename(PC1 = 2, PC2 = 3)
    })
  ) %>%
  tidyr::unnest(site_scores)

#Variable loadings (“species” scores) for all groups
loadings_all <- models %>%
  filter(!vapply(rda, is.null, logical(1))) %>%
  transmute(
    hlav = hovedokosystem,
    species_scores = lapply(species_scores, function(x) {
      as.data.frame(x)[, 1:2, drop = FALSE] %>%
        rownames_to_column("variable") %>%
        rename(PC1 = 2, PC2 = 3)
    })
  ) %>%
  tidyr::unnest(species_scores) %>%
  rename(hovedokosystem = hlav)

#Variance explained per group
var_expl_all <- models %>%
  filter(!vapply(rda, is.null, logical(1))) %>%
  transmute(
    hovedokosystem,
    var = lapply(rda, function(m) {
      v <- vegan::eigenvals(m); tibble(PC = seq_along(v), var = as.numeric(v / sum(v)))
    })
  ) %>%
  tidyr::unnest(var) %>%
  mutate(var_pct = round(100 * var, 2))


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

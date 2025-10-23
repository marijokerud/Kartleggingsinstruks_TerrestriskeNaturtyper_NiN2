library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(vegan)
library(tibble)

dat.PCA <- dat %>%
  select( -kartleggingsar, -lokalitetskvalitet, -mosaikk,
          -naturmangfold, -naturtype_full, -tilstand, 
          -region, -km2, - maned, -oppdragstaker)  #-hovedokosystem, -identifikasjon_lokalId, -naturtype,-naturtypekode_short, 


prep_group <- function(gdf) {
  ids   <- gdf$identifikasjon_lokalId
  subg  <- gdf$naturtypekode_short  # used for polygons/colouring
  
  X <- gdf %>%
    select(where(is.numeric)) %>%
    mutate(across(everything(), ~ ifelse(is.finite(.), ., NA_real_)))
  
  # drop columns that are all NA or zero variance
  X <- X %>%
    select(where(~ sum(!is.na(.)) > 0)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0))
  
  # drop rows all-NA across remaining cols (track which IDs/subgroups stay)
  keep_row <- rowSums(!is.na(X)) > 0
  X   <- X[keep_row, , drop = FALSE]
  ids <- ids[keep_row]
  subg <- subg[keep_row]
  
  # mean impute (within this group)
  X_imp <- X %>% mutate(across(everything(), ~ replace_na(., mean(., na.rm = TRUE))))
  
  list(X_imp = X_imp, ids = ids, subg = subg)
}

#------------------------------------------------------------
# 2) Build models per hovedokosystem
#------------------------------------------------------------
models <- dat.PCA %>%
  group_by(hovedokosystem) %>%
  nest() %>%
  mutate(
    prep   = map(data, prep_group),
    X_imp  = map(prep, "X_imp"),
    row_id = map(prep, "ids"),
    subgroup = map(prep, "subg"),
    ok     = map_lgl(X_imp, ~ nrow(.x) >= 2 && ncol(.x) >= 2),
    
    rda    = map2(X_imp, ok, ~ if (.y) vegan::rda(.x, scale = TRUE) else NULL),
    
    # Site scores (PC1/PC2) + carry ID and subgroup (same order)
    site_scores = pmap(list(rda, row_id, subgroup), function(m, ids, subg) {
      if (is.null(m)) return(NULL)
      sc <- as.data.frame(vegan::scores(m, display = "sites")[, 1:2, drop = FALSE])
      sc %>%
        mutate(
          identifikasjon_lokalId = ids,
          naturtypekode_short = subg
        ) %>%
        relocate(identifikasjon_lokalId, naturtypekode_short) %>%
        rename(PC1 = 3, PC2 = 4)
    }),
    
    # Species (variable) scores per group
    species_scores = map(rda, ~ {
      if (is.null(.x)) return(NULL)
      as.data.frame(vegan::scores(.x, display = "species")[, 1:2, drop = FALSE]) %>%
        rownames_to_column("variable") %>%
        rename(PC1 = 2, PC2 = 3)
    }),
    
    # Variance explained
    var_expl = map(rda, ~ {
      if (is.null(.x)) return(NULL)
      ev <- vegan::eigenvals(.x); ev / sum(ev)
    })
  ) %>%
  ungroup()



#######################
library(dplyr)
library(purrr)
library(tibble)
library(vegan)
library(tidyr)

# models must include columns: hovedokosystem, rda, X_imp (imputed numeric df per group)
# If your existing `models` doesn't keep X_imp, add it where you build models.

# 1) Run envfit per group
models_sig <- models %>%
  mutate(
    envfit = map2(rda, X_imp, ~ {
      if (is.null(.x) || is.null(.y)) return(NULL)
      vegan::envfit(.x, .y, permutations = 999)
    }),
    
    # 2) Tidy envfit results (vectors) per group
    envfit_tbl = map(envfit, ~ {
      if (is.null(.x)) return(NULL)
      vec_scores <- as.data.frame(vegan::scores(.x, display = "vectors"))
      tibble(
        variable = rownames(vec_scores),
        axis1 = vec_scores[,1],
        axis2 = vec_scores[,2],
        r2 = .x$vectors$r,
        p  = .x$vectors$pvals
      ) %>%
        mutate(p_adj = p.adjust(p, method = "BH")) %>%
        arrange(p)
    })
  )

# 3) Stack all groups; mark significant variables
envfit_all <- models_sig %>%
  select(hovedokosystem, envfit_tbl) %>%
  unnest(envfit_tbl)

# e.g., significant at 5% FDR:
sig_vars_all <- envfit_all %>%
  filter(p_adj < 0.05)

# Peek:
sig_vars_all %>% group_by(hovedokosystem) %>%
  summarise(n_sig = n(), .groups = "drop") %>%
  arrange(desc(n_sig))

# Join envfit significance onto your per-group loadings before scaling/plotting
models_for_plot <- models %>%
  left_join(
    sig_vars_all %>% select(hovedokosystem, variable, p_adj, r2),
    by = c("hovedokosystem")
  ) %>%
  mutate(
    species_scores_sig = map2(species_scores, hovedokosystem, ~ {
      if (is.null(.x)) return(NULL)
      # keep only significant variables for THIS group
      sig_this <- sig_vars_all %>%
        filter(hovedokosystem == ..2, p_adj < 0.05) %>%
        pull(variable)
      .x %>% filter(variable %in% sig_this)
    })
  )
###prevois one or next one?

# keep only significant variables (p_adj < 0.05) per hovedokosystem
models_for_plot <- models %>%
  mutate(
    species_scores_sig = purrr::map2(species_scores, hovedokosystem, ~ {
      if (is.null(.x)) return(NULL)
      sig <- sig_vars_all %>%
        dplyr::filter(hovedokosystem == .y, p_adj < 0.05) %>%
        dplyr::pull(variable)
      .x %>% dplyr::filter(variable %in% sig)
    })
  )


#------------------------------------------------------------
# 3) Plot per group with arrows computed/scaled per group
#    - keep only the longest arrows PER GROUP
#    - draw convex hulls per naturtypekode_short (optional)
#------------------------------------------------------------

# Choose arrow selection rule (pick ONE approach):
keep_top_k <- NULL     # e.g., 5  (set to an integer to keep top-K arrows)
keep_q     <- 0.25     # e.g., 0.90 to keep arrows >= 90th percentile length

scale_loadings <- function(site_df, load_df) {
  # site_df: per-group site scores with PC1, PC2
  # load_df: per-group species scores with PC1, PC2
  mult <- min(
    diff(range(site_df$PC1, na.rm = TRUE)) / (2 * max(abs(load_df$PC1))),
    diff(range(site_df$PC2, na.rm = TRUE)) / (2 * max(abs(load_df$PC2)))
  )
  mutate(load_df, xend = PC1 * mult, yend = PC2 * mult)
}

# Loop and save
walk(seq_len(nrow(models)), function(i) {
  gname <- models$hovedokosystem[i]
  m     <- models$rda[[i]]
  sites <- models$site_scores[[i]]
  loads <- models_for_plot$species_scores_sig[[i]]
  ve    <- models$var_expl[[i]]
  
  if (is.null(m) || is.null(sites) || is.null(loads)) {
    message("Skipping group ", gname, " (no model or no scores).")
    return(invisible())
  }
  
  # compute arrow lengths per group and select longest arrows per group
  loads <- loads %>% mutate(len = sqrt(PC1^2 + PC2^2))
  
  if (!is.null(keep_top_k)) {
    loads_keep <- loads %>% slice_max(len, n = keep_top_k, with_ties = FALSE)
  } else {
    thr <- quantile(loads$len, keep_q, na.rm = TRUE)
    loads_keep <- loads %>% filter(len >= thr)
  }
  
  # scale arrows to group’s score space
  loads_scaled <- scale_loadings(sites, loads_keep)
  
  # optional convex hulls per naturtypekode_short (skip if < 3 unique points)
  hulls_g <- sites %>%
    filter(!is.na(PC1), !is.na(PC2)) %>%
    group_by(naturtypekode_short) %>%
    filter(dplyr::n_distinct(paste(PC1, PC2)) >= 3) %>%
    slice(chull(PC1, PC2)) %>%
    ungroup()
  
  p <- ggplot(sites, aes(PC1, PC2, colour = naturtypekode_short)) +
    # filled hulls (optional – comment out if you don’t want them)
    geom_polygon(
      data = hulls_g,
      aes(x = PC1, y = PC2, fill = naturtypekode_short, group = naturtypekode_short),
      alpha = 0.20, colour = NA, inherit.aes = FALSE
    ) +
    # points (optional)
    # geom_point(size = 1.6, alpha = 0.7) +
    # per-group arrows & labels
    geom_segment(
      data = loads_scaled,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.02, "npc"))
    ) +
    geom_text(
      data = loads_scaled,
      aes(x = xend, y = yend, label = variable),
      inherit.aes = FALSE,
      hjust = -0.1, vjust = 0.5
    ) +
    coord_equal() +
    labs(
      title = gname,
      x = paste0("PC1 (", round(100 * ve[1], 1), "%)"),
      y = paste0("PC2 (", round(100 * ve[2], 1), "%)"),
      colour = "Naturtype", fill = "Naturtype"
    ) +
    theme_bw() +
    theme(legend.position = "right")
  
  # Save one PNG per group
  fname <- paste0("PCA_", str_replace_all(gname, "[^[:alnum:]]+", "_"), ".png")
  ggsave(filename = fname, plot = p, width = 7, height = 6, dpi = 300)
})


#------------------------------------------------------------
# 4) Tidy outputs you can use 
#------------------------------------------------------------

# All site scores (PC1/PC2) with ID and hovedokosystem in one tibble
site_scores_all <- models %>%
  filter(!vapply(rda, is.null, logical(1))) %>%
  select(hovedokosystem, site_scores) %>%
  unnest(site_scores)

# All loadings (“species” scores) per group
loadings_all <- models %>%
  filter(!map_lgl(rda, is.null)) %>%                # keep groups with a model
  mutate(species_scores = map(rda, ~ {
    sc <- vegan::scores(.x, display = "species")[, 1:2, drop = FALSE]
    as.data.frame(sc) %>%
      rownames_to_column("variable") %>%
      dplyr::rename(PC1 = 2, PC2 = 3)
  })) %>%
  select(hovedokosystem, species_scores) %>%
  unnest(species_scores)

# PC1/PC2 variance (%) per group
pc_var_all <- models %>%
  filter(!vapply(rda, is.null, logical(1))) %>%
  transmute(
    hovedokosystem,
    pc1 = round(100 * map_dbl(var_expl, ~ .x[1]), 1),
    pc2 = round(100 * map_dbl(var_expl, ~ .x[2]), 1)
  )

# (Optional) quick peek
head(site_scores_all)
head(loadings_all)
pc_var_all





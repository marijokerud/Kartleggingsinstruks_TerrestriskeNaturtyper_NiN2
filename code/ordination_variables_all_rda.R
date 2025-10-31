library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(vegan)
library(stringr)
library(ggplot2)
library(readxl)     # if you load from xlsx
library(openxlsx)   # for write.xlsx if needed

# ------------------------------------------------
# 1) Wide data like your PCA (KEEP Y columns here)
# ------------------------------------------------
names(data_clean6)
dat.RDA1 <- data_clean6 %>%
  select(-Variable_type) %>% 
  pivot_wider(names_from = NiN_variable_code, values_from = NiN_variable_value)

dat.RDA <- dat.RDA1 %>%
  select( -kartleggingsar, -lokalitetskvalitet, -mosaikk, 
          -naturmangfold_orig, -naturtype, -naturtype_full, -tilstand_orig, 
          -region, -km2, - maned, -oppdragstaker) 

#Save table as excel file
#slice(1:5000)
#write.xlsx(as.data.frame(dat.RDA), file = "data/test_RDA_HOS.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)


# Explicit response names (new capitalization)
y_cols <- c("naturmangfold","tilstand")
stopifnot(all(y_cols %in% names(dat.RDA)))

# ------------------------------------------------
# 2) Helper: build Y (numeric) and X (numeric-only NiN) per group
# ------------------------------------------------
prep_group_rda <- function(gdf) {
  ids  <- gdf$identifikasjon_lokalId
  subg <- gdf$naturtypekode_short
  
  # Y must exist and be numeric
  stopifnot(all(y_cols %in% names(gdf)))
  Y <- gdf %>%
    select(all_of(y_cols)) %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  
  # Drop rows with NA in Y
  keep_y <- rowSums(is.na(Y)) == 0
  if (!any(keep_y)) return(list(Y=NULL, X_imp=NULL, ids=NULL, subg=NULL))
  Y <- Y[keep_y, , drop = FALSE]
  
  # X: strictly numeric predictors only (drop Y from X)
  X <- gdf[keep_y, , drop = FALSE] %>%
    select(where(is.numeric)) %>%
    select(-any_of(y_cols))
  
  # Clean X: non-finite -> NA, drop all-NA and zero-variance columns
  X <- X %>%
    mutate(across(everything(), ~ ifelse(is.finite(.), ., NA_real_))) %>%
    select(where(~ sum(!is.na(.)) > 0)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0))
  
  if (nrow(X) < 3 || ncol(X) < 2) {
    return(list(Y=NULL, X_imp=NULL, ids=NULL, subg=NULL))
  }
  
  # Mean-impute within group (X only)
  X_imp <- X %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  list(Y = Y, X_imp = X_imp, ids = ids[keep_y], subg = subg[keep_y])
}

# ------------------------------------------------
# 3) Fit RDA per hovedokosystem and extract outputs
# ------------------------------------------------
models_rda <- dat.RDA %>%
  group_by(hovedokosystem) %>%
  nest() %>%
  mutate(
    prep     = map(data, prep_group_rda),
    Y        = map(prep, "Y"),
    X_imp    = map(prep, "X_imp"),
    row_id   = map(prep, "ids"),
    subgroup = map(prep, "subg"),
    ok       = map_lgl(X_imp, ~ !is.null(.x) && nrow(.x) >= 3 && ncol(.x) >= 2),
    
    # RDA: Y ~ X_imp (guard: pure numeric frames)
    rda = pmap(list(Y, X_imp, ok), function(Y, X, okflag) {
      if (!okflag) return(NULL)
      Ynum <- as.data.frame(lapply(Y, as.numeric))
      Xnum <- as.data.frame(lapply(X, as.numeric))
      vegan::rda(Ynum ~ ., data = Xnum, scale = TRUE)
    }),
    
    # Site scores (RDA1/RDA2)
    site_scores = pmap(list(rda, row_id, subgroup), function(m, ids, subg) {
      if (is.null(m)) return(NULL)
      sc <- as.data.frame(vegan::scores(m, display = "sites", choices = 1:2, scaling = 2))
      tibble(
        identifikasjon_lokalId = ids,
        naturtypekode_short = subg,
        RDA1 = sc[,1], RDA2 = sc[,2]
      )
    }),
    
    # Predictor (biplot) scores
    biplot_scores = map(rda, ~ {
      if (is.null(.x)) return(NULL)
      bp <- as.data.frame(vegan::scores(.x, display = "bp", choices = 1:2, scaling = 2))
      bp %>%
        rownames_to_column("predictor") %>%
        rename(RDA1 = 2, RDA2 = 3) %>%
        mutate(length = sqrt(RDA1^2 + RDA2^2))
    }),
    
    # Response arrows (naturmangfold, tilstand)
    response_scores = map(rda, ~ {
      if (is.null(.x)) return(NULL)
      sp <- as.data.frame(vegan::scores(.x, display = "species", choices = 1:2, scaling = 2))
      sp %>%
        rownames_to_column("response") %>%
        rename(RDA1 = 2, RDA2 = 3)
    }),
    
    # % constrained variance (use constrained eigenvalues) 
    var_expl = map(rda, ~ {
      if (is.null(.x)) return(NULL)
      ev <- vegan::eigenvals(.x, constrained = TRUE)
      ev / sum(ev)
    }),
    
    # Adjusted R² and permutation tests (modest permutations)
    r2_adj       = map_dbl(rda, ~ if (is.null(.x)) NA_real_ else vegan::RsquareAdj(.x)$adj.r.squared),
    anova_global = map(rda, ~ if (is.null(.x)) NULL else anova.cca(.x, permutations = 199)),
    anova_terms  = map(rda, ~ if (is.null(.x)) NULL else anova.cca(.x, by = "terms", permutations = 199)),
    
    # Add FDR (BH) column for by-terms table (if available)
    anova_terms_fdr = map(anova_terms, ~ {
      if (is.null(.x)) return(NULL)
      df <- as.data.frame(.x)
      if ("Pr(>F)" %in% names(df)) df$padj <- p.adjust(df[["Pr(>F)"]], method = "BH")
      df
    })
  ) %>%
  ungroup()

# ------------------------------------------------
# 4) Tidy stacked outputs (like your PCA objects)
# ------------------------------------------------
site_scores_all_rda <- models_rda %>%
  filter(!map_lgl(site_scores, is.null)) %>%
  select(hovedokosystem, site_scores) %>%
  unnest(site_scores)

nin.biplot <- nin.all %>% 
  rename(predictor = NiN_variable_code)

biplot_all_rda <- models_rda %>%
  filter(!map_lgl(biplot_scores, is.null)) %>%
  select(hovedokosystem, biplot_scores) %>%
  unnest(biplot_scores) %>%
  mutate(predictor = str_remove(as.character(predictor), "^X+")) %>%                 # drop leading X
  mutate(predictor = str_replace_all(as.character(predictor), fixed("."), "-")) %>%  # change . -> -
  inner_join(nin.biplot, by = "predictor") 

response_all_rda <- models_rda %>%
  filter(!map_lgl(response_scores, is.null)) %>%
  select(hovedokosystem, response_scores) %>%
  unnest(response_scores)

var_all_rda <- models_rda %>%
  filter(!map_lgl(var_expl, is.null)) %>%
  transmute(
    hovedokosystem,
    rda1   = round(100 * map_dbl(var_expl, ~ .x[1]), 1),
    rda2   = round(100 * map_dbl(var_expl, ~ .x[2]), 1),
    r2_adj = round(r2_adj, 3)
  )

# (Optional) write to Excel
write.xlsx(as.data.frame(site_scores_all_rda), "RDA_plots/RDA_site_scores.xlsx", rowNames = FALSE)
write.xlsx(as.data.frame(biplot_all_rda),     "RDA_plots/RDA_biplot_scores.xlsx", rowNames = FALSE)
write.xlsx(as.data.frame(response_all_rda),   "RDA_plots/RDA_response_scores.xlsx", rowNames = FALSE)
write.xlsx(as.data.frame(var_all_rda),    "RDA_plots/RDA_axis_variance.xlsx", rowNames = FALSE)

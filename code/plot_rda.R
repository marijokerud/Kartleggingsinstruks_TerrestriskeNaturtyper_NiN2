library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)
library(grid)   # for arrow()
library(ggrepel)



make_rda_plots <- function(models_rda, nin_lookup,
                           draw_points = FALSE, save_dir = "RDA_plots") {
  dir.create(save_dir, showWarnings = FALSE)
  
  scale_arrows <- function(sites, arrows) {
    if (is.null(arrows) || nrow(arrows) == 0) return(NULL)
    rngx  <- diff(range(sites$RDA1, na.rm = TRUE))
    rngy  <- diff(range(sites$RDA2, na.rm = TRUE))
    axmax <- max(abs(arrows$RDA1), na.rm = TRUE)
    aymax <- max(abs(arrows$RDA2), na.rm = TRUE)
    if (!is.finite(rngx) || !is.finite(rngy) || axmax == 0 || aymax == 0) return(NULL)
    mult <- min(rngx / (2 * axmax), rngy / (2 * aymax))
    dplyr::mutate(arrows, xend = RDA1 * mult, yend = RDA2 * mult)
  }
  
  walk(seq_len(nrow(models_rda)), function(i) {
    title <- models_rda$hovedokosystem[i]
    mfit  <- models_rda$rda[[i]]
    bplt  <- models_rda$biplot_scores[[i]]
    resp  <- models_rda$response_scores[[i]]
    vexp  <- models_rda$var_expl[[i]]
    
    if (is.null(mfit) || is.null(bplt) || length(vexp) < 2) {
      message("Skipping ", title, " (no model or scores)."); return(invisible())
    }
    
    # site (LC) scores
    sc_lc <- as.data.frame(vegan::scores(mfit, display = "lc", choices = 1:2, scaling = 2))
    sites <- tibble::tibble(
      identifikasjon_lokalId = models_rda$row_id[[i]],
      naturtypekode_short    = models_rda$subgroup[[i]],
      RDA1 = sc_lc[,1], RDA2 = sc_lc[,2]
    )
    
    # clean predictor IDs to match lookup (same as you did before)
    bplt <- bplt %>%
      mutate(
        predictor = predictor %>%
          as.character() %>%
          str_remove("^X+") %>%
          str_replace_all(fixed("."), "-"),
        length = if (!"length" %in% names(.)) sqrt(RDA1^2 + RDA2^2) else length
      ) %>%
      left_join(nin_lookup, by = "predictor")
    
    # keep only Variable_Type of interest, and apply 50% rule within each type
    bplt_keep <- bplt %>%
      filter(Variable_Type %in% c("Tilstand", "Naturmangfold")) %>%
      group_by(Variable_Type) %>%
      mutate(max_len = max(length, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(length >= 0.5 * max_len) %>%
      select(-max_len)
    
    # If nothing survives (rare), silently skip plotting arrows
    if (nrow(bplt_keep) == 0) {
      message("No arrows pass 50% rule in ", title, ". Plotting sites + responses only.")
    }
    
    # scale predictor and response arrows
    bplt_s <- scale_arrows(sites, bplt_keep)
    resp_s <- scale_arrows(sites, dplyr::rename(resp, predictor = response))
    
    # convex hulls
    hulls <- sites %>%
      filter(!is.na(RDA1), !is.na(RDA2)) %>%
      group_by(naturtypekode_short) %>%
      filter(dplyr::n_distinct(paste(RDA1, RDA2)) >= 3) %>%
      slice(chull(RDA1, RDA2)) %>%
      ungroup()
    
    p <- ggplot(sites, aes(RDA1, RDA2, colour = naturtypekode_short)) +
      geom_polygon(
        data = hulls,
        aes(RDA1, RDA2, fill = naturtypekode_short, group = naturtypekode_short),
        alpha = 0.20, colour = NA, inherit.aes = FALSE
      ) +
      { if (draw_points) geom_point(size = 1.4, alpha = 0.6) } +
      # predictor arrows (filtered by 50% per Variable_Type)
      geom_segment(
        data = bplt_s,
        aes(x = 0, y = 0, xend = xend, yend = yend, linetype = Variable_Type),
        inherit.aes = FALSE,
        arrow = grid::arrow(length = unit(0.02, "npc"))
      ) +
      ggrepel::geom_text_repel(
        data = bplt_s,
        aes(x = xend, y = yend, label = predictor, colour = NULL),
        inherit.aes = FALSE, size = 3,
        max.overlaps = Inf, box.padding = 0.3, point.padding = 0.1,
        min.segment.length = 0, segment.size = 0.3
      ) +
      # response arrows (always plotted, bold labels)
      geom_segment(
        data = resp_s,
        aes(x = 0, y = 0, xend = xend, yend = yend),
        inherit.aes = FALSE
      ) +
      ggrepel::geom_text_repel(
        data = resp_s,
        aes(x = xend, y = yend, label = predictor),
        inherit.aes = FALSE, fontface = "bold", size = 3.2,
        max.overlaps = Inf, box.padding = 0.35, point.padding = 0.15,
        min.segment.length = 0, segment.size = 0.3
      ) +
      coord_equal() +
      labs(
        title = title,
        x = sprintf("RDA1 (%.1f%%)", 100 * as.numeric(vexp[1])),
        y = sprintf("RDA2 (%.1f%%)", 100 * as.numeric(vexp[2])),
        colour = "Naturtype", fill = "Naturtype", linetype = "Variable_Type"
      ) +
      theme_bw() +
      theme(legend.position = "right")
    
    ggsave(
      filename = file.path(save_dir, paste0("RDA_", str_replace_all(title, "[^[:alnum:]]+", "_"), ".png")),
      plot = p, width = 7, height = 6, dpi = 300
    )
  })
}
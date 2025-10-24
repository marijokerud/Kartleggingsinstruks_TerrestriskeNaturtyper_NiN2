library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)
library(grid)   # for arrow()

make_rda_plots <- function(models_rda, 
                           keep_top_k = NULL, keep_q = 0.9,
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
  
  purrr::walk(seq_len(nrow(models_rda)), function(i) {
    title <- models_rda$hovedokosystem[i]
    mfit  <- models_rda$rda[[i]]
    bplt  <- models_rda$biplot_scores[[i]]
    resp  <- models_rda$response_scores[[i]]
    vexp  <- models_rda$var_expl[[i]]
    
    if (is.null(mfit) || is.null(bplt) || length(vexp) < 2) {
      message("Skipping ", title, " (no model or scores)."); return(invisible())
    }
    
    # >>> use LC site scores (not WA) <<<
    sc_lc <- as.data.frame(vegan::scores(mfit, display = "lc", choices = 1:2, scaling = 2))
    sites <- tibble::tibble(
      identifikasjon_lokalId = models_rda$row_id[[i]],
      naturtypekode_short    = models_rda$subgroup[[i]],
      RDA1 = sc_lc[,1], RDA2 = sc_lc[,2]
    )
    
    # choose longest predictor arrows
    bplt <- dplyr::mutate(bplt, len = sqrt(RDA1^2 + RDA2^2))
    if (!is.null(keep_top_k)) {
      bplt <- dplyr::slice_max(bplt, len, n = keep_top_k, with_ties = FALSE)
    } else if (!is.null(keep_q)) {
      thr <- stats::quantile(bplt$len, keep_q, na.rm = TRUE)
      bplt <- dplyr::filter(bplt, len >= thr)
    }
    
    # scale arrows to site cloud
    bplt_s <- scale_arrows(sites, bplt)
    resp_s <- scale_arrows(sites, dplyr::rename(resp, predictor = response))
    
    # convex hulls per naturtypekode_short
    hulls <- sites %>%
      dplyr::filter(!is.na(RDA1), !is.na(RDA2)) %>%
      dplyr::group_by(naturtypekode_short) %>%
      dplyr::filter(dplyr::n_distinct(paste(RDA1, RDA2)) >= 3) %>%
      dplyr::slice(chull(RDA1, RDA2)) %>%
      dplyr::ungroup()
    
    p <- ggplot2::ggplot(sites, aes(RDA1, RDA2, colour = naturtypekode_short)) +
      ggplot2::geom_polygon(
        data = hulls,
        aes(RDA1, RDA2, fill = naturtypekode_short, group = naturtypekode_short),
        alpha = 0.20, colour = NA, inherit.aes = FALSE
      ) +
      { if (draw_points) ggplot2::geom_point(size = 1.4, alpha = 0.6) } +
      ggplot2::geom_segment(
        data = bplt_s,
        aes(x = 0, y = 0, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = grid::arrow(length = unit(0.02, "npc"))
      ) +
      ggplot2::geom_text(
        data = bplt_s,
        aes(x = xend, y = yend, label = predictor),
        inherit.aes = FALSE,
        hjust = -0.1, vjust = 0.5
      ) +
      ggplot2::geom_segment(
        data = resp_s,
        aes(x = 0, y = 0, xend = xend, yend = yend),
        inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data = resp_s,
        aes(x = xend, y = yend, label = predictor),
        inherit.aes = FALSE, fontface = "bold",
        hjust = -0.1, vjust = 0.5
      ) +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = title,
        x = sprintf("RDA1 (%.1f%%)", 100 * as.numeric(vexp[1])),
        y = sprintf("RDA2 (%.1f%%)", 100 * as.numeric(vexp[2])),
        colour = "Naturtype", fill = "Naturtype"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "right")
    
    ggplot2::ggsave(
      filename = file.path(save_dir, paste0("RDA_", stringr::str_replace_all(title, "[^[:alnum:]]+", "_"), ".png")),
      plot = p, width = 7, height = 6, dpi = 300
    )
  })
}

# ---- Run it ----
# Longest 25% of predictor arrows, no points:
make_rda_plots(models_rda, keep_q = 0.9, draw_points = FALSE, save_dir = "RDA_plots")
# OR keep exactly top 5 arrows per plot:
# make_rda_plots(models_rda, keep_top_k = 5, draw_points = FALSE)







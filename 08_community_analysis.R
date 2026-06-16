# 08_community_analysis.R
# Community-level bat activity analysis
# Input:     data_env  — cleaned detection data with all covariates
#            bat_theme — shared ggplot2 theme object
#            col_red / col_white — shared colour constants
# Returns:   named list of model objects, data frames, and figures

run_community_analysis <- function(data_env, bat_theme, col_red, col_white) {

  library(vegan)
  library(mvabund)
  library(tidyverse)

  # ── Build effort-corrected community data ──────────────────────────────────
  effort <- data_env %>%
    group_by(site, year, color, intensity) %>%
    summarise(nights = n_distinct(date), .groups = "drop")

  comm_dat <- data_env %>%
    group_by(site, year, color, intensity, species, block) %>%
    summarise(detections = sum(detections), .groups = "drop") %>%
    left_join(effort, by = c("site", "year", "color", "intensity")) %>%
    mutate(det_per_night = detections / nights)

  # ── Build community matrix and env frame ──────────────────────────────────
  # mean_phase is averaged across nights per group; site-level covariates use first()
  community_matrix <- comm_dat %>%
    select(site, year, color, intensity, species, block, det_per_night) %>%
    pivot_wider(names_from = species, values_from = det_per_night, values_fill = 0) %>%
    left_join(
      data_env %>%
        group_by(site, year, color, intensity) %>%
        summarise(
          mean_phase           = mean(mean_phase, na.rm = TRUE),
          pct_nonforest        = first(pct_nonforest),
          mean_brightness_site = first(mean_brightness_site),
          .groups = "drop"
        ),
      by = c("site", "year", "color", "intensity")
    )

  species_cols <- setdiff(
    names(community_matrix),
    c("site", "year", "color", "intensity", "mean_phase",
      "pct_nonforest", "mean_brightness_site", "block")
  )

  comm <- community_matrix %>% select(all_of(species_cols))
  env  <- community_matrix %>%
    select(site, year, color, intensity, mean_phase,
           pct_nonforest, mean_brightness_site, block) %>%
    mutate(color = factor(color), block = factor(block))

  # ── NMDS ──────────────────────────────────────────────────────────────────
  cat("Running NMDS...\n")
  nmds <- metaMDS(comm, distance = "bray", k = 2, trymax = 100, trace = FALSE)
  cat("  Stress:", round(nmds$stress, 3), "\n")

  # ── PERMANOVA (year as stratum) ────────────────────────────────────────────
  cat("Running PERMANOVA...\n")
  perm <- adonis2(
    comm ~ color + intensity + mean_phase + pct_nonforest + mean_brightness_site + block,
    data         = env,
    method       = "bray",
    permutations = 9999,
    strata       = env$year
  )

  # ── Homogeneity of dispersion ──────────────────────────────────────────────
  bc        <- vegdist(comm, method = "bray")
  bd        <- betadisper(bc, group = env$color)
  disp_test <- anova(bd)

  # ── dbRDA (year as condition) ──────────────────────────────────────────────
  cat("Running dbRDA...\n")
  db <- dbrda(
    comm ~ color + intensity + mean_phase + pct_nonforest +
      mean_brightness_site + block + Condition(year),
    data     = env,
    distance = "bray"
  )
  db_global <- anova(db, permutations = 9999)
  db_margin <- anova(db, by = "margin", permutations = 9999)

  # ── mvabund ───────────────────────────────────────────────────────────────
  cat("Running mvabund (nBoot = 9999 — may take a while)...\n")
  Y        <- mvabund(comm)
  mv_fit   <- manyglm(
    Y ~ color + intensity + mean_phase + pct_nonforest +
      mean_brightness_site + block + year,
    family = "negative.binomial",
    data   = env
  )
  mv_anova <- anova(mv_fit, p.uni = "adjusted", nBoot = 9999)

  # ── Fig C1: dbRDA ordination biplot ───────────────────────────────────────
  site_scores <- scores(db, display = "sites") |>
    as.data.frame() |>
    bind_cols(env)

  bp_mat <- scores(db, display = "bp")
  env_scores <- as.data.frame(bp_mat) |>
    mutate(
      variable = rownames(bp_mat),
      variable = dplyr::recode(variable,
        colorW               = "White light",
        intensity            = "Intensity",
        mean_phase           = "Moon phase",
        pct_nonforest        = "% Non-forest",
        mean_brightness_site = "Sky brightness",
        blockthree           = "Block: 3-hr",
        blockseven           = "Block: 7-hr"
      )
    )

  p_global <- round(db_global$`Pr(>F)`[1], 3)

  fig_c1 <- ggplot(site_scores, aes(dbRDA1, dbRDA2, color = color)) +
    geom_point(size = 2.5, alpha = 0.75) +
    geom_segment(
      data = env_scores,
      aes(x = 0, y = 0, xend = dbRDA1, yend = dbRDA2),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
      color = "#444444", linewidth = 0.6
    ) +
    ggrepel::geom_text_repel(
      data = env_scores,
      aes(dbRDA1, dbRDA2, label = variable),
      inherit.aes = FALSE,
      size = 3, color = "#222222", seed = 42
    ) +
    scale_color_manual(
      values = c(R = col_red, W = col_white),
      labels = c(R = "Red", W = "White"),
      name   = "Light color"
    ) +
    labs(
      title    = "Bat Community Composition — dbRDA",
      subtitle = paste0("Year partialled (Condition) · Global p = ", p_global),
      x        = "dbRDA1", y = "dbRDA2",
      caption  = "Bray–Curtis distance · arrows = constrained predictors"
    ) +
    bat_theme +
    theme(legend.position = "top")

  # ── Fig C2: Activity heatmap (z-scored by species) ────────────────────────
  heat_dat <- data_env %>%
    group_by(species, intensity, color) %>%
    summarise(mean_det = mean(detections), .groups = "drop") %>%
    group_by(species) %>%
    mutate(mean_det_z = as.numeric(scale(mean_det))) %>%
    ungroup() %>%
    mutate(species = as.character(species))
  
  sp_order <- heat_dat %>%
    group_by(species) %>%
    summarise(mean_z = mean(mean_det_z, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_z) %>%
    pull(species)
  
  heat_dat <- heat_dat %>%
    mutate(species = factor(species, levels = sp_order))

  fig_c2 <- ggplot(
    heat_dat,
    aes(
      x    = factor(intensity),
      y    = species,
      fill = mean_det_z
    )
  ) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(
      low      = col_red,
      mid      = "#F5F5F5",
      high     = col_white,
      midpoint = 0,
      name     = "Relative\nactivity (z)"
    ) +
    facet_wrap(
      ~ color,
      labeller = labeller(color = c(R = "Red light", W = "White light"))
    ) +
    labs(
      title   = "Species Activity by Light Intensity and Color",
      x       = "Light intensity (% of maximum)",
      y       = NULL,
      caption = "Z-scored within species across all treatment combinations"
    ) +
    bat_theme +
    theme(
      panel.grid = element_blank(),
      axis.line  = element_blank(),
      axis.ticks = element_blank()
    )

  # ── Return ─────────────────────────────────────────────────────────────────
  list(
    comm_dat   = comm_dat,
    comm       = comm,
    env        = env,
    nmds       = nmds,
    perm       = perm,
    disp_test  = disp_test,
    db         = db,
    db_global  = db_global,
    db_margin  = db_margin,
    mv_fit     = mv_fit,
    mv_anova   = mv_anova,
    fig_c1     = fig_c1,
    fig_c2     = fig_c2
  )
}

# 09_sample_size_figure.R
# Sample size (site-nights) by light intensity, colour and year — Colter Bay.
# Input:  output/sample_size_by_intensity_color.csv, output/sample_size_by_year_intensity.csv
#         (site-night counts of the modelled data, all 7 sites incl. imputed-brightness CRNR1/DALO1)
# Output: output/figures/sample_size_intensity.png

library(tidyverse)
library(patchwork)

by_int  <- read_csv("output/sample_size_by_intensity_color.csv", show_col_types = FALSE)
by_year <- read_csv("output/sample_size_by_year_intensity.csv", show_col_types = FALSE)

col_red   <- "#C0392B"
col_white <- "#2E86AB"   # blue stands in for white light (as in the qmd)

int_levels <- sort(unique(by_int$intensity))
lab_color  <- c(R = "Red", W = "White")

ss_theme <- theme_classic(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "#EBEBEB", linewidth = 0.4),
    axis.line          = element_line(color = "#444444", linewidth = 0.5),
    axis.ticks         = element_line(color = "#444444"),
    axis.text          = element_text(color = "#333333", size = 10),
    axis.title         = element_text(color = "#222222", size = 11),
    plot.title         = element_text(color = "#111111", size = 13, face = "bold",
                                      margin = margin(b = 3)),
    plot.subtitle      = element_text(color = "#666666", size = 9, margin = margin(b = 8)),
    legend.text        = element_text(color = "#333333", size = 10),
    legend.title       = element_text(color = "#444444", size = 9, face = "bold"),
    strip.background   = element_rect(fill = "#F2F2F2", color = NA),
    strip.text         = element_text(color = "#222222", face = "bold", size = 10),
    plot.margin        = margin(12, 14, 10, 12)
  )

# ── A: total site-nights per intensity, red vs white ─────────────────────────
a_dat <- by_int %>%
  mutate(intensity = factor(intensity, levels = int_levels),
         color = factor(color, levels = c("R", "W")))

p_a <- ggplot(a_dat, aes(intensity, site_nights, fill = color)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white", linewidth = 0.6) +
  # label only the dominant cell (75% red, driven by the 2021/2023 control years)
  geom_text(data = filter(a_dat, site_nights == max(site_nights)),
            aes(label = paste0(site_nights, " (control years)")),
            position = position_dodge(width = 0.8), hjust = -0.08, vjust = 0.3,
            size = 3.3, color = "#333333") +
  scale_fill_manual(values = c(R = col_red, W = col_white), labels = lab_color,
                    name = "Light colour") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "A. Site-nights per intensity",
       subtitle = "Modelled data, 7 sites, 2019–2024 (1,816 site-nights)",
       x = "Light intensity (%)", y = "Site-nights") +
  ss_theme +
  theme(legend.position = "top", legend.justification = "left")

# ── B: where each intensity comes from (year × intensity) ────────────────────
b_dat <- by_year %>%
  pivot_longer(c(R, W), names_to = "color", values_to = "site_nights") %>%
  complete(year, intensity = int_levels, color, fill = list(site_nights = 0)) %>%
  mutate(intensity = factor(intensity, levels = int_levels),
         year = factor(year, levels = rev(sort(unique(year)))),
         color = factor(color, levels = c("R", "W"), labels = lab_color),
         cell = if_else(site_nights == 0, NA_real_, site_nights))

p_b <- ggplot(b_dat, aes(intensity, year)) +
  geom_tile(aes(fill = cell), color = "white", linewidth = 1.2) +
  geom_text(aes(label = if_else(site_nights == 0, "", as.character(site_nights)),
                color = site_nights >= 40),
            size = 3, show.legend = FALSE) +
  scale_fill_gradient(low = "#DCE6EE", high = "#1B2A4A", trans = "log10",
                      na.value = "#F7F7F7", name = "Site-nights",
                      breaks = c(3, 10, 30, 100, 250)) +
  scale_color_manual(values = c(`FALSE` = "#222222", `TRUE` = "white")) +
  facet_wrap(~ color, ncol = 2) +
  labs(title = "B. Which years supply each intensity",
       subtitle = "Each intensity comes largely from one or two years, so intensity is partly confounded with year",
       x = "Light intensity (%)", y = NULL) +
  ss_theme +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(), legend.position = "right")

fig <- p_a / p_b + plot_layout(heights = c(1, 1.15))

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/sample_size_intensity.png", fig, width = 11, height = 9.5, dpi = 300)

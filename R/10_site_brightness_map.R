# =============================================================================
# 10_site_brightness_map.R
# Map of the 7 Colter Bay stations over NLCD 2021 land cover, coloured and
# labelled by sky brightness (mean_brightness_site: mean DSU of the lights-off
# sky photos, after EXCLUDE_IMAGES in python/run_sky_brightness.py).
# CRNR1 and DALO1 have no photos; they carry the value of their nearest
# photographed site (as in the qmd): CRNR1 <- SHLA1, DALO1 <- AMLA1.
# Colour scale matches the distance-project map (Distance_Project/R/41_site_brightness_map.R).
# Output: output/figures/site_brightness_map.png
# =============================================================================
suppressMessages({ library(tidyverse); library(sf); library(terra); library(ggrepel) })

# site table — same coordinates as R/02_clean_data.R (not sourced: that file also cleans data_raw)
sites <- tibble(
  site = c("CORA1", "DALO1", "CRNR1", "NORO1", "AMLA1", "SHLA1", "GSLA1"),
  lon  = c(-110.6354, -110.64717, -110.63882, -110.63902, -110.64412, -110.64197, -110.64065),
  lat  = c(43.90172, 43.90791, 43.90151, 43.90642, 43.90422, 43.90194, 43.90496)
)

sky <- read_csv("output/sky_brightness_measurements.csv", show_col_types = FALSE) %>%
  select(site, mean_brightness_site)
brightness_proxy <- c(CRNR1 = "SHLA1", DALO1 = "AMLA1")
sky <- bind_rows(
  sky %>% mutate(imputed = FALSE),
  tibble(site = names(brightness_proxy),
         mean_brightness_site = sky$mean_brightness_site[match(brightness_proxy, sky$site)],
         imputed = TRUE)
)
sites <- sites %>% left_join(sky, by = "site")
stopifnot("every site has brightness" = all(!is.na(sites$mean_brightness_site)))

# ---- NLCD land cover, cropped to the study area ----------------------------
r <- rast("Annual_NLCD_LndCov_2021_CU_C1V1.tif")
sites_sf <- st_transform(st_as_sf(sites, coords = c("lon", "lat"), crs = 4326, remove = FALSE), crs(r))

bb <- st_bbox(sites_sf); pad <- 500
rc <- crop(r, ext(bb["xmin"] - pad, bb["xmax"] + pad, bb["ymin"] - pad, bb["ymax"] + pad))
try(levels(rc) <- NULL, silent = TRUE)   # strip category table -> raw integer codes

rdf <- as.data.frame(rc, xy = TRUE); names(rdf)[3] <- "code"
rdf$cover <- factor(dplyr::case_when(
  rdf$code == 11            ~ "Water",
  rdf$code %in% 21:24       ~ "Developed",
  rdf$code %in% 41:43       ~ "Forest",
  rdf$code %in% c(51, 52)   ~ "Shrub",
  rdf$code %in% 71:74       ~ "Grassland",
  rdf$code %in% c(81, 82)   ~ "Cropland",
  rdf$code %in% c(90, 95)   ~ "Wetland",
  TRUE                      ~ "Other"),
  levels = c("Forest", "Shrub", "Grassland", "Wetland", "Water", "Developed", "Cropland", "Other"))
pal <- c(Forest = "#4E7A51", Shrub = "#C7B77A", Grassland = "#E9E2C6", Wetland = "#79B0A6",
         Water = "#A6CEE3", Developed = "#BDBDBD", Cropland = "#E8D8A0", Other = "#F0F0F0")

sc <- st_coordinates(sites_sf); sites$X <- sc[, 1]; sites$Y <- sc[, 2]
sites <- sites %>%
  mutate(label = sprintf("%s  %.1f%s", site, mean_brightness_site, if_else(imputed, "*", "")))

# scale bar (250 m) near bottom-left
xr <- range(rdf$x); yr <- range(rdf$y)
sbx <- xr[1] + 0.06 * diff(xr); sby <- yr[1] + 0.05 * diff(yr)

p <- ggplot() +
  geom_raster(data = rdf, aes(x, y, fill = cover)) +
  scale_fill_manual(values = pal, name = "Land cover", drop = TRUE) +
  geom_point(data = sites, aes(X, Y), size = 5, color = "#111111") +   # dark ring
  geom_point(data = sites, aes(X, Y, color = mean_brightness_site), size = 3.8) +
  # imputed sites: white inner ring marks the borrowed value
  geom_point(data = filter(sites, imputed), aes(X, Y), shape = 1, size = 2.2,
             color = "white", stroke = 0.9) +
  scale_color_gradient(low = "#FFF3C4", high = "#B35806", limits = c(2.6, 24.1),
                       name = "Sky brightness\n(dark, DSU)") +
  ggrepel::geom_label_repel(data = sites, aes(X, Y, label = label),
                            size = 3, color = "#111111", fill = alpha("white", 0.8), label.size = 0,
                            label.padding = unit(0.12, "lines"), box.padding = 0.6, point.padding = 0.4,
                            force = 3, max.overlaps = Inf, seed = 1, min.segment.length = 0) +
  annotate("segment", x = sbx, xend = sbx + 250, y = sby, yend = sby, linewidth = 1.1) +
  annotate("text", x = sbx + 125, y = sby, label = "250 m", vjust = -0.6, size = 3) +
  annotate("text", x = xr[2] - 0.04 * diff(xr), y = yr[2] - 0.05 * diff(yr), label = "N",
           fontface = "bold", size = 5) +
  annotate("segment", x = xr[2] - 0.04 * diff(xr), xend = xr[2] - 0.04 * diff(xr),
           y = yr[2] - 0.13 * diff(yr), yend = yr[2] - 0.075 * diff(yr),
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"), linewidth = 0.8) +
  coord_equal(expand = FALSE) +
  labs(title = "Sky brightness at the 7 Colter Bay stations",
       subtitle = paste0("Mean DSU of the lights-off sky photos; value after each site name.\n",
                         "* No photos: CRNR1 uses SHLA1's value, DALO1 uses AMLA1's (nearest photographed site).\n",
                         "Colour scale matches the distance-project map. Background: NLCD 2021 land cover."),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
        plot.subtitle = element_text(size = 10, color = "#444444"))

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("output/figures/site_brightness_map.png", p, width = 9, height = 8, dpi = 300, bg = "white")
message("saved output/figures/site_brightness_map.png  (", nrow(sites), " sites; brightness ",
        round(min(sites$mean_brightness_site), 2), "-", round(max(sites$mean_brightness_site), 2), " DSU)")

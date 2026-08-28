# =============================================================================
# Fish-GLOBIOM — manuscript figures (run 3945)
#
# Produces every figure in "Integrated assessment of food-system and land-based
# environmental impacts under blue growth scenarios" from the committed per-figure
# data in data/figure_data/ (extracted from the run-3945 GLOBIOM output by
# R/extract_figure_data.py; see README "Data availability").
#
# Run from the repository root:  Rscript R/make_figures.R
# Outputs are written to figures/ as Figure_<n>_*.png.
#
# Multi-panel figures (2, 3, 4) are assembled with ImageMagick `convert`.
# =============================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(sf); library(tmap)
})

# Paths are resolved relative to the repository root; run this script from there.
if (!dir.exists(file.path("data", "figure_data")))
  stop("Run from the repository root (data/figure_data/ not found in the working directory).")
fig_data      <- function(x) file.path("data", "figure_data", x)
figure_folder <- function(x) file.path("figures", x)
panels_dir    <- file.path(tempdir(), "fish_globiom_panels")
dir.create(panels_dir, showWarnings = FALSE)
panel_png     <- function(x) file.path(panels_dir, x)
im_assemble   <- function(args) stopifnot(system(paste("convert", args)) == 0L)

# Scenario aesthetics — colourblind-safe (Okabe-Ito); series are also distinguished
# by point shape and line style, not colour alone.
scen_cols   <- c("BAU"="#E69F00", "Barriers to Blue Growth"="#D55E00",
                 "Blue Transformation"="#0072B2", "FAO"="grey45")
scen_shapes <- c("BAU"=17, "Barriers to Blue Growth"=15, "Blue Transformation"=18, "FAO"=16)
scen_lines  <- c("BAU"="solid", "Barriers to Blue Growth"="longdash",
                 "Blue Transformation"="dotted", "FAO"="dashed")
level_order <- c("Reference 2020", "BAU", "Barriers to Blue Growth", "Blue Transformation")


# =============================================================================
# Figure 1 — Global demand for fish and livestock
# =============================================================================
f1 <- read.csv(fig_data("fig1_demand.csv"))
ggplot(f1, aes(x = YEAR, y = value / 1000, col = PUBSCEN, group = PUBSCEN)) +
  geom_line(aes(linetype = PUBSCEN)) +
  geom_point(aes(shape = PUBSCEN), size = 3) +
  ylab("Global Demand (Mt, live weight)") + xlab("Year") +
  theme_classic() +
  facet_wrap(~product_type, scales = "free_y") +
  scale_color_manual(values = scen_cols, name = "Scenario") +
  scale_shape_manual(values = scen_shapes, name = "Scenario") +
  scale_linetype_manual(values = scen_lines, name = "Scenario")
ggsave(figure_folder("Figure_1_demand.png"), width = 7, height = 3.6, dpi = 300)


# =============================================================================
# Figure S1 — Regional seafood demand (2000-2050)
#
# Regional companion to Figure 1: total seafood by region and scenario, using the
# same country -> region aggregation as Figure 5's fish panel. Regenerated from
# run-3945 (the originally submitted Figure S1 was from the earlier run).
# =============================================================================
s1 <- read.csv(fig_data("figS1_demand_regional.csv"))
s1$YEAR   <- as.numeric(s1$YEAR)
s1$Region <- str_to_title(sub("REG$", "", s1$REGION))
ggplot(s1, aes(x = YEAR, y = value / 1000, col = PUBSCEN, group = PUBSCEN)) +
  geom_line(aes(linetype = PUBSCEN)) +
  geom_point(aes(shape = PUBSCEN), size = 1.2) +
  ylab("Regional Demand (Mt, live weight)") + xlab("Year") +
  theme_classic(base_size = 11) +
  facet_wrap(~Region, scales = "free_y") +
  scale_color_manual(values = scen_cols, name = "Scenario") +
  scale_shape_manual(values = scen_shapes, name = "Scenario") +
  scale_linetype_manual(values = scen_lines, name = "Scenario")
ggsave(figure_folder("Figure_S1_demand_regional.png"), width = 12, height = 8, dpi = 300)


# =============================================================================
# Figure 2 — Production by system (a) and production trends (b)
# =============================================================================
lookup <- read.csv(file.path("data", "lookup_table.csv"))
sysnames <- c(BOVID = "Cattle", SHOAT = "Sheep & Goat", PIG = "Pig", POULTRY = "Poultry",
              CATCH_FW = "Capture (Freshwater)", CATCH = "Capture (Marine)",
              AQUA_FW = "Aquaculture (Freshwater)", AQUA_M = "Aquaculture (Marine)")
system_colors <- c("Capture (Freshwater)"="#66c2a5", "Capture (Marine)"="#1b9e77",
                   "Aquaculture (Freshwater)"="#2c7fb8", "Aquaculture (Marine)"="lightblue",
                   "Cattle"="pink4", "Sheep & Goat"="tan3", "Pig"="#e9a3c9", "Poultry"="#f1b6da")

prod <- read.csv(fig_data("fig2a_production.csv")) %>%
  left_join(lookup %>% select(ITEM, SYST), by = "ITEM") %>%
  mutate(SYST = ifelse(ITEM == "SGMEAT", "SHOAT", SYST)) %>%
  group_by(ALLSCEN3, SYST) %>%
  summarise(prod_kt = sum(OUTPUT, na.rm = TRUE), .groups = "drop") %>%
  mutate(PUBSYST = factor(sysnames[SYST],
         levels = c("Cattle", "Sheep & Goat", "Pig", "Poultry", "Capture (Freshwater)",
                    "Capture (Marine)", "Aquaculture (Freshwater)", "Aquaculture (Marine)")))
p2a <- ggplot(prod, aes(x = ALLSCEN3, y = prod_kt / 1000, fill = PUBSYST)) +
  geom_bar(stat = "identity", width = 0.7, position = "stack") +
  scale_fill_manual(values = system_colors, name = "") +
  labs(title = "a) Global Food Production by System", x = NULL, y = "Production (Mt, live weight)") +
  scale_x_discrete(limits = level_order) + theme_classic(base_size = 13) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold", size = 13))
ggsave(panel_png("fig2a.png"), p2a, width = 8, height = 5, dpi = 300)

tr <- read.csv(fig_data("fig2b_trends.csv"))
tr$YEAR <- as.numeric(tr$YEAR)
p2b <- ggplot(tr) +
  geom_line(aes(x = YEAR, y = output / 1000, linetype = Source, group = PUBSCEN, col = PUBSCEN)) +
  geom_point(aes(x = YEAR, y = output / 1000, col = PUBSCEN, shape = PUBSCEN), size = 2) +
  theme_classic(base_size = 13) +
  scale_color_manual(values = scen_cols, name = "Scenario") +
  scale_shape_manual(values = scen_shapes, name = "Scenario") +
  scale_linetype_manual(values = c("FAO" = "dashed", "This Study" = "solid"), name = "Source") +
  labs(title = "b) Production Trends Over Time", y = "Production (Mt)", x = NULL) +
  facet_wrap(~TYPE, ncol = 1, scales = "free") +
  theme(plot.title = element_text(face = "bold", size = 13))
ggsave(panel_png("fig2b.png"), p2b, width = 8, height = 7, dpi = 300)

im_assemble(sprintf('-append "%s" "%s" "%s"',
                    panel_png("fig2a.png"), panel_png("fig2b.png"),
                    figure_folder("Figure_2_production.png")))


# =============================================================================
# Figure 3 — Feed crop usage (a) and aquafeed production in 2050 (b)
# =============================================================================
ramp <- colorRampPalette(c("#2c7fb8", "#66c2a5", "#E69F00", "#e9a3c9", "#0072B2", "#D55E00", "#009E73"))
# Readable ingredient names in place of internal GLOBIOM codes.
crop_lab <- c(BARL="Barley", BEAD="Beans (dry)", CASS="Cassava", CHKP="Chickpea", CORN="Maize",
  CORN_DG="Maize DDGS", COTT="Cottonseed", GNUT="Groundnut", MILL="Millet", POTA="Potato",
  RAPE="Rapeseed", RAPE_ML="Rapeseed meal", RICE="Rice", SOYA="Soybean", SOYA_ML="Soybean meal",
  SRGH="Sorghum", SUGC="Sugarcane", SUNF="Sunflower", SWPO="Sweet potato", WHEA="Wheat", WHEA_DG="Wheat DDGS")
feed_lab <- c(Corn="Maize", FSHM="Fishmeal", FSHO="Fish oil", Rape="Rapeseed", Soya="Soybean", Whea="Wheat")
relabel  <- function(x, map) { y <- unname(map[as.character(x)]); ifelse(is.na(y), as.character(x), y) }

feed <- read.csv(fig_data("fig3a_feed.csv")) %>%
  mutate(ALLSCEN3 = factor(ALLSCEN3, levels = level_order), Crop = relabel(ITEM, crop_lab))
p3a <- ggplot(feed, aes(x = ALLSCEN3, y = OUTPUT / 1000, fill = Crop)) +
  geom_col(width = 0.7, position = "stack") +
  scale_fill_manual(values = ramp(length(unique(feed$Crop))), name = "Feed crop") +
  labs(title = "a) Global Feed Crop Usage", x = NULL, y = "Feed Production (Mt)") +
  scale_x_discrete(limits = level_order) + theme_classic(base_size = 13) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold", size = 13))
ggsave(panel_png("fig3a.png"), p3a, width = 8.5, height = 5, dpi = 300)

af <- read.csv(fig_data("fig3b_aquafeed.csv")) %>%
  filter(value != 0) %>%
  mutate(PUBSCEN = factor(PUBSCEN, levels = level_order), Feed = relabel(PRODUCT, feed_lab))
p3b <- ggplot(af) +
  geom_col(aes(x = PUBSCEN, y = value / 1000, fill = Feed)) +
  scale_x_discrete(limits = level_order) + theme_classic(base_size = 13) +
  scale_fill_manual(values = ramp(length(unique(af$Feed))), name = "Aquafeed ingredient") +
  labs(title = "b) Aquafeed Production in 2050", x = NULL, y = "Aquafeed Production (Mt)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold", size = 13))
ggsave(panel_png("fig3b.png"), p3b, width = 8.5, height = 5, dpi = 300)

im_assemble(sprintf('-append "%s" "%s" "%s"',
                    panel_png("fig3a.png"), panel_png("fig3b.png"),
                    figure_folder("Figure_3_feed.png")))


# =============================================================================
# Figure 4 — Land, emissions, water and nitrogen impacts (2050)
#
# Uses the repo's glob.plot() (stacked delta-from-baseline bars); the four panels
# are assembled into a 2x2 grid.
# =============================================================================
source(file.path("R", "functions", "glob_plot.R"))

# glob.plot() expects a global scen_pub_names (ALLSCEN3 -> PUBSCEN).
scen_pub_names <- data.frame(
  ALLSCEN3 = c("SCEN_DIET0_CULTURE0_CAPTURE0", "SCEN_DIET-10_CULTURE0_CAPTURE-10",
               "SCEN_DIET+10_CULTURE+50_CAPTURE+10"),
  PUBSCEN  = c("BAU", "Barriers to Blue Growth", "Blue Transformation"),
  stringsAsFactors = FALSE)
reference_scenario <- "SCEN_DIET0_CULTURE0_CAPTURE0"

df4 <- read.csv(fig_data("fig4_impacts.csv"), check.names = FALSE, stringsAsFactors = FALSE)
df4$YEAR <- as.numeric(df4$YEAR)

land_col <- c("#009E73", "#E69F00", "#CC79A7", "#56B4E9", "#D55E00")
emis_col <- c("#D55E00", "#E69F00", "#0072B2", "#CC79A7")
watr_col <- c("#56B4E9")
frtn_col <- c("#009E73")

p4 <- list(
  land = glob.plot(scen_baseline = reference_scenario, glob_df = df4, var_id = "LAND",
                   colors_plot = land_col, title = "a. Land Cover in ", save = FALSE),
  emis = glob.plot(scen_baseline = reference_scenario, glob_df = df4, var_id = "EMIS",
                   colors_plot = emis_col, title = "b. Emissions in ", save = FALSE),
  watr = glob.plot(scen_baseline = reference_scenario, glob_df = df4, var_id = "WATR",
                   colors_plot = watr_col, title = "c. Water Use in ", save = FALSE),
  frtn = glob.plot(scen_baseline = reference_scenario, glob_df = df4, var_id = "FRTN",
                   colors_plot = frtn_col, title = "d. Nitrogenous Fertilizer in ", save = FALSE)
)
for (nm in names(p4)) ggsave(panel_png(paste0("fig4_", nm, ".png")), p4[[nm]], width = 8, height = 6, dpi = 300)
im_assemble(sprintf('\\( "%s" "%s" +append \\) \\( "%s" "%s" +append \\) -append "%s"',
                    panel_png("fig4_land.png"), panel_png("fig4_emis.png"),
                    panel_png("fig4_watr.png"), panel_png("fig4_frtn.png"),
                    figure_folder("Figure_4_impacts.png")))


# =============================================================================
# Figure 5 — Regional maps (cultivated land, natural land, fish production)
#
# Blue Transformation and Barriers to Blue Growth, each mapped against BAU.
# =============================================================================
reg_map <- st_read(file.path("data", "regions", "Reg37_aggregate.shp"), quiet = TRUE) %>%
  mutate(Region37 = toupper(Region37))
land <- read.csv(fig_data("fig5_land.csv"), stringsAsFactors = FALSE)
fish <- read.csv(fig_data("fig5_fish.csv"), stringsAsFactors = FALSE)
base_scen <- "SCEN_DIET0_CULTURE0_CAPTURE0"
divpal <- c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7",
            "#d1e5f0", "#92c5de", "#4393c3", "#2166ac")
pubname <- c("SCEN_DIET+10_CULTURE+50_CAPTURE+10" = "Blue Transformation",
             "SCEN_DIET-10_CULTURE0_CAPTURE-10"   = "Barriers to Blue Growth")

land_delta <- function(scen, items) {
  land %>% filter(ITEM %in% items) %>%
    group_by(REGION, ALLSCEN3) %>% summarise(OUTPUT = sum(OUTPUT), .groups = "drop") %>%
    pivot_wider(names_from = ALLSCEN3, values_from = OUTPUT) %>%
    mutate(delta = .data[[scen]] - .data[[base_scen]]) %>% select(REGION, delta)
}
fish_delta <- function(scen) {
  fish %>% pivot_wider(names_from = ALLSCEN3, values_from = OUTPUT) %>%
    mutate(delta = .data[[scen]] - .data[[base_scen]]) %>% select(REGION, delta)
}
map_panel <- function(d, title, legend) {
  reg_map %>% left_join(d, by = c("Region37" = "REGION")) %>%
    tm_shape(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m") +
    tm_polygons(fill = "delta",
                fill.scale = tm_scale_continuous(values = divpal, midpoint = 0, value.na = "grey85"),
                fill.legend = tm_legend(title = legend, orientation = "landscape",
                                        frame = FALSE, bg.color = NA),
                col = "white", lwd = 0.4) +
    tm_title(title, size = 1.1) +
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "bottom",
              legend.frame = FALSE, legend.bg.color = NA,
              outer.margins = 0, inner.margins = c(0.02, 0, 0.02, 0), asp = 0)
}

for (scen in names(pubname)) {
  pub <- pubname[[scen]]
  a  <- map_panel(land_delta(scen, "Managed Land"),
                  paste0(pub, "\na. Cultivated land"), "Δ cultivated land vs Baseline (1000 ha)")
  b  <- map_panel(land_delta(scen, c("FOREST", "OTHNATVEG")),
                  "b. Natural land", "Δ natural land vs Baseline (1000 ha)")
  cc <- map_panel(fish_delta(scen),
                  "c. Fish production", "Δ fish production vs Baseline (1000 t)")
  out <- tmap_arrange(a, b, cc, ncol = 1)
  tmap_save(out, filename = figure_folder(paste0("Figure_5_", gsub(" ", "_", pub), "_map.png")),
            width = 6.5, height = 11, dpi = 200)
}


# =============================================================================
# Figure 7 (main) and Figure S2 (supplement) — FMFO source shift (2000-2050)
# =============================================================================
d <- read.csv(fig_data("fig7_s2_fmfo_sources.csv"), stringsAsFactors = FALSE, check.names = FALSE)
lev <- c("Whole pelagic/marine fish", "Whole non-pelagic/marine fish", "Processing waste (trimmings)")
d$source <- factor(d$source, levels = lev)
fmfo_cols   <- c("Whole pelagic/marine fish"="#0072B2", "Whole non-pelagic/marine fish"="#56B4E9",
                 "Processing waste (trimmings)"="#E69F00")
fmfo_shapes <- c("Whole pelagic/marine fish"=16, "Whole non-pelagic/marine fish"=17,
                 "Processing waste (trimmings)"=15)
fmfo_lines  <- c("Whole pelagic/marine fish"="solid", "Whole non-pelagic/marine fish"="longdash",
                 "Processing waste (trimmings)"="dotted")

# Figure 7: the two sources shown in the main text (lines, proportion axis)
d7 <- d %>% filter(source %in% c("Whole pelagic/marine fish", "Processing waste (trimmings)"))
p7 <- ggplot(d7, aes(x = year, y = proportion, colour = source, shape = source, linetype = source)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2.6, stroke = 0.7) +
  scale_colour_manual(values = fmfo_cols, name = "Source") +
  scale_shape_manual(values = fmfo_shapes, name = "Source") +
  scale_linetype_manual(values = fmfo_lines, name = "Source") +
  scale_x_continuous(breaks = seq(2000, 2050, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Year", y = "Proportion of FMFO biomass") +
  theme_classic(base_size = 12) +
  theme(legend.position = "right", legend.key.width = unit(1.6, "lines"))
ggsave(figure_folder("Figure_7_fmfo_sources.png"), p7, width = 7.4, height = 4.0, dpi = 300)

# Figure S2: all three sources as a stacked proportion bar
pS2 <- ggplot(d, aes(x = factor(year), y = proportion, fill = source)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = fmfo_cols, name = "Source") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Year", y = "Proportion of FMFO biomass") +
  theme_classic(base_size = 12) + theme(legend.position = "right")
ggsave(figure_folder("Figure_S2_fmfo_sources.png"), pS2, width = 7.6, height = 4.2, dpi = 300)


# =============================================================================
# Figure S3 — FMFO inclusion in aquafeed by species group under BAU (2000-2050)
# =============================================================================
s3 <- read.csv(fig_data("figS3_fmfo_inclusion.csv"), stringsAsFactors = FALSE)
grp_order <- c("Salmonids", "Marine finfish", "Shrimp", "Other crustaceans", "Freshwater fish")
s3$group <- factor(s3$group, levels = grp_order)
grp_cols   <- c("Salmonids"="#0072B2", "Marine finfish"="#009E73", "Shrimp"="#D55E00",
                "Other crustaceans"="#E69F00", "Freshwater fish"="#CC79A7")
grp_shapes <- c("Salmonids"=16, "Marine finfish"=17, "Shrimp"=15, "Other crustaceans"=18, "Freshwater fish"=4)
grp_lines  <- c("Salmonids"="solid", "Marine finfish"="longdash", "Shrimp"="dotdash",
                "Other crustaceans"="dotted", "Freshwater fish"="twodash")
pS3 <- ggplot(s3, aes(x = year, y = fmfo_pct, colour = group, shape = group, linetype = group)) +
  geom_vline(xintercept = 2020, colour = "grey70", linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 2011, y = Inf, label = "observed",  vjust = 1.6, size = 3, colour = "grey45") +
  annotate("text", x = 2035, y = Inf, label = "projected", vjust = 1.6, size = 3, colour = "grey45") +
  geom_line(linewidth = 0.7) + geom_point(size = 2.6, stroke = 0.7) +
  scale_colour_manual(values = grp_cols, name = "Species group") +
  scale_shape_manual(values = grp_shapes, name = "Species group") +
  scale_linetype_manual(values = grp_lines, name = "Species group") +
  scale_x_continuous(breaks = seq(2000, 2050, 10)) +
  labs(x = "Year", y = "Fishmeal and fish oil share of\nformulated (crop + FMFO) feed (%)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "right", legend.key.width = unit(1.6, "lines"))
ggsave(figure_folder("Figure_S3_fmfo_inclusion.png"), pS3, width = 7.2, height = 4.2, dpi = 300)

cat("Wrote figures to", figure_folder(""), "\n")

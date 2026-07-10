# scratch_geods_ahah_test.R - 30-minute spike: does a GeoDS product (AHAH v5)
# add anything to the KS2 models beyond the existing census context?
# AHAH = Access to Healthy Assets & Hazards (LSOA21 level, open licence).
# Higher AHAH percentile = less healthy environment.
suppressMessages({library(tidyverse); library(lme4); library(performance)})

cache_dir <- here::here("data", "cache")

# ---- 1. LSOA 2021 population-weighted centroids (keyset pagination) ----
lsoa_pwc_path <- file.path(cache_dir, "lsoa_pwc_2021.rds")
if (!file.exists(lsoa_pwc_path)) {
  message("Fetching LSOA centroids ...")
  base <- paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
                 "LSOA_PopCentroids_EW_2021_V4/FeatureServer/0/query")
  pages <- list(); last_fid <- 0
  repeat {
    url <- paste0(base, "?where=", utils::URLencode(paste0("FID>", last_fid), reserved = TRUE),
                  "&outFields=LSOA21CD,FID&orderByFields=FID&returnGeometry=true&outSR=27700",
                  "&resultRecordCount=2000&f=json")
    resp <- jsonlite::fromJSON(url)
    feats <- resp$features
    if (is.null(feats) || length(feats) == 0 || nrow(feats) == 0) break
    pages[[length(pages) + 1]] <- tibble(lsoa21cd = feats$attributes$LSOA21CD,
                                         easting = feats$geometry$x,
                                         northing = feats$geometry$y)
    last_fid <- max(feats$attributes$FID)
    if (nrow(feats) < 2000) break
    Sys.sleep(0.2)
  }
  saveRDS(bind_rows(pages), lsoa_pwc_path)
}
lsoa <- readRDS(lsoa_pwc_path)
message("LSOA centroids: ", nrow(lsoa))

# ---- 2. AHAH joined to centroids ----
ahah <- read_csv(file.path(cache_dir, "geods_ahah_v5.csv"), show_col_types = FALSE) %>%
  select(lsoa21cd, ahah_pct, domain_h_pct, domain_g_pct, domain_e_pct, domain_r_pct) %>%
  inner_join(lsoa, by = "lsoa21cd") %>%
  filter(!is.na(easting))
message("AHAH LSOAs with centroids: ", nrow(ahah))

# ---- 3. Attach to KS2 schools (5 nearest LSOAs, mean percentile) ----
ks2_urns <- readRDS(here::here("data", "ks2", "ks2_panel.rds")) %>% distinct(URN)
gias <- readRDS(here::here("data", "gias", "gias_establishments.rds")) %>%
  mutate(easting = suppressWarnings(as.numeric(easting)),
         northing = suppressWarnings(as.numeric(northing)))
schools <- ks2_urns %>%
  left_join(gias %>% select(URN = urn, easting, northing), by = "URN") %>%
  filter(!is.na(easting), easting > 0)

nn <- nabor::knn(as.matrix(ahah[, c("easting", "northing")]),
                 as.matrix(schools[, c("easting", "northing")]), k = 5)
vars <- c("ahah_pct", "domain_h_pct", "domain_g_pct", "domain_e_pct", "domain_r_pct")
mat <- as.matrix(ahah[, vars])
school_ahah <- map_dfc(seq_along(vars), function(j) {
  tibble(!!vars[j] := rowMeans(matrix(mat[nn$nn.idx, j], nrow = nrow(schools))))
}) %>% bind_cols(schools["URN"], .)
message("Schools with AHAH: ", nrow(school_ahah))

# ---- 4. Model test: Phase F census spec +/- AHAH ----
zs <- function(x) as.numeric(scale(x))
OF <- c("1"="Outstanding","2"="Good","3"="Requires improvement","4"="Inadequate")
ctx <- readRDS(here::here("data", "census", "school_census_context.rds"))

d <- readRDS(here::here("data", "ks2", "ks2_panel.rds")) %>%
  filter(MINORGROUP %in% c("Academy", "Maintained school"),
         !is.na(READ_AVERAGE), !is.na(PERCTOT), PERCTOT > 0, !is.na(PPERSABS10),
         !is.na(PTFSM6CLA1A), !is.na(PNUMEAL), !is.na(PTMOBN),
         !is.na(TOTPUPS), TOTPUPS > 0, !is.na(LANAME)) %>%
  mutate(ofsted = factor(OF[as.character(OFSTEDRATING_external)], levels = OF)) %>%
  inner_join(ctx %>% select(URN, cens_l4plus_idw10, cens_ltsick_idw10,
                            cens_unemp_idw10, mean_dist_k10), by = "URN") %>%
  inner_join(school_ahah, by = "URN") %>%
  filter(!is.na(ofsted)) %>%
  mutate(z_read = zs(READ_AVERAGE), z_abs = zs(PERCTOT), z_pers = zs(PPERSABS10),
         z_fsm = zs(PTFSM6CLA1A), z_eal = zs(PNUMEAL), z_size = zs(TOTPUPS),
         z_mob = zs(PTMOBN), z_l4plus = zs(cens_l4plus_idw10),
         z_ltsick = zs(cens_ltsick_idw10), z_cunemp = zs(cens_unemp_idw10),
         z_dist = zs(log(mean_dist_k10)),
         z_ahah = zs(ahah_pct), z_env = zs(domain_e_pct),
         z_green = zs(domain_g_pct), z_retail = zs(domain_r_pct))

cat("\nModel rows:", nrow(d), "\n")
cat("Bivariate r with reading: ahah", round(cor(d$READ_AVERAGE, d$ahah_pct, use="pair"), 2),
    "| env", round(cor(d$READ_AVERAGE, d$domain_e_pct, use="pair"), 2),
    "| green", round(cor(d$READ_AVERAGE, d$domain_g_pct, use="pair"), 2),
    "| retail", round(cor(d$READ_AVERAGE, d$domain_r_pct, use="pair"), 2), "\n\n")

f0 <- lmer(z_read ~ z_abs + z_pers + z_fsm + z_eal + z_size + z_mob + ofsted +
             z_l4plus + z_ltsick + z_cunemp + z_dist + year_label + (1 | LANAME),
           data = d, REML = FALSE)
f1 <- update(f0, . ~ . + z_ahah)                          # overall index
f2 <- update(f0, . ~ . + z_env + z_green + z_retail)      # domains (health-access domain
                                                          # overlaps census; excluded)
for (m in list(`census (Phase F)` = f0, `+ AHAH overall` = f1, `+ AHAH domains` = f2)) {}
res <- map_dfr(list(`census (Phase F)` = f0, `+ AHAH overall` = f1, `+ AHAH domains` = f2),
               function(m) {
                 r <- suppressWarnings(r2(m))
                 tibble(marginal = round(r$R2_marginal, 3),
                        conditional = round(r$R2_conditional, 3),
                        AIC = round(AIC(m)))
               }, .id = "model")
print(as.data.frame(res))

cat("\nAHAH overall coefficient:",
    round(summary(f1)$coefficients["z_ahah", "Estimate"], 3),
    "(t =", round(summary(f1)$coefficients["z_ahah", "t value"], 1), ")\n")
cat("Domain coefficients (f2):\n")
print(round(summary(f2)$coefficients[c("z_env", "z_green", "z_retail"), c(1, 3)], 3))

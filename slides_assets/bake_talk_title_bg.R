# Title background for bh_schools_talk_2026.qmd: the catchment map from
# the "Ten schools, and where the children live" slide, without titles or
# legend, under the same purple 110-degree fade as bake_title_bg.py.

BHS <- "E:/bh_school_system"
core <- readLines(file.path(BHS, "R", "00_core.R"), encoding = "UTF-8", warn = FALSE)
eval(parse(text = sub("here::here()", "BHS", core, fixed = TRUE), encoding = "UTF-8"))

oi    <- bh_data("open_inputs.rds")
lsoa  <- bh_data("lsoa.geojson") |> sf::st_transform(4326)
catch <- bh_data("catchments_current.geojson") |> sf::st_transform(4326)
sch   <- schools_sf(oi$schools) |> dplyr::filter(name != "Peacehaven Community School")

kid_n <- oi$zones |> dplyr::group_by(lsoa) |> dplyr::summarise(children = sum(Oi), .groups = "drop")
kid_pts <- lsoa |> dplyr::inner_join(kid_n, by = c("lsoa21cd" = "lsoa")) |>
  sf::st_point_on_surface() |> suppressWarnings()

# The map sits to the right, where the fade is lightest; the title text
# sits over the darker left side.
map <- ggplot() +
  geom_sf(data = catch, aes(fill = catchment), alpha = 0.35, colour = "grey30", linewidth = 0.5) +
  geom_sf(data = kid_pts, aes(size = children), colour = "#1f4e79", alpha = 0.4) +
  geom_sf(data = sch, shape = 21, fill = "white", colour = "black", size = 5, stroke = 1.5) +
  ggrepel::geom_text_repel(data = sch, aes(lon, lat, label = short_sch(name)), size = 6,
                           colour = "#1f3b57", seed = 2, box.padding = 0.6, min.segment.length = 0) +
  scale_fill_manual(values = CATCH_COLOURS, guide = "none") +
  scale_size_area(max_size = 10, guide = "none") +
  coord_sf(xlim = c(-0.36, -0.01), ylim = c(50.795, 50.89), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#FAFAFA", colour = NA))

fade <- grid::rectGrob(gp = grid::gpar(col = NA, fill = grid::linearGradient(
  colours = grDevices::rgb(58, 24, 87, alpha = 255 * c(0.97, 0.93, 0.70, 0.40, 0.18),
                           maxColorValue = 255),
  stops = c(0, 0.42, 0.62, 0.80, 1),
  # 110 degrees: left to right, tilted slightly downwards
  x1 = 0.5 - 0.47, y1 = 0.5 + 0.17, x2 = 0.5 + 0.47, y2 = 0.5 - 0.17)))

out <- "E:/school_attainment_tool/slides_assets/title-bg-talk.png"
ragg::agg_png(out, width = 1920, height = 1080, res = 110, background = "#FAFAFA")
print(map)
grid::grid.draw(fade)
invisible(dev.off())
cat("wrote", out, "\n")

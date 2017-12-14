#' Color Palletes Usefull for NLCD and NED plotting
#'
#' Color pallets for visualizing land use and elevation raster data
#'
#' @examples
#' plot(SB_nlcd_2001, col = col_lc)
#' plot(SB_ned_13, col = col_elev)
#'
#' @author
#' Mike Johnson

col_elev<-c("#06407F",
            "#317A9D",
            "#4ABEBB",
            "#40AE89",
            "#467B5D",
        "#3C6D4D", "#1A572E", "#034C00", "#045D03", "#6C975F", "#6B823A",
        "#88A237", "#C5D16B", "#DDE580", "#FFF6AE", "#FBCB81", "#F0B16A",
        "#F2B16D", "#D18338", "#B16F33", "#825337", "#66422A", "#4F2C0C")

col_lc <- data.frame(
  class = c("water", "water",
            "developed", "developed", "developed", "developed",
            "barren",
            "forest", "forest", "forest",
            "shrubland", "shrubland",
            "herbaceous", "herbaceous", "herbaceous", "herbaceous",
            "planted", "planted",
            "wetlands", "wetlands",
            NA),
  code = c(11, 12,
           21, 22, 23, 24,
           31,
           41, 42, 43,
           51, 52,
           71, 72, 73, 74,
           81, 82,
           90, 95,
           0),
  description = c(
    "Open Water", "Perennial Ice/Snow",
    "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, Medium Intensity",
    "Barren Land (Rock/Sand/Clay)",
    "Deciduous Forest", "Deciduous Forest", "Mixed Forest",
    "Dwarf Scrub", "Scrub/Shrub",
    "Grassland/Herbaceous", "Sedge/Herbaceuous", "Lichens", "Moss",
    "Pasture/Hay", "Sedge/Herbaceuous",
    "woodywetlands", "Emergent Herbaceous Wetlands",
    "none"),
  color = c(
    "#5475A8", "#FFFFFF",
    "#E29E8C", "#FF0000", "#B50000", "#D2CDC0",
    "#85C77E",
    "#38814E", "#38814E", "#AF963C",
    "#DCCA8F", "#FDE9AA",
    "#D1D182", "#A3CC51", "#82BA9E", "#82BA9E",
    "#82BA9E", "#FBF65D",
    "#CA9146", "#CA9146",
    NA),
  stringsAsFactors = FALSE)

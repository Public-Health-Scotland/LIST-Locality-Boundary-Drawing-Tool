load_2022_dz <- function() {
  datazone_shapefiles_dir <- path("/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/")

  dz2022_sf <- read_sf(path(datazone_shapefiles_dir, "Data Zone Boundaries 2022", "SG_DataZone_Bdry_2022.shp")) %>%
    dplyr::select(dz2022 = dzcode, Shape_Area = st_area_sh)

  dz2022_info_lookup <- get_spd(col_select = c("hb2019", "hb2019name", "hscp2019", "hscp2019name", "intzone2022", "intzone2022name", "datazone2022", "datazone2022name", "pop2022")) %>%
    summarise(
      pop2022 = sum(pop2022, na.rm = TRUE), ,
      .by = c("hb2019", "hb2019name", "hscp2019", "hscp2019name", "intzone2022", "intzone2022name", "datazone2022", "datazone2022name")
    ) %>%
    rename(dz2022 = datazone2022, dz2022name = datazone2022name)

  full_dz2022_shapefile <- dz2022_sf %>%
    full_join(dz2022_info_lookup, by = c("dz2022"), relationship = "one-to-one") %>%
    dplyr::select(
      hb2019, hb2019name,
      hscp2019, hscp2019name,
      intzone2022, intzone2022name,
      dz2022, dz2022name,
      pop2022, Shape_Area
    ) %>%
    st_transform(4326)

  return(full_dz2022_shapefile)
}

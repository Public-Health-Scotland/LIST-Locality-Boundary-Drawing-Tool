load_2011_dz <- function() {
  lookups_dir <- path("/conf/linkage/output/lookups/Unicode/Geography")

  dz2011_shapefiles_dir <- path(lookups_dir, "Shapefiles")

  locality_lookup_dir <- path(lookups_dir, "HSCP Locality")

  dz2011_pcd <- get_spd(col_select = c("hb2019", "hb2019name", "hscp2019", "hscp2019name", "intzone2011", "datazone2011", "pop2022")) %>%
    summarise(
      pop2022 = sum(pop2022, na.rm = TRUE), ,
      .by = c("hb2019", "hb2019name", "hscp2019", "hscp2019name", "intzone2011", "datazone2011")
    ) %>%
    rename(dz2011 = datazone2011)

  dz2011_locality_lookup <- path(locality_lookup_dir, "HSCP Localities_DZ11_Lookup_20240513.rds") %>%
    readRDS() %>%
    dplyr::select(dz2011 = datazone2011, hscp_locality)


  dz2011_shapefiles <- path(
    dz2011_shapefiles_dir,
    "Data Zones 2011", "SG_DataZone_Bdry_2011.shp"
  ) %>%
    read_sf() %>%
    mutate(Shape_Area = st_area(geometry)) %>%
    select(dz2011 = DataZone, Shape_Area) %>%
    # converts the shapefile to use latitude and longitude
    st_transform(4326) %>%
    left_join(dz2011_locality_lookup, by = "dz2011") %>%
    left_join(dz2011_pcd, by = "dz2011") %>%
    select(hb2019, hb2019name, hscp2019, hscp2019name, hscp_locality, intzone2011, dz2011, Shape_Area, pop2022) %>%
    st_transform(4326)

  return(dz2011_shapefiles)
}

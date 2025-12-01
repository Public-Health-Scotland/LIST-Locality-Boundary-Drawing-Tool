
load_2022_dz <- function(){
  
  lookups_dir <- path("/conf/linkage/output/lookups/Unicode")
  datazone_shapefiles_dir <- path(lookups_dir, "Geography/Shapefiles/")
  PCD_dir <- path(lookups_dir,"Geography/Scottish Postcode Directory/")
  
  dz2022_sf <- read_sf(path(datazone_shapefiles_dir,"Data Zone Boundaries 2022","SG_DataZoneBdry_2022_MHW.shp")) %>%
    dplyr::select(dz2022=DZCode,dz2022name=DZName,Shape_Area)
  
  dz2022_info_lookup <- list.files(PCD_dir,full.names = TRUE) %>%
    data.frame(filename=.) %>%
    mutate(correct_files=grepl("Scottish_Postcode_Directory_",filename) & grepl(".rds",filename)) %>%
    filter(correct_files) %>%
    mutate(year=map_chr(str_extract_all(filename,"\\(?[0-9]+\\)?"),1),
           file_no = map_chr(str_extract_all(filename,"\\(?[0-9]+\\)?"),2)) %>%
    filter(year == max(year),
           file_no == max(file_no)) %>%
    pull(filename) %>%
    readRDS() %>%
    summarise(
      pop2022 = sum(pop2022,na.rm=TRUE),
      ,.by=c("hb2019","hb2019name","hscp2019","hscp2019name","intzone2022","intzone2022name","datazone2022","datazone2022name")
    ) %>%
    rename(dz2022=datazone2022,dz2022name=datazone2022name)
  
  full_dz2022_shapefile <- dz2022_sf %>%
    left_join(dz2022_info_lookup,by=c("dz2022","dz2022name"),relationship="one-to-one") %>%
    dplyr::select(
      
      hb2019,hb2019name,
      hscp2019,hscp2019name,
      intzone2022,intzone2022name,
      dz2022,dz2022name,
      pop2022,Shape_Area
      
    )
  
  return(full_dz2022_shapefile)
  
}
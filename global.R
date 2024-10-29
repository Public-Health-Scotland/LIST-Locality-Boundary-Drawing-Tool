
library(shiny)
library(flexdashboard)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(leaflet.extras)
library(flextable)
library(tidyverse)
library(readxl)
library(phsmethods)
library(phsstyles)
library(shinyWidgets)
library(sf)
library(fs)

# 0. Fix Geometry Options

sf_use_s2(FALSE)

# 1. Directories ----

lookups_dir <- path("/conf/linkage/output/lookups/Unicode")

proposed_datazone_shapefiles_dir <- path("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/DZ_IZ_2022_Consultation/")

current_datazone_sizes_dir <- path(lookups_dir,"Populations/Estimates")

current_datazones_locality_lookup_dir <- path(lookups_dir,"Geography/HSCP Locality")

current_datazone_shapefiles_dir <- path(lookups_dir, "Geography/Shapefiles/")


# 2. Lookups ----

## 2.1 Current Info ----

### 2.1.1 Data Zones ----

current_datazone_names <- arrow::read_parquet(
  file = path(lookups_dir, "Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_1.parquet"),
  col_select = starts_with("datazone2011"),
  as_data_frame = FALSE
) %>%
  distinct() %>%
  collect()

current_datazone_sizes <- path(current_datazone_sizes_dir,"DataZone2011_pop_est_2011_2021.rds") %>%
  read_rds() %>%
  filter(year==2021) %>%
  distinct(datazone2011,total_pop) %>%
  summarise(TotalPop = sum(total_pop),
            .by=c("datazone2011")) %>%
  rename(DataZone=datazone2011)


current_datazones_localities <- path(current_datazones_locality_lookup_dir,"HSCP Localities_DZ11_Lookup_20240513.rds") %>%
  readRDS() %>%
  dplyr::select(DataZone=datazone2011,Locality=hscp_locality)


current_datazone_shapefiles <- path(
  current_datazone_shapefiles_dir,
  "Data Zones 2011", "SG_DataZone_Bdry_2011.shp"
) %>% 
  read_sf() %>%
  select(DataZone,InterZone=intzone201, HSCP_name=hscp2019na,Shape_Area) %>%
  # converts the shapefile to use latitude and longitude
  st_transform(4326) %>%
  left_join(current_datazone_sizes,by="DataZone") %>%
  left_join(current_datazones_localities,by="DataZone") %>%
  mutate(HSCP_name=ifelse(HSCP_name == "Edinburgh","City Of Edinburgh",HSCP_name))

### 2.1.2. Localities ----

current_locality_sizes <- current_datazone_shapefiles %>%
  st_drop_geometry() %>%
  summarise(TotalPop=sum(TotalPop),
            .by="Locality") %>%
  rename(hscp_local=Locality)

current_locality_shapefiles <- path(current_datazone_shapefiles_dir, "HSCP Locality (Datazone2011 Base)/HSCP_Locality.shp") %>%
  read_sf() %>%
  dplyr::select(hscp_local, HSCP_name, HSCP,Shape_Area) %>%
  st_transform(4326) %>%
  left_join(current_locality_sizes,by="hscp_local") 
 
current_locality_hscp_lookup <- current_locality_shapefiles %>%
  st_drop_geometry() %>%
  distinct(hscp_local,HSCP_name)


### 2.1.3. Intermediate Zones ----

current_IZ_shapefiles <- current_datazone_shapefiles %>%
  summarise(
    TotalPop=sum(TotalPop),
    Shape_Area=sum(Shape_Area),
    geometry = sf::st_union(geometry),
    .by=c("InterZone","Locality","HSCP_name")
  ) 


## 2.2. Proposed ----

### 2.2.1. Data Zones ----

proposed_datazone_shapefiles <- path(
  proposed_datazone_shapefiles_dir,
  "Proposed_DZ_2022_Boundaries.shp"
) %>%
  read_sf() %>%
  dplyr::select(ProposedDZ, ProposedIZ, TotalPop, Shape_Area, LAName, Closest_2011_DZ=DZ2011) %>%
  left_join(current_datazone_names, by=c("Closest_2011_DZ"="datazone2011")) %>%
  st_transform(4326) %>%
  mutate(LAName=ifelse(LAName == "Na h-Eileanan Siar","Western Isles",LAName)) %>%
  mutate(LAName=ifelse(LAName %in% c("Clackmannanshire","Stirling"),
                       "Clackmannanshire and Stirling",LAName)) %>%
  mutate(LAName=ifelse(LAName == "City Of Edinburgh","Edinburgh",LAName))

### 2.2.2. Intermediate Zones ----

proposed_IZ_shapefiles <- proposed_datazone_shapefiles %>%
  summarise(
    TotalPop=sum(TotalPop),
    Shape_Area=sum(Shape_Area),
    geometry = sf::st_union(geometry),
    .by=c("ProposedIZ","LAName")
  ) %>%
  suppressWarnings() %>%
  suppressMessages()


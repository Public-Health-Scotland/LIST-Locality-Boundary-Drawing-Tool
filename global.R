
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

source("Data Load Functions/Load 2011 DZ.R")

source("Data Load Functions/Load 2022 DZ.R")


# 2. Lookups ----

## 2.1 Current Info ----

### 2.1.1 Data Zones ----

dz2011_shapefiles <- load_2011_dz() %>%
  st_transform(4326) %>%
  mutate(hscp2019name=ifelse(hscp2019name == "Na h-Eileanan Siar","Western Isles",hscp2019name)) %>%
  mutate(hscp2019name=ifelse(hscp2019name %in% c("Clackmannanshire","Stirling"),
                             "Clackmannanshire and Stirling",hscp2019name)) %>%
  mutate(hscp2019name=ifelse(hscp2019name == "City Of Edinburgh","Edinburgh",hscp2019name))

dz2022_shapefiles <- load_2022_dz() %>%
  st_transform(4326) %>%
  mutate(hscp2019name=ifelse(hscp2019name == "Na h-Eileanan Siar","Western Isles",hscp2019name)) %>%
  mutate(hscp2019name=ifelse(hscp2019name %in% c("Clackmannanshire","Stirling"),
                             "Clackmannanshire and Stirling",hscp2019name)) %>%
  mutate(hscp2019name=ifelse(hscp2019name == "City Of Edinburgh","Edinburgh",hscp2019name))


### 2.1.3. Intermediate Zones ----

iz2011_shapefiles <- dz2011_shapefiles %>%
  summarise(
    pop2022=sum(pop2022),
    Shape_Area=sum(Shape_Area),
    geometry = sf::st_union(geometry),
    .by=c("intzone2011","hscp_locality","hscp2019name")
  ) 

iz2022_shapefiles <- dz2022_shapefiles %>%
  summarise(
    pop2022=sum(pop2022),
    Shape_Area=sum(Shape_Area),
    geometry = sf::st_union(geometry),
    .by=-c("dz2022","dz2022name","pop2022","Shape_Area")
  ) %>%
  suppressWarnings() %>%
  suppressMessages()

### 2.1.2. Localities ----

hscp_locality2011_shapefiles <- dz2011_shapefiles %>%
  summarise(
    
    pop2022 = sum(pop2022),
    Shape_Area=sum(Shape_Area),
    geometry=st_union(geometry),
    .by=-c("intzone2011","dz2011","Shape_Area","pop2022","geometry")
    
  )



#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Set Up User Inputs ----

  locality_proposal <- reactive({
    file <- input$proposal
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c("csv", "xlsx"), "Please upload a csv or xlsx file"))

    if (ext == "csv") {
      read_csv(file$datapath) %>%
        dplyr::select(dz2022, Proposed_Locality) %>%
        na.omit() %>%
        mutate_all(trimws, "both")
    } else {
      read_xlsx(file$datapath, sheet = "Input Sheet", col_types = "text") %>%
        dplyr::select(dz2022, Proposed_Locality) %>%
        na.omit() %>%
        mutate_all(trimws, "both")
    }
  })


  # 3. Find New Locality Boundaries ----

  proposed_locality_w_dz2022 <- reactive({
    dz2022_shapefiles %>%
      right_join(locality_proposal(), by = c("dz2022"))
  })

  proposed_locality_boundaries <- reactive({
    proposed_locality_w_dz2022() %>%
      summarise(
        pop2022 = sum(pop2022),
        Shape_Area = sum(Shape_Area),
        geometry = sf::st_union(geometry),
        .by = c("Proposed_Locality")
      )
  })

  # 4. Get Locality And HSCP Of Interest ----

  localities_of_interest <- reactive({
    proposed_locality_w_dz2022() %>%
      pull(Proposed_Locality) %>%
      unique()
  })

  hscp_of_interest <- reactive({
    proposed_locality_w_dz2022() %>%
      pull(hscp2019name) %>%
      unique()
  })


  # 5. Shapefiles Of Interest ----

  dz2011_of_interest <- reactive({
    dz2011_shapefiles %>%
      filter(hscp2019name %in% hscp_of_interest())
  })


  dz2011_IZ_of_interest <- reactive({
    iz2011_shapefiles %>%
      filter(hscp2019name %in% hscp_of_interest())
  })


  dz2011_localities_of_interest <- reactive({
    hscp_locality2011_shapefiles %>%
      filter(hscp2019name %in% hscp_of_interest())
  })


  dz2022_of_interest <- reactive({
    dz2022_shapefiles %>%
      filter(hscp2019name %in% hscp_of_interest())
  })

  dz2022_IZ_of_interest <- reactive({
    iz2022_shapefiles %>%
      filter(hscp2019name %in% hscp_of_interest())
  })

  # 6. Population Changes ----

  population_ests_proposed <- reactive({
    proposed_locality_boundaries() %>%
      dplyr::select(hscp_locality = Proposed_Locality, proposed_population = pop2022) %>%
      st_drop_geometry() %>%
      mutate(proposed_pop_perc = 100 * (proposed_population / sum(proposed_population)))
  })

  population_ests_2011 <- reactive({
    dz2011_localities_of_interest() %>%
      dplyr::select(hscp_locality, current_population = pop2022) %>%
      st_drop_geometry() %>%
      mutate(current_pop_perc = 100 * (current_population / sum(current_population)))
  })

  population_ests_wide_of_interest <- reactive({
    population_ests_2011() %>%
      full_join(population_ests_proposed(), by = "hscp_locality")
  })

  population_ests_long_of_interest <- reactive({
    population_ests_wide_of_interest() %>%
      pivot_longer(-"hscp_locality", names_to = "measure")
  })

  # 7. Area Changes ----

  area_ests_2011 <- reactive({
    dz2011_localities_of_interest() %>%
      dplyr::select(hscp_locality, current_shape_area = Shape_Area) %>%
      st_drop_geometry() %>%
      mutate(current_area_perc = 100 * (current_shape_area / sum(current_shape_area)))
  })

  area_ests_proposed <- reactive({
    proposed_locality_boundaries() %>%
      dplyr::select(hscp_locality = Proposed_Locality, proposed_shape_area = Shape_Area) %>%
      st_drop_geometry() %>%
      mutate(proposed_area_perc = 100 * (proposed_shape_area / sum(proposed_shape_area)))
  })

  area_ests_wide_of_interest <- reactive({
    area_ests_2011() %>%
      full_join(area_ests_proposed(), by = "hscp_locality")
  })

  area_ests_long_of_interest <- reactive({
    area_ests_wide_of_interest() %>%
      pivot_longer(-"hscp_locality", names_to = "measure")
  })

  # 8. Find Changes In Locality Boundaries ----

  locality_differences_of_interest <- reactive({
    locality_differences <- vector("list", length = length(localities_of_interest()))

    for (i in 1:length(localities_of_interest())) {
      locality <- localities_of_interest()[i]

      proposed_shape <- proposed_locality_boundaries() %>%
        filter(Proposed_Locality == locality)

      shape_2011 <- dz2011_localities_of_interest() %>%
        filter(hscp_locality == locality)

      difference_in_shape <- st_sym_difference(shape_2011$geometry, proposed_shape$geometry) %>%
        suppressWarnings() %>%
        suppressMessages()

      locality_differences[[i]] <- difference_in_shape %>%
        st_union() %>%
        suppressWarnings() %>%
        suppressMessages()

      names(locality_differences) <- localities_of_interest()
    }

    locality_differences
  })

  output$map_of_interest <- renderLeaflet({
    # Create Base Map ----

    base_map <- leaflet(height = "2000px") %>%
      ## Add Search Option ----

      addSearchOSM() %>%
      ## Add Underlying Scotland Map ----

      addProviderTiles(providers$Esri.WorldTopoMap,
        options = providerTileOptions(minZoom = 6)
      )


    map_with_datazones <- base_map %>%
      addPolygons(
        data = dz2011_localities_of_interest(),
        group = "2011 Locality Boundaries",
        color = phs_colors("phs-green"),
        fillOpacity = 0
      ) %>%
      addPolygons(
        data = dz2011_of_interest(),
        group = "2011 DataZone Boundaries",
        color = phs_colors("phs-blue"),
        fillOpacity = 0
      ) %>%
      addPolygons(
        data = dz2011_IZ_of_interest(),
        group = "2011 Intermediate Zone Boundaries",
        color = phs_colors("phs-graphite"),
        fillOpacity = 0
      ) %>%
      addPolygons(
        data = dz2022_IZ_of_interest(),
        group = "2022 Intermediate Zone Boundaries",
        color = phs_colors("phs-liberty"),
        fillOpacity = 0
      ) %>%
      addPolygons(
        data = dz2022_of_interest(),
        group = "2022 DataZone Boundaries",
        color = phs_colors("phs-magenta"),
        fillOpacity = 0,
        popup = ~ paste("2022 Datazone: ", dz2022),
        label = ~ lapply(
          paste("2022 Datazone: ", dz2022),
          htmltools::HTML
        ),
        highlightOptions = highlightOptions(
          color = "red",
          weight = 2,
          bringToFront = TRUE
        )
      ) %>%
      addPolygons(
        data = proposed_locality_boundaries(),
        group = "Proposed Locality Boundaries (Based On 2022 DZ)",
        color = phs_colors("phs-rust"),
        fillOpacity = 0,
        popup = ~ paste("Proposed Locality: ", Proposed_Locality),
        label = ~ lapply(
          paste("Proposed Locality: ", Proposed_Locality),
          htmltools::HTML
        )
      )

    for (i in 1:length(locality_differences_of_interest())) {
      map_with_datazones <- map_with_datazones %>%
        addPolygons(
          data = locality_differences_of_interest()[[i]],
          group = "Differences Between Current and Proposed Locality Boundaries",
          color = phs_colors("phs-teal"),
          fillOpacity = 0
        )
    }


    map_with_datazones <- map_with_datazones %>%
      addLayersControl(
        # Groups will show in order they are set here
        overlayGroups = c(
          "2011 DataZone Boundaries",
          "2022 DataZone Boundaries",
          "2011 Intermediate Zone Boundaries",
          "2022 Intermediate Zone Boundaries",
          "2011 Locality Boundaries",
          "Proposed Locality Boundaries (Based On 2022 DZ)",
          "Differences Between Current and Proposed Locality Boundaries"
        ),
        position = "topright",
        # set collapsed = FALSE so that controls always displayed
        options = layersControlOptions(collapsed = FALSE)
      )

    map_with_datazones
  })


  output$area_changes <- renderDT({
    area_ests_wide_of_interest() %>%
      mutate(
        current_shape_area = shape_areas_formatting(current_shape_area),
        proposed_shape_area = shape_areas_formatting(proposed_shape_area)
      ) %>%
      mutate(
        current_area_perc = paste0(round(current_area_perc, 1), "%"),
        proposed_area_perc = paste0(round(proposed_area_perc, 1), "%")
      ) %>%
      mutate(
        current_area_perc = ifelse(current_area_perc == "NA%", NA, current_area_perc),
        proposed_area_perc = ifelse(proposed_area_perc == "NA%", NA, proposed_area_perc)
      ) %>%
      rename(
        "Locality" = "hscp_locality",
        "2011 Locality: Surface Area" = "current_shape_area",
        "2011 Locality: Percentage Of Surface Area" = "current_area_perc",
        "Proposed Surface Area" = "proposed_shape_area",
        "Proposed Percentage Of Surface Area" = "proposed_area_perc"
      ) %>%
      DT::datatable()
  })

  output$population_changes <- renderDT({
    population_ests_wide_of_interest() %>%
      mutate(
        current_population = prettyNum(current_population, big.mark = ","),
        proposed_population = prettyNum(proposed_population, big.mark = ",")
      ) %>%
      mutate(
        current_population = gsub("NA", "", current_population),
        proposed_population = gsub("NA", "", proposed_population)
      ) %>%
      mutate(
        current_pop_perc = paste0(round(current_pop_perc, 1), "%"),
        proposed_pop_perc = paste0(round(proposed_pop_perc, 1), "%")
      ) %>%
      mutate(
        current_pop_perc = ifelse(current_pop_perc == "NA%", NA, current_pop_perc),
        proposed_pop_perc = ifelse(proposed_pop_perc == "NA%", NA, proposed_pop_perc)
      ) %>%
      rename(
        "Locality" = "hscp_locality",
        "2011 Locality: Population" = "current_population",
        "2011 Locality: Percentage Of Population" = "current_pop_perc",
        "Proposed Population" = "proposed_population",
        "Proposed Percentage Of Population" = "proposed_pop_perc"
      ) %>%
      DT::datatable()
  })
})

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
    validate(need(ext %in% c("csv","xlsx"), "Please upload a csv or xlsx file"))
    
    if (ext == "csv"){
      
      read_excel(file$datapath, col_types = "text") %>%
        dplyr::select(ProposedDZ,Proposed_Locality) %>%
        na.omit()
      
    } else{
      
      read_xlsx(file$datapath, sheet="New DZs", col_types = "text") %>%
    dplyr::select(ProposedDZ,Proposed_Locality) %>%
    na.omit()
      
    }
    
    
    
  })
  
  
  
  # 3. Find New Locality Boundaries ----
  
  proposed_locality_boundaries <- reactive({
    
    proposed_datazone_shapefiles %>%
    right_join(locality_proposal(),by=c("ProposedDZ")) %>%
    summarise(TotalPop=sum(TotalPop),
              Shape_Area=sum(Shape_Area),
              geometry = sf::st_union(geometry),
              .by=c("Proposed_Locality"))
  })
  
  # 4. Get Locality And HSCP Of Interest ----
  
  localities_of_interest <- reactive({ 
    
    locality_proposal() %>%
      pull(Proposed_Locality) %>%
      unique() 
    
  })
  
  hscp_of_interest <- reactive({
    
    current_locality_hscp_lookup %>%
      filter(hscp_local %in% localities_of_interest()) %>%
      pull(HSCP_name) %>%
      unique()
    
  })
    
    
  
  # 5. Shapefiles Of Interest ----
  
  current_datazones_of_interest <- reactive({
    
    current_datazone_shapefiles %>%
      filter(HSCP_name %in% hscp_of_interest())
    
  })
    
    
  
  current_localities_of_interest <- reactive({
    
    current_locality_shapefiles %>%
      filter(HSCP_name %in% hscp_of_interest())
    
  })
  
  current_IZ_of_interest <- reactive({
    
    current_IZ_shapefiles %>%
      filter(HSCP_name %in% hscp_of_interest())
    
  })
  
  proposed_datazones_of_interest <- reactive({
    
    proposed_datazone_shapefiles %>%
      filter(LAName %in% hscp_of_interest())
  
  })
    
  proposed_IZ_of_interest <- reactive({
    
    proposed_IZ_shapefiles %>%
      filter(LAName %in% hscp_of_interest())
    
  })
  
  proposed_localities_of_interest <-reactive({ proposed_locality_boundaries() })
  
  # 6. Population Changes ----
  
  population_ests_proposed <- reactive({
    
    proposed_localities_of_interest() %>%
    dplyr::select(Locality=Proposed_Locality,proposed_population=TotalPop) %>%
    st_drop_geometry() %>%
    mutate(proposed_pop_perc=100*(proposed_population/sum(proposed_population)))
    
  })
  
  population_ests_current <- reactive({
    
    current_localities_of_interest() %>%
    dplyr::select(Locality=hscp_local,current_population=TotalPop) %>%
    st_drop_geometry() %>%
    mutate(current_pop_perc=100*(current_population/sum(current_population)))
  
  })
  
  population_ests_wide_of_interest <- reactive({
    
    population_ests_current() %>%
    full_join(population_ests_proposed(),by="Locality")
    
  })
  
  population_ests_long_of_interest <- reactive({
    
    population_ests_wide_of_interest() %>%
    pivot_longer(-"Locality",names_to="measure")
  
  })
  
  # 7. Area Changes ----
  
  area_ests_current <- reactive({
    
    current_localities_of_interest() %>%
    dplyr::select(Locality=hscp_local,current_shape_area=Shape_Area) %>%
    st_drop_geometry() %>%
    mutate(current_area_perc=100*(current_shape_area/sum(current_shape_area)))
  })
    
  area_ests_proposed <- reactive({
    
    proposed_localities_of_interest() %>%
    dplyr::select(Locality=Proposed_Locality,proposed_shape_area=Shape_Area) %>%
    st_drop_geometry() %>%
    mutate(proposed_area_perc=100*(proposed_shape_area/sum(proposed_shape_area)))
  
  })
    
  area_ests_wide_of_interest <- reactive({
    
    area_ests_current() %>%
    full_join(area_ests_proposed(),by="Locality")
    
  })
    
  area_ests_long_of_interest <- reactive({
    
    area_ests_wide_of_interest() %>%
    pivot_longer(-"Locality",names_to="measure")
  
  })
  
  # 8. Find Changes In Locality Boundaries ----
  
  locality_differences_of_interest <- reactive({ 
    
  locality_differences <-  vector("list",length=length(localities_of_interest()))
  
  for (i in 1:length(localities_of_interest())){
    
    locality <- localities_of_interest()[i]
    
    proposed_shape <- proposed_localities_of_interest() %>%
      filter(Proposed_Locality == locality)
    
    current_shape <- current_localities_of_interest() %>%
      filter(hscp_local == locality) 
    
    difference_in_shape <- st_sym_difference(current_shape$geometry,proposed_shape$geometry) %>%
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
                     options = providerTileOptions(minZoom = 6)) 
    
    
    map_with_datazones <- base_map %>%
      
      addPolygons(data=proposed_datazones_of_interest(),
                  group ="Proposed DataZone Boundaries",
                  color=phs_colors("phs-magenta"),
                  popup = ~ paste("Proposed Datazone: ", ProposedDZ),
                  label = ~ lapply(paste("Proposed Datazone: ", ProposedDZ), 
                                   htmltools::HTML),
                  highlightOptions = highlightOptions(
                    color = "red",
                    weight = 2,
                    bringToFront = TRUE
                  )) %>%
      
      addPolygons(data=current_localities_of_interest(),
                  group="Current Locality Boundaries",
                  color=phs_colors("phs-green")) %>%
      
      addPolygons(data=current_datazones_of_interest(),
                  group="Current DataZone Boundaries",
                  color=phs_colors("phs-blue")) %>%
      
      addPolygons(data=current_IZ_of_interest(),
                  group="Current Intermediate Zone Boundaries",
                  color=phs_colors("phs-graphite")) %>%
      
      addPolygons(data=proposed_IZ_of_interest(),
                  group="Proposed Intermediate Zone Boundaries",
                  color=phs_colors("phs-liberty")) %>%
      
      addPolygons(data=proposed_localities_of_interest(),
                  group="Proposed Locality Boundaries",
                  color=phs_colors("phs-rust"),
                  popup = ~ paste("Proposed Locality: ", Proposed_Locality),
                  label = ~ lapply(paste("Proposed Locality: ", Proposed_Locality), 
                                   htmltools::HTML)) 
    
    for (i in 1:length(locality_differences_of_interest())){

      map_with_datazones <- map_with_datazones %>%

        addPolygons(data=locality_differences_of_interest()[[i]],
                    group="Differences Between Current and Proposed Locality Boundaries",
                    color=phs_colors("phs-teal"))


    }
    
    
    map_with_datazones <- map_with_datazones %>%
      
      addLayersControl(
        # Groups will show in order they are set here
        overlayGroups = c("Current DataZone Boundaries",
                          "Proposed DataZone Boundaries",
                          "Current Intermediate Zone Boundaries",
                          "Proposed Intermediate Zone Boundaries",
                          "Current Locality Boundaries",
                          "Proposed Locality Boundaries",
                          "Differences Between Current and Proposed Locality Boundaries"),
        position = "topright",
        # set collapsed = FALSE so that controls always displayed
        options = layersControlOptions(collapsed = FALSE)
      )
    
    map_with_datazones
    
    
  })
  
  
  output$area_changes <- renderDT({
    
    area_ests_wide_of_interest() %>%
      mutate(current_shape_area=round(current_shape_area),
             proposed_shape_area=round(proposed_shape_area)) %>%
      mutate(current_area_perc=paste0(round(current_area_perc,1),"%"),
             proposed_area_perc=paste0(round(proposed_area_perc,1),"%")) %>%
      mutate(current_area_perc=ifelse(current_area_perc == "NA%",NA,current_area_perc),
             proposed_area_perc=ifelse(proposed_area_perc == "NA%",NA,proposed_area_perc)) %>%
      rename("Current Surface Area"="current_shape_area",
             "Current Percentage Of Surface Area"="current_area_perc",
             "Proposed Surface Area"="proposed_shape_area",
             "Proposed Percentage Of Surface Area"="proposed_area_perc") %>%
      DT::datatable()
    
    
  })
  
  output$population_changes <- renderDT({
    
    population_ests_wide_of_interest() %>%
      mutate(current_pop_perc=paste0(round(current_pop_perc,1),"%"),
             proposed_pop_perc=paste0(round(proposed_pop_perc,1),"%")) %>%
      mutate(current_pop_perc=ifelse(current_pop_perc == "NA%",NA,current_pop_perc),
             proposed_pop_perc=ifelse(proposed_pop_perc == "NA%",NA,proposed_pop_perc)) %>%
      rename("Current Population"="current_population",
             "Current Percentage Of Population"="current_pop_perc",
             "Proposed Population"="proposed_population",
             "Proposed Percentage Of Population"="proposed_pop_perc") %>%
      DT::datatable()
    
    
  })
  
  
})

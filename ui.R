#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(ui <- fluidPage(
  tagList(
    # Specify most recent fontawesome library - change version as needed
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.2/css/all.css);"),
    tags$style(type = "text/css", "#map_of_interest {height: calc(100vh - 200px) !important;}"),
    navbarPage(
      id = "home", # id used for jumping between tabs
      title = div(
        tags$a(img(src = "phs-logo.png", height = 40),
               href = "https://www.publichealthscotland.scot/",
               target = "_blank"
        ), # PHS logo links to PHS website
        style = "position: relative; top: -5px;"
      ),
      windowTitle = "Locality Boundaries Proposal Assessment Tool", # Title for browser tab
      header = tags$head(
        includeCSS("www/styles.css"), # CSS stylesheet
        tags$link(rel = "shortcut icon", href = "favicon_phs.ico") # Icon for browser tab
      ), # tabpanel
      ############################################## .
      # PAGE 1 ----
      ############################################## .
      tabPanel(
        title = "Data Zone Map",
        # Look at https://fontawesome.com/search?m=free for icons
        # icon = icon_no_warning_fn("stethoscope"),
        value = "dz_map",
        h1("Data zone map"),
        h2("Proposal Input"),
        fileInput("proposal","Please Input a Proposal",multiple=FALSE,accept=c(".csv",".xlsx")),
        h2("Proposal Map"),
        leafletOutput("map_of_interest"),
        h2("Population Change Comparison"),
        DTOutput("population_changes"),
        h2("Area Change Comparison"),
        DTOutput("area_changes")
      ) # tabpanel
    ) # navbar
  ) # taglist
) # ui fluidpage
)
  

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(terra)

gcms <- c("Ensemble", "bcc-csm1-1", "BNU-ESM", "CanESM2", "CNRM-CM5", "CSIRO-Mk3-6-0", "GFDL-ESM2G", "HadEM2-CC365", "HadGEM2-ES365", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC-ESM", "MIROC-ESM-CHEM", "MIROC5", "MRI-CGCM3")

scenarios <- c("rcp45", "rcp85")

gye_boundary <- st_read("/home/steve/OneDrive/whitebark/gyeboundary/GYE_boundary_dd.shp")

establishment <- list.files(path = "/media/smithers/shuysman/data/out/establishment/", pattern = "annual_aet_sum_.*nc", full.names = TRUE) %>%
    rast() %>%
    terra::project(crs("EPSG:4326")) / 10

mpb_files <- list.files(path = "/media/smithers/shuysman/data/out/beetle/", pattern = "Beetle_GDD_.*nc", full.names = TRUE)

refugia_colors = c("#00000000", "green")##colorBin("Greens", domain = NULL, bins = 2, na.color = "transparent")

ui <- fluidPage(
    titlePanel("Whitebark Pine Refugia"),
    sidebarLayout(
        sidebarPanel(
            h3("All Layers"),
            sliderInput("year", "Year", min=2006, max=2099, value=2050),
            selectInput("gcm", "GCM",
                        choices = gcms,
                        selected = "Ensemble"),
            selectInput("scenario", "Scenario",
                        choices = scenarios,
                        selected = "rcp45"),
            sliderInput("alpha", "Alpha", min = 0, max = 1, value = 0.5),
            hr(),
            h3("Establishment"),
            sliderInput("aet_threshold", "AET Threshold", min=0, max=500, value=250),
            hr(),
            h3("Mountain Pine Beetle"), 
            sliderInput("gdd_threshold", "GDD Threshold", min = 0, max = 2000, value = 833),
            hr(),
            h3("White Pine Blister Rust"),
            hr(),
            h3("Fire")
        ),
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Refugia",
                         tags$style(type = "text/css", "#refugia_map {height: calc(100vh - 80px) !important;}"),
                         leafletOutput("refugia_map", width = "100%", height = "100%")),
                tabPanel("Establishment",
                         tags$style(type = "text/css", "#establishment_map {height: calc(100vh - 80px) !important;}"),
                         leafletOutput("establishment_map", width = "100%", height = "100%")),
                tabPanel("Mountain Pine Beetle",
                         tags$style(type = "text/css", "#mpb_map {height: calc(100vh - 80px) !important;}"),
                         leafletOutput("mpb_map", width = "100%", height = "100%")),
            ))
    )
)

server <- function(input, output, session) {

    establishment_layer <- reactive({
        establishment %>% subset(terra::time(.) == input$year) %>% clamp(lower = input$aet_threshold, values = FALSE)
    })

    mpb_layer <- reactive({
        mpb_files[str_detect(mpb_files, input$gcm) & str_detect(mpb_files, input$scenario)] %>%
            rast() %>%
            subset(terra::time(.) == input$year) %>%
            clamp(upper = input$gdd_threshold, values = FALSE)
    })

    refugia_layer <- reactive({
        intersect(establishment_layer(),
                  resample(mpb_layer(), establishment_layer()))
    })
    
    output$refugia_map <- renderLeaflet({
        leaflet() %>%
                        addProviderTiles(providers$Stadia.StamenTerrain,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            addRasterImage(refugia_layer(), colors = refugia_colors, opacity = input$alpha) %>%
            addPolygons(data = gye_boundary, fill = FALSE)
    })
        
    output$establishment_map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stadia.StamenTerrain,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            addRasterImage(establishment_layer(), opacity = input$alpha) %>%
            addPolygons(data = gye_boundary, fill = FALSE)
    })

    output$mpb_map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stadia.StamenTerrain,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            addRasterImage(mpb_layer(), opacity = input$alpha) %>%
            addPolygons(data = gye_boundary, fill = FALSE)
    })
}

shinyApp(ui, server)

library(move2)
#library(move)
library(ggmap)
library(adehabitatHR)
library(shiny)
library(shinycssloaders)
library(fields)
library(scales)
library(lubridate)
library(zip)
library(shinyBS)
library(sf)
library(mapview)
library(pals)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(dplyr)
library(chromote)

#Bandwidth (h) plays a key role in shaping how you should choose resolution (res) and extent when estimating animal home ranges using kernel density estimation.
#When bandwidth is large (like with the href method), the kernel spreads widely from each location point. 
#In this case, a lower resolution is enough because the output is already smooth and fine details are lost in the wide smoothing. 
#Also, the extent can be smaller because the wide kernels already cover space beyond the observed points.
#But when bandwidth is small (like with LSCV or a tight custom value), the kernels are concentrated around each point. 
#Now you need a higher resolution to capture those fine details accurately — otherwise, the map will miss important structure. 
#You also need a larger extent, because tight kernels near the edge can get cut off if there's not enough buffer around the data.
#Smaller bandwidth → higher resolution + larger extent
#Larger bandwidth → lower resolution + smaller extent


##### Interface ######
shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Estimating the home range from the UD on a Map"),
    sidebarLayout(
      sidebarPanel(
        
        sliderInput(ns("perc"), "% of points included in KUD ", min = 0, max = 100, value = 95, width = "100%"),
        
        tags$div("Defaults for grid/ext change based on bandwidth method.
                 *Smaller bandwidth(LSCV) or small custom number → higher resolution + larger extent
                  -* Larger bandwidth(href)or large custom number → lower resolution + smaller extent", 
                 style = "color: blue; font-style: italic;"),
        
        selectInput(ns("hest"), 
                    "Bandwidth selection (hest)", 
                    choices = c("href", "LSCV", "custom"), 
                    selected = "href", 
                    width = "100%"),
        
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'href'", ns("hest")),
          helpText("For href, set extent to 0.5–1.0 , and grid resolution to 100–200.") ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'LSCV'", ns("hest")),
          helpText("For LSCV, set extent to 0.7–1.2, and grid resolution to 300–500.") ),
        
        # If custom is selected:
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("hest")),
          numericInput(ns("h_custom"), 
                       "Custom bandwidth value", 
                       value = 100, min = 10,max = 5000, step = 1, width = "100%")),
        
        numericInput(ns("res"), 
                     "Grid resolution (res:10 to 1000)", 
                     value = 100, min = 10, max = 1000, step = 10, width = "100%"),
        
        numericInput(ns("ext"), 
                     "Extent (space around data area)", 
                     value = 1, min = 0.1, max = 5, step = 0.1, width = "100%"),
        
        checkboxGroupInput(ns("animal_selector"), "Select Animal:", choices = NULL),
        downloadButton(ns("save_html"),"Save map as HTML", class = "btn-sm"),
        #downloadButton(ns("save_png"), "save Map as PNG", class = "btn-sm"),
        #downloadButton(ns("download_geojson"), "Download MCP as GeoJSON", class = "btn-sm"),
        downloadButton(ns("download_kmz"), "Download as KMZ", class = "btn-sm"),
        bsTooltip(id=ns("download_kmz"), title="Format for GoogleEarth", placement = "bottom", trigger = "hover", options = list(container = "body")),
        downloadButton(ns("download_gpkg"), "Download as GPKG", class = "btn-sm"),
        bsTooltip(id=ns("download_gpkg"), title="Shapefile for QGIS/ArcGIS", placement = "bottom", trigger = "hover", options = list(container = "body")),
        downloadButton(ns("download_kud_table"), "Download KUD Areas Table", class = "btn-sm"),
        ,width = 3),
      
      mainPanel(leafletOutput(ns("leafmap"), height = "85vh") ,width = 9)
    )
  )
}



#####server######

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  dataObj <- reactive({ data })
  
  # exclude all individuals with less than 5 locations
  data_filtered <- reactive({
    req(data)
    data %>%
      group_by(mt_track_id()) %>%
      filter(n() >= 5) %>%
      ungroup()
  })
  
  ##select animal in side bar
  observe({
    req(data_filtered())
    df <- data_filtered()
    
    animal_choices <- unique(mt_track_id(df))
    updateCheckboxGroupInput(session = session,
                             inputId = "animal_selector",
                             choices = animal_choices,
                             selected = animal_choices)
  })
  
  
  selected_data <- reactive({
    req(input$animal_selector)
    df <- data_filtered()
    selected <- filter_track_data(df, .track_id = input$animal_selector)
    selected
  })
  
  
  # Calculate individual KUD
  kud_cal <- reactive({
    req(input$perc, input$res, input$ext, input$hest )
    
    data_sel <- selected_data()
    
    crs_proj <- mt_aeqd_crs(data_sel, center = "center", units = "m")
    sf_data_proj <- st_transform(data_sel, crs_proj)
    
    sf_data_proj$id <- mt_track_id(sf_data_proj)
    sp_data_proj <- as_Spatial(sf_data_proj[,'id'])
    sp_data_proj <- sp_data_proj[,(names(sp_data_proj) %in% "id")] 
    sp_data_proj$id <- make.names(as.character(sp_data_proj$id),allow_=F)
    
    h_value <- if (input$hest == "custom") input$h_custom else input$hest
    
    data_kud <- adehabitatHR::kernelUD( sp_data_proj, h = h_value, grid = input$res, extent = input$ext )
    
    kud_polygons <- adehabitatHR::getverticeshr(data_kud, percent = input$perc)#extracts the home range polygon at a certain percentage
    
    sf_kud <- st_as_sf(kud_polygons)
    
    sf_kud <- sf_kud %>%
      rename(track_id = id) %>%
      st_cast("POLYGON") %>%
      mutate(area = as.numeric(st_area(.)) / 1e6) %>%
      group_by(track_id) %>%
      filter(area == max(area)) %>%
      slice(1) %>%
      ungroup()%>%
      st_transform(4326) %>%
      mutate(track_id = as.character(track_id))
      
    
    track_lines <- mt_track_lines(data_sel)
    track_id_col <- mt_track_id_column(data_sel)
    track_lines <- track_lines %>%
      rename(track_id = all_of(track_id_col)) %>%
      mutate(track_id = make.names(as.character(track_id), allow_ = FALSE))
    
    
    return(list(data_kud = sf_kud, track_lines = track_lines))
    
    
  })
  
  
  
  ##leaflet map####
  
  kudmap <- reactive({
    req(kud_cal())
    kud <- kud_cal()
    bounds <- as.vector(st_bbox(dataObj()))
    track_lines <- kud$track_lines
    
    sf_kud <- kud$data_kud
    ids <- unique(c(sf_kud$track_id, track_lines$track_id))
    pal <- colorFactor(palette = pals::glasbey(), domain = ids)
    
    
    leaflet(options = leafletOptions(minZoom = 2)) %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%       
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addTiles(group = "OpenStreetMap") %>%
      addScaleBar(position = "topleft") %>%
      
      addPolylines(data = track_lines, color = ~pal(track_lines$track_id),
                   weight = 3, group = "Tracks") %>%
      
      addPolygons(data = sf_kud, fillColor = ~pal(track_id),color = "black",fillOpacity = 0.4,
                  weight = 2,label = ~track_id, group = "KUD") %>%
      
      
      addLegend(position = "bottomright",pal = pal,values = ids,title = "Track") %>%
      
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        #overlayGroups = c("Tracks", "KUD", "Population KUD"),
        overlayGroups = c("Tracks", "KUD"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$leafmap <- renderLeaflet({kudmap()})
  
  
  ###download the table of KUD
  output$download_kud_table <- downloadHandler(
    filename = paste0("KUDs_",input$perc,input$hest,input$res, input$ext,"_areas.csv"),
    content = function(file) {
      kud <- kud_cal()$data_kud
      kud_df <- as.data.frame(kud)
      df <- data.frame(TrackID = kud_df$track_id, Area_km2 = kud_df$area, KUD_percent=input$perc,Grid_resolution=input$res,Extent =input$ext )
      write.csv(df, file, row.names = FALSE) })
  
      #kud_df <- st_drop_geometry(kud)  # remove geometry to get plain data frame
      
  ### save map as HTML
  output$save_html <- downloadHandler(
    filename = paste0("homerange-kud_",input$perc,input$hest,input$res, input$ext,".html"),
    content = function(file) {
      saveWidget(widget = kudmap(),file=file) })
  
  ###download shape as kmz  
  output$download_kmz <- downloadHandler(
    filename = paste0("homerang-kud_",input$perc,input$hest,input$res, input$ext,".kmz"),
    content = function(file) {
      temp_kmz <- tempdir()
      kud_shape <- st_as_sf(kud_cal()$data_kud)
      kml_path <- file.path(temp_kmz, "kud.kml")
      st_write(kud_shape, kml_path, driver="KML", delete_dsn = TRUE)
      #zip::zip(zipfile = file, files = kml_path, mode = "cherry-pick")})
      zip::zipr(zipfile = file, files = kml_path) })
  
  
  ###download shape as GeoPackage (GPKG)
  output$download_gpkg <- downloadHandler(
    filename = paste0("homerang-kud_",input$perc,".gpkg"),
    content = function(file) {
      kud_shape <- st_as_sf(kud_cal()$data_kud)
      st_write(kud_shape, file, driver = "GPKG", delete_dsn = TRUE)} )
  

  return(reactive({ current() }))
}
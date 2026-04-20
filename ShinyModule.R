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
library(sp)
library(mapview)
library(pals)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(dplyr)
library(chromote)
library(raster)
library(jsonlite)
library(shinycssloaders)

`%||%` <- function(x, y) if (is.null(x)) y else x

##### Interface ######
shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Estimating the home range using The kernel function(utilization distribution) on a Map"),
    sidebarLayout(
      sidebarPanel(
        
        uiOutput(ns("animals_ui")),
        tags$div(style = "display:none;", textInput(ns("animals_json"), label = NULL, value = "")),
        
        sliderInput(ns("perc"), "% of points included in KUD ", min = 0, max = 100, value = 95, width = "100%"),
        
        tags$div(
          "* If you get an error, try adjusting resolution and extent:
            - For small areas → increase grid resolution
            - For wide-ranging movements → lower grid resolution",
          style = "color: darkblue; font-size: 90%; font-style: italic; white-space: pre-line;"),
        
        selectInput(ns("hest"),"Bandwidth selection (hest)",choices = c("href", "LSCV", "custom"), selected = "href", width = "100%"),
        
        tags$div(" *for Smaller bandwidth(LSCV) or small custom number → higher resolution + larger extent
                  -* for Larger bandwidth(href)or large custom number → lower resolution + smaller extent", 
                 style = "color: darkgreen; font-style: italic;white-space: pre-line;"),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'href'", ns("hest")),
          helpText("For href, set extent to 0.5–1.0 , and grid resolution to 100–200.") ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'LSCV'", ns("hest")),
          helpText("For LSCV, set extent to 0.7–1.2, and grid resolution to 300–500.") ),
        
        # If custom is selected:
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("hest")),
          numericInput(ns("h_custom"), "Custom bandwidth value", value = 100, min = 10,max = 5000, step = 1, width = "100%")),
        
        numericInput(ns("res"), "Grid resolution (res:10 to 1000)", value = 100, min = 10, max = 1000, step = 10, width = "100%"),
        numericInput(ns("ext"), "Extent (space around data area)", value = 1, min = 0.1, max = 5, step = 0.1, width = "100%"),
        
        
        downloadButton(ns("save_html"),"Save map as HTML", class = "btn-sm"),
        #downloadButton(ns("save_png"), "save Map as PNG", class = "btn-sm"),
        #downloadButton(ns("download_geojson"), "Download MCP as GeoJSON", class = "btn-sm"),
        downloadButton(ns("download_kmz"), "Download as KMZ", class = "btn-sm"),
        bsTooltip(id=ns("download_kmz"), title="Format for GoogleEarth", placement = "bottom", trigger = "hover", options = list(container = "body")),
        downloadButton(ns("download_gpkg"), "Download as GPKG", class = "btn-sm"),
        bsTooltip(id=ns("download_gpkg"), title="Shapefile for QGIS/ArcGIS", placement = "bottom", trigger = "hover", options = list(container = "body")),
        downloadButton(ns("download_kud_table"), "Download KUD Areas Table", class = "btn-sm")
        ,width = 3),
      
      mainPanel(
        withSpinner(leafletOutput(ns("leafmap"), height = "85vh")),
        width = 9
      )
    )
  )
}



shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  
  # exclude all individuals with less than 5 locations
  data_filtered <- reactive({
    req(data)
    data %>%
      group_by(mt_track_id()) %>%
      filter(n() >= 5) %>%
      ungroup()
  })
  
  all_ids_vec <- reactive({
    req(data_filtered())
    sort(unique(as.character(mt_track_id(data_filtered()))))
  })
  output$animals_ui <- renderUI({
    animal_choices <- all_ids_vec()
    restored_sel <- isolate(input$animal_selector)
    sel <- if (!is.null(restored_sel)) restored_sel else animal_choices
    
    checkboxGroupInput( ns("animal_selector"),"Select Track:",choices = animal_choices,selected = sel )
  })
  
  applied_animals <- reactiveVal(NULL)
  init_applied <- reactiveVal(FALSE)
  
  observeEvent(input$animal_selector, {
    req(!is.null(input$animal_selector))
    applied_animals(as.character(input$animal_selector))
    init_applied(TRUE)
  }, ignoreInit = FALSE)
  
  observeEvent(input$animal_selector, {
    vals <- input$animal_selector %||% character(0)
    updateTextInput(session,"animals_json", value = jsonlite::toJSON(vals, auto_unbox = FALSE))
  }, ignoreInit = TRUE)
  
  selected_data <- reactive({
    req(init_applied())
    
    sel <- applied_animals()
    df  <- data_filtered()
    
    if (is.null(sel) || length(sel) == 0) return(df[0, ])
    
    filter_track_data(df, .track_id = sel)
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
    
    data_kud <- adehabitatHR::kernelUD( sp_data_proj, h = h_value, grid = input$res, extent = input$ext, same4all=TRUE )
    
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
  
  ##population-level Kernel Utilization Distribution (KUD)
  
  average_kud_cal <- reactive({
    req(input$perc, input$res, input$ext, input$hest )
    message("Starting average KUD calculation...")
    
    data_sel <- selected_data()
    
    crs_proj <- mt_aeqd_crs(data_sel, center = "center", units = "m")
    sf_data_proj <- st_transform(data_sel, crs_proj)
    
    sf_data_proj$id <- mt_track_id(sf_data_proj)
    sp_data_proj <- as_Spatial(sf_data_proj[,'id'])
    sp_data_proj <- sp_data_proj[,(names(sp_data_proj) %in% "id")] 
    sp_data_proj$id <- make.names(as.character(sp_data_proj$id),allow_=F)
    
    h_value <- if (input$hest == "custom") input$h_custom else input$hest
    
    
    data_kud <- adehabitatHR::kernelUD( sp_data_proj, h = h_value, grid = input$res, extent = input$ext, same4all = TRUE )
    
    
    ####Generate Population-Level KUD Contour from Averaged Raster######## 
    #(manual replacement for adehabitatHR::getverticeshr)
    
    func_spdf <- function(x) as(x, "SpatialPixelsDataFrame")
    spdfs <- lapply(data_kud, func_spdf)  
    
    rasters <- lapply(spdfs, raster::raster)
    raster_stack <- raster::stack(rasters)
    avg_ud <- raster::calc(raster_stack, fun = mean)
    
    # Normalize: becomes a proper probability surface
    total_sum <- raster::cellStats(avg_ud, stat = "sum")#Statistics across cells
    avg_ud <- avg_ud / total_sum
    
    # Compute threshold for contour
    vals <- raster::values(avg_ud)#Assign (new) values to a Raster* object.
    vals <- vals[!is.na(vals)]
    vals_sorted <- sort(vals, decreasing = TRUE)
    cumprob <- cumsum(vals_sorted) / sum(vals_sorted)
    level <- input$perc / 100
    threshold <- vals_sorted[min(which(cumprob >= level))]
    
    # Create contour lines
    #Draw contour lines that trace around the area where the values are greater than or equal to this threshold.
    cl <- raster::rasterToContour(avg_ud, levels = threshold)#Raster to contour lines conversion-output is vector layer with contour lines
    sf_contour <- st_as_sf(cl) 
    sf_contour <- st_transform(sf_contour, 4326)
    
    return(list(avg_raster = avg_ud, contours = sf_contour))
  })
  
  ##leaflet map####
  
  kudmap <- reactive({
    req(kud_cal())
    kud_dat <- kud_cal()
    bounds <- as.vector(st_bbox(selected_data()))
    track_lines <- kud_dat$track_lines
    
    sf_kud <- kud_dat$data_kud
    
    ids <- unique(c(sf_kud$track_id, track_lines$track_id))
    pal <- colorFactor(palette = pals::glasbey(), domain = ids)
    
    avg_kud <- average_kud_cal()  # Population KUD
    avg_contours <- avg_kud$contours
    
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
      
      # Population KUD
      addPolygons(data = avg_contours, color = "red",fillOpacity = 0.3, weight = 2,
                  label = "Population-Level 95% KUD", group = "Population KUD") %>%
      
      
      addLegend(position = "bottomright",pal = pal,values = ids,title = "Track") %>%
      
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        overlayGroups = c("Tracks", "KUD", "Population KUD"),
        options = layersControlOptions(collapsed = FALSE))%>%
      
      hideGroup("Population KUD")
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
  
  
  ### save map as HTML
  output$save_html <- downloadHandler(
    filename = paste0("homerange-kud_",input$perc,input$hest,input$res, input$ext,".html"),
    content = function(file) {
      saveWidget(widget = kudmap(),file=file) })
  
  ### save map as PNG
  output$save_png <- downloadHandler(
    filename = paste0("homerange-kud_",input$perc,input$hest,input$res, input$ext,".png"),
    content = function(file) {
      html_file <- "leaflet_export.html"
      saveWidget(kudmap(), file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot2::webshot(url = html_file,file = file,vwidth = 1000,vheight = 800) })
  
  
  ###download shape as kmz  
  output$download_kmz <- downloadHandler(
    filename = paste0("homerang-kud_",input$perc,input$hest,input$res, input$ext,".kmz"),
    content = function(file) {
      temp_kmz <- tempdir()
      kud_shape <- st_as_sf(kud_cal()$data_kud)
      kml_path <- file.path(temp_kmz, "kud.kml")
      st_write(kud_shape, kml_path, driver="KML", delete_dsn = TRUE)
      zip::zipr(zipfile = file, files = kml_path) })
  
  
  
  ###download shape as GeoPackage (GPKG)
  output$download_gpkg <- downloadHandler(
    filename = paste0("homerang-kud_",input$perc,".gpkg"),
    content = function(file) {
      kud_shape <- st_as_sf(kud_cal()$data_kud)
      st_write(kud_shape, file, driver = "GPKG", delete_dsn = TRUE)} )
  
  
  return(reactive({ current() }))
}

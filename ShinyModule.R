library(move2)
library(adehabitatHR)
library(shiny)
library(shinycssloaders)
library(zip)
library(shinyBS)
library(sf)
library(sp)
library(pals)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(dplyr)
library(raster)
library(jsonlite)

`%||%` <- function(x, y) if (is.null(x)) y else x

##### Interface ######
shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Estimating the home range using The kernel function(utilization distribution) on a Map"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4("Tracks"),
        uiOutput(ns("animals_ui")),
        fluidRow(
          column(6, actionButton(ns("select_all_animals"), "Select All Animals", class = "btn-sm")),
          column(6, actionButton(ns("unselect_animals"), "Unselect All Animals", class = "btn-sm"))
        ),
        
        tags$div(style = "display:none;", textInput(ns("animals_json"), label = NULL, value = "")),
        
        sliderInput(ns("perc"), "% of points included in KUD", min = 0, max = 100, value = 95, width = "100%"),
        
        tags$div(
                      "* If you get an error, try adjusting resolution and extent:
            - For small areas -> increase grid resolution
            - For wide-ranging movements -> lower grid resolution",
                      style = "color: darkblue; font-size: 90%; font-style: italic; white-space: pre-line;"
        ),
        
        selectInput(ns("hest"), "Bandwidth selection (hest)", choices = c("href", "LSCV", "custom"),selected = "href",width = "100%"),
        
        tags$div(
                  "* For smaller bandwidth (LSCV) or small custom number -> higher resolution + larger extent
        * For larger bandwidth (href) or large custom number -> lower resolution + smaller extent",
                  style = "color: darkgreen; font-style: italic; white-space: pre-line;"
                ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'href'", ns("hest")),
          helpText("For href, set extent to 0.5-1.0, and grid resolution to 100-200.")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'LSCV'", ns("hest")),
          helpText("For LSCV, set extent to 0.7-1.2, and grid resolution to 300-500.")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("hest")),
          numericInput(ns("h_custom"), "Custom bandwidth value", value = 100, min = 10, max = 5000, step = 1, width = "100%")
        ),
        
        numericInput(ns("res"), "Grid resolution (res: 10 to 1000)", value = 100, min = 10, max = 1000, step = 10, width = "100%"),
        numericInput(ns("ext"), "Extent (space around data area)", value = 1, min = 0.1, max = 5, step = 0.1, width = "100%"),
        
        hr(),
        actionButton(ns("apply_btn"), "Apply Changes", class = "btn-primary btn-block"),
        uiOutput(ns("apply_warning")),
        hr(),
        
        downloadButton(ns("save_html"), "Save map as HTML", class = "btn-sm"),
        br(),
        br(),
        downloadButton(ns("save_png"), "Save map as PNG", class = "btn-sm"),
        br(),
        br(),
        downloadButton(ns("download_kmz"), "Download as KMZ", class = "btn-sm"),
        br(),
        br(),
        downloadButton(ns("download_gpkg"), "Download as GPKG (", class = "btn-sm"),
        br(),
        br(),
        downloadButton(ns("download_kud_table"), "Download KUD Areas Table", class = "btn-sm")
      ),
      
      mainPanel(
        withSpinner(leafletOutput(ns("leafmap"), height = "85vh")),
        width = 9
      )
    )
  )
}

##### Server ######
shinyModule <- function(input, output, session, data) {
  
  current <- reactiveVal(NULL)
  
  locked_settings <- reactiveVal(NULL)
  locked_data <- reactiveVal(NULL)
  
  applied_animals <- reactiveVal(NULL)
  init_applied <- reactiveVal(FALSE)
  init_done <- reactiveVal(FALSE)
  
  apply_warning <- reactiveVal(FALSE)
  
  if (is.null(data) || nrow(data) == 0) {
    message("Input is NULL or has 0 rows — returning NULL.")
    return(reactive(NULL))
  }
  
  if (!sf::st_is_longlat(data)) {
    data <- sf::st_transform(data, 4326)
  }
  current(data)
  
  track_col <- mt_track_id_column(data)
  
  filtered_data <- reactive({
    data %>%
      group_by(mt_track_id()) %>%
      filter(n() >= 5) %>%
      ungroup()
  })
  
  filtered_ids <- reactive({
    sort(unique(as.character(mt_track_id(filtered_data()))))
  })
  
  output$apply_warning <- renderUI({
    if (isTRUE(apply_warning())) {
      div(
        style = "color:#b30000; font-weight:800; margin-top:6px;",
        "No track selected"
      )
    } else {
      NULL
    }
  })
  
  output$animals_ui <- renderUI({
    ids <- filtered_ids()
    
    if (!length(ids)) {
      return(tags$div(style = "color:#666;", "No tracks with at least 5 locations."))
    }
    
    restored_sel <- isolate(input$animals)
    sel <- if (!is.null(restored_sel)) restored_sel else ids
    sel <- intersect(sel, ids)
    if (!length(sel)) sel <- ids
    
    checkboxGroupInput(
      session$ns("animals"),
      label = NULL,
      choices = ids,
      selected = sel
    )
  })
  
  observeEvent(input$animals, {
    vals <- as.character(input$animals %||% character(0))
    
    if (length(vals) > 0) {
      applied_animals(vals)
      init_applied(TRUE)
      apply_warning(FALSE)
    }
    
    updateTextInput(session,"animals_json",value = jsonlite::toJSON(vals, auto_unbox = FALSE) )
  }, ignoreInit = FALSE)
  
  observeEvent(input$select_all_animals, {
    updateCheckboxGroupInput(session, "animals", selected = filtered_ids())
  }, ignoreInit = TRUE)
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  }, ignoreInit = TRUE)
  
  selected_data <- reactive({
    req(init_applied())
    
    sel <- applied_animals()
    d <- filtered_data()
    
    if (is.null(sel) || !length(sel)) return(d[0, ])
    
    d[mt_track_id(d) %in% sel, ]
  })
  
  # initialize
  observe({
    if (isTRUE(init_done())) return()
    req(init_applied())
    
    d0 <- selected_data()
    if (nrow(d0) == 0) return()
    
    locked_data(d0)
    locked_settings(list(
      animals= applied_animals(),
      perc= input$perc,
      hest= input$hest,
      h_custom = input$h_custom %||% 100,
      res= input$res,
      ext= input$ext
    ))
    
    init_done(TRUE)
  })
  
  observeEvent(input$apply_btn, {
    if (is.null(input$animals) || length(input$animals) == 0) {
      apply_warning(TRUE)
      return()
    }
    
    d_applied <- selected_data()
    if (nrow(d_applied) == 0) {
      apply_warning(TRUE)
      return()
    }
    
    locked_data(d_applied)
    locked_settings(list(
      animals  = input$animals,
      perc= input$perc,
      hest= input$hest,
      h_custom = input$h_custom %||% 100,
      res = input$res,
      ext = input$ext
    ))
    
    apply_warning(FALSE)
  }, ignoreInit = TRUE)
  
  active_data <- reactive({
    locked_data() %||% selected_data()
  })
  
  active_settings <- reactive({
    locked_settings() %||% list(
      animals = applied_animals(),
      perc = input$perc,
      hest= input$hest,
      h_custom = input$h_custom %||% 100,
      res = input$res,
      ext = input$ext
    )
  })
  
  prep_kud_input <- function(data_sel) {
    crs_proj <- mt_aeqd_crs(data_sel, center = "center", units = "m")
    sf_data_proj <- st_transform(data_sel, crs_proj)
    
    sf_data_proj$id <- mt_track_id(sf_data_proj)
    sp_data_proj <- as_Spatial(sf_data_proj[, "id"])
    sp_data_proj <- sp_data_proj[, names(sp_data_proj) %in% "id"]
    sp_data_proj$id <- make.names(as.character(sp_data_proj$id), allow_ = FALSE)
    
    sp_data_proj
  }
  
  get_h_value <- function(s) {
    if (identical(s$hest, "custom")) s$h_custom else s$hest
  }
  
  compute_ud <- function(data_sel, s) {
    adehabitatHR::kernelUD(
      prep_kud_input(data_sel),
      h = get_h_value(s),
      grid = s$res,
      extent = s$ext,
      same4all = TRUE
    )
  }
  
  kud_cal <- reactive({
    data_sel <- active_data()
    req(data_sel)
    if (is.null(data_sel) || nrow(data_sel) == 0) return(NULL)
    
    s <- active_settings()
    
    data_kud <- compute_ud(data_sel, s)
    kud_polygons <- adehabitatHR::getverticeshr(data_kud, percent = s$perc)
    
    sf_kud <- st_as_sf(kud_polygons) %>%
      rename(track_id = id) %>%
      suppressWarnings(st_cast(., "POLYGON")) %>%
      mutate(area = as.numeric(st_area(.)) / 1e6) %>%
      group_by(track_id) %>%
      filter(area == max(area)) %>%
      slice(1) %>%
      ungroup() %>%
      st_transform(4326) %>%
      mutate(track_id = as.character(track_id))
    
    track_lines <- mt_track_lines(data_sel)
    track_id_col <- mt_track_id_column(data_sel)
    track_lines <- track_lines %>%
      rename(track_id = all_of(track_id_col)) %>%
      mutate(track_id = make.names(as.character(track_id), allow_ = FALSE))
    
    list(data_kud = sf_kud, track_lines = track_lines)
  })
  
  average_kud_cal <- reactive({
    data_sel <- active_data()
    req(data_sel)
    if (is.null(data_sel) || nrow(data_sel) == 0) return(NULL)
    
    s <- active_settings()
    
    data_kud <- compute_ud(data_sel, s)
    
    spdfs <- lapply(data_kud, function(x) as(x, "SpatialPixelsDataFrame"))
    rasters <- lapply(spdfs, raster::raster)
    raster_stack <- raster::stack(rasters)
    avg_ud <- raster::calc(raster_stack, fun = mean)
    
    total_sum <- raster::cellStats(avg_ud, stat = "sum")
    avg_ud <- avg_ud / total_sum
    
    vals <- raster::values(avg_ud)
    vals <- vals[!is.na(vals)]
    vals_sorted <- sort(vals, decreasing = TRUE)
    cumprob <- cumsum(vals_sorted) / sum(vals_sorted)
    threshold <- vals_sorted[min(which(cumprob >= (s$perc / 100)))]
    
    sf_contour <- raster::rasterToContour(avg_ud, levels = threshold) %>%
      st_as_sf() %>%
      st_transform(4326)
    
    list(avg_raster = avg_ud, contours = sf_contour)
  })
  
  kudmap <- reactive({
    data_sel <- active_data()
    
    if (is.null(data_sel) || nrow(data_sel) == 0) {
      return(leaflet() %>% addProviderTiles("OpenStreetMap"))
    }
    
    s <- active_settings()
    kud_dat <- kud_cal()
    avg_kud <- average_kud_cal()
    
    req(!is.null(kud_dat), !is.null(avg_kud))
    
    sf_kud <- kud_dat$data_kud
    track_lines <- kud_dat$track_lines
    avg_contours <- avg_kud$contours
    
    ids <- unique(c(sf_kud$track_id, track_lines$track_id))
    pal <- colorFactor(palette = pals::glasbey(), domain = ids)
    bounds <- as.vector(st_bbox(data_sel))
    
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addTiles(group = "OpenStreetMap") %>%
      addScaleBar(position = "topleft") %>%
      addPolylines(
        data = track_lines,
        color = ~pal(track_lines$track_id),
        weight = 3,
        group = "Tracks"
      ) %>%
      addPolygons(data = sf_kud,fillColor = ~pal(track_id), color = "black",fillOpacity = 0.4,weight = 2,label = ~track_id,group = "KUD") %>%
      addPolygons(data = avg_contours,color = "red",fillOpacity = 0.3,weight = 2,label = paste0("Population-Level ", s$perc, "% KUD"),group = "Population KUD") %>%
      addLegend(position = "bottomright", pal = pal, values = ids, title = "Track") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        overlayGroups = c("Tracks", "KUD", "Population KUD"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Population KUD")
  })
  
  output$leafmap <- renderLeaflet({
    kudmap()
  })
  
  file_stub <- reactive({
    s <- active_settings()
    paste0("homerange_kud_", s$perc, "_", s$hest, "_", s$res, "_", s$ext)
  })
  
  output$download_kud_table <- downloadHandler(
    filename = function() paste0(file_stub(), "_areas.csv"),
    content = function(file) {
      s <- active_settings()
      kud_df <- as.data.frame(kud_cal()$data_kud)
      
      df <- data.frame(
        TrackID = kud_df$track_id,
        Area_km2 = kud_df$area,
        KUD_percent = s$perc,
        Grid_resolution = s$res,
        Extent = s$ext
      )
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$save_html <- downloadHandler(
    filename = function() paste0(file_stub(), ".html"),
    content = function(file) {
      saveWidget(widget = kudmap(), file = file)
    }
  )
  
  output$save_png <- downloadHandler(
    filename = function() paste0(file_stub(), ".png"),
    content = function(file) {
      html_file <- tempfile(fileext = ".html")
      saveWidget(kudmap(), file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot2::webshot(url = html_file, file = file, vwidth = 1000, vheight = 800)
    }
  )
  
  output$download_kmz <- downloadHandler(
    filename = function() paste0(file_stub(), ".kmz"),
    content = function(file) {
      kud_shape <- st_as_sf(kud_cal()$data_kud)
      kml_path <- file.path(tempdir(), "kud.kml")
      st_write(kud_shape, kml_path, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
      zip::zipr(zipfile = file, files = kml_path)
    }
  )
  
  output$download_gpkg <- downloadHandler(
    filename = function() paste0(file_stub(), ".gpkg"),
    content = function(file) {
      kud_shape <- st_as_sf(kud_cal()$data_kud)
      st_write(kud_shape, file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    }
  )
  
  return(reactive({
    req(current())
    current()
  }))
}
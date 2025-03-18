library(shiny)
library(bslib)
library(magick)
library(shinyjs)
library(here)
library(tidyverse)
library(base64enc)

# Create directories if they don't exist
dirs <- c("passed", "failed", "skipped")
walk(dirs, function(folder){
  if (!dir.exists(here("data", "confirmed_watersheds", folder))){
    dir.create(here("data", "confirmed_watersheds", folder))
  }
})

ui <- page_sidebar(
  title = "Watershed Review Tool",
  sidebar = sidebar(
    fileInput("folder", "Choose Images", multiple = TRUE,
              accept = c('image/png', 'image/jpeg', 'image/jpg')),
    hr(),
    div(
      style = "position: sticky; top: 80px;",
      actionButton("pass", "Pass", class = "btn-success", width = "100%"),
      br(), br(),
      actionButton("fail", "Fail", class = "btn-danger", width = "100%"),
      br(), br(),
      actionButton("skip", "Skip", class = "btn-warning", width = "100%")
    )
  ),
  
  layout_column_wrap(
    width = 1,
    card(
      full_screen = TRUE,
      card_header("Image Preview"),
      uiOutput("image_container")
    )
  ),
  
  # Add necessary JavaScript for drag and zoom functionality
  tags$head(
    tags$script("
      var dragItem = null;
      var dragStartX = 0;
      var dragStartY = 0;
      var imageX = 0;
      var imageY = 0;
      var currentZoom = 1;

      function handleDragStart(e) {
        dragItem = e.target;
        dragStartX = e.clientX - imageX;
        dragStartY = e.clientY - imageY;
        document.addEventListener('mousemove', handleDrag);
        document.addEventListener('mouseup', handleDragEnd);
      }

      function handleDrag(e) {
        if (!dragItem) return;
        imageX = e.clientX - dragStartX;
        imageY = e.clientY - dragStartY;
        dragItem.style.transform = `translate(${imageX}px, ${imageY}px) scale(${currentZoom})`;
      }

      function handleDragEnd() {
        dragItem = null;
        document.removeEventListener('mousemove', handleDrag);
        document.removeEventListener('mouseup', handleDragEnd);
      }

      function handleWheel(e) {
        e.preventDefault();
        const delta = e.deltaY * -0.001;
        currentZoom = Math.min(Math.max(0.1, currentZoom + delta), 3);
        
        // Update the image scale
        const img = e.target;
        img.style.transform = `translate(${imageX}px, ${imageY}px) scale(${currentZoom})`;
      }
    ")
  )
)

server <- function(input, output, session) {
  # Reactive values for image management
  rv <- reactiveValues(
    current_image = NULL,
    image_index = 1,
    images = NULL
  )
  
  # When new files are uploaded
  observeEvent(input$folder, {
    rv$images <- input$folder
    rv$image_index <- 1
    rv$current_image <- rv$images[rv$image_index, ]
  })
  
  # Display current image
  output$image_container <- renderUI({
    req(rv$current_image)
    
    # create a data URI for the image
    img_data <- base64enc::dataURI(
      file = rv$current_image$datapath,
      mime = rv$current_image$type
    )
    
    tags$div(
      style = "overflow: hidden; position: relative; height: 600px;",
      tags$img(
        src = img_data,
        style = "cursor: move; transform-origin: center;",
        onmousedown = "handleDragStart(event)",
        onwheel = "handleWheel(event)",
        height = "100%"
      )
    )
  })
  
  # Function to move image to appropriate folder and load next image
  move_and_next <- function(decision) {
    req(rv$current_image)
    
    target_dir <- file.path(here("data", "confirmed_watersheds", decision))
    if (!dir.exists(target_dir)) dir.create(target_dir)
    
    file.copy(
      rv$current_image$datapath,
      file.path(target_dir, rv$current_image$name),
      overwrite = TRUE
    )
    
    rv$image_index <- rv$image_index + 1
    if (rv$image_index <= nrow(rv$images)) {
      rv$current_image <- rv$images[rv$image_index, ]
    } else {
      rv$current_image <- NULL
    }
  }
  
  # Handle button clicks
  observeEvent(input$pass, { move_and_next("passed") })
  observeEvent(input$fail, { move_and_next("failed") })
  observeEvent(input$skip, { move_and_next("skipped") })
}

shinyApp(ui, server)

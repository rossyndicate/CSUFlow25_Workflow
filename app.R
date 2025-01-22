library(shiny)
library(bslib)
library(magick)
library(shinyjs)

# Create directories if they don't exist
dirs <- c("passed", "failed", "skipped")
sapply(dirs, function(d) if (!dir.exists(d)) dir.create(d))

ui <- page_sidebar(
  title = "Image Review Tool",
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
      actionButton("skip", "Skip", class = "btn-warning", width = "100%"),
      hr(),
      sliderInput("zoom", "Zoom Level", min = 0.1, max = 3, value = 1, step = 0.1)
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
  
  # Add necessary JavaScript for drag functionality
  tags$head(
    tags$script("
      var dragItem = null;
      var dragStartX = 0;
      var dragStartY = 0;
      var imageX = 0;
      var imageY = 0;

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
        dragItem.style.transform = 'translate(' + imageX + 'px, ' + imageY + 'px)';
      }

      function handleDragEnd() {
        dragItem = null;
        document.removeEventListener('mousemove', handleDrag);
        document.removeEventListener('mouseup', handleDragEnd);
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
    
    tags$div(
      style = "overflow: hidden; position: relative; height: 600px;",
      tags$img(
        src = rv$current_image$datapath,
        style = sprintf("cursor: move; transform-origin: center; scale: %s;", input$zoom),
        onmousedown = "handleDragStart(event)",
        height = "100%"
      )
    )
  })
  
  # Function to move image to appropriate folder and load next image
  move_and_next <- function(decision) {
    req(rv$current_image)
    
    # Create target directory if it doesn't exist
    target_dir <- file.path(getwd(), decision)
    if (!dir.exists(target_dir)) dir.create(target_dir)
    
    # Move file to appropriate folder
    file.copy(
      rv$current_image$datapath,
      file.path(target_dir, rv$current_image$name),
      overwrite = TRUE
    )
    
    # Move to next image
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

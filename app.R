library(shiny)
library(leaflet)
library(googlesheets4)
library(dplyr)
library(shinyjs)
library(DT)

# Authenticate and load sheet

gs4_auth(path = "minibusschedule-22c11059254e.json")  
sheet_id <- "https://docs.google.com/spreadsheets/d/11qbR5cYRAL5-qbFn4ox2rUYbznNW-wz6H6jAwmadUns/edit?gid=0#gid=0"

get_authorized_drivers <- function() {
# Read authorized drivers
  tryCatch({
    read_sheet(sheet_id, sheet = "Drivers") %>%
      select(ReferenceNumber, Phone, Region, Place) %>%
      mutate(Phone = as.character(Phone))
  }, error = function(e) {
    data.frame(ReferenceNumber = character(), Phone = character(), Region = character(), Place = character())
  })
}

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('get_location', function(message) {
      navigator.geolocation.getCurrentPosition(function(pos) {
        Shiny.setInputValue('driver_lat', pos.coords.latitude);
        Shiny.setInputValue('driver_lon', pos.coords.longitude);
      });
    });
  ")),
  
  titlePanel("Ride Schedule and Monitor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("userType", "I am a:", choices = c("Driver", "Passenger")),
      
      conditionalPanel(
        condition = "input.userType == 'Driver'",
        textInput("refNum", "Enter Your Reference Number"),
        textInput("phone", "Enter Your Phone Number"),
        actionButton("verifyDriver", "Verify Driver"),
        uiOutput("driverForm")
      ),
      
      conditionalPanel(
        condition = "input.userType == 'Passenger'",
        uiOutput("driverSelector"),
        numericInput("numFriends", "Number of people riding with You:", value = 0, min = 0),
        textAreaInput("problemReport", "Any Problems with your ride?", placeholder = "E.g., bus is delayed or off-route"),
        actionButton("submitProblem", "Report Problem"),
        actionButton("submitPassengerInfo", "Submit # of riders (optional)"),
        actionButton("getLocation", "Update Drivers Live Location"),
        actionButton("refreshMap", "Refresh Driver Location (Manual)"),
        actionButton("thumbsUp", HTML("<span style='color:brown;'>👍</span>")),
        actionButton("thumbsDown", HTML("<span style='color:brown;'>👎</span>")),
        textOutput("ratingOutput")
      )
    ),
    
    mainPanel(
      leafletOutput("driverMap", height = 300),
      DTOutput("busTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  authorizedDriver <- reactiveVal(FALSE)
  verified_driver_data <- reactiveVal(NULL)
  ratings <- reactiveValues(data = data.frame(Driver = character(), Thumbs = numeric()))
  bus_schedule <- reactiveVal(read_sheet(sheet_id, sheet = "BusData"))
  drivers_df <- reactiveVal(get_authorized_drivers())
  
  output$driverForm <- renderUI({
    req(authorizedDriver())
    tagList(
      textInput("driverName", "Driver's Name"),
      textInput("time", "Time of operation"),
      textInput("plate", "Location of operation"),
      actionButton("addEntry", "Submit your bus entry"),
      actionButton("stopTracking", "Stop Tracking")
    )
  })
  
  observeEvent(input$verifyDriver, {
    req(input$refNum, input$phone)
    auth <- drivers_df() %>% filter(ReferenceNumber == input$refNum, Phone == input$phone)
    if (nrow(auth) == 1) {
      authorizedDriver(TRUE)
      verified_driver_data(auth)
      session$sendCustomMessage("get_location", list())
      showNotification("Driver verified successfully.", type = "message")
    } else {
      authorizedDriver(FALSE)
      showNotification("Invalid reference number or phone.", type = "error")
    }
  })
  
  output$driverSelector <- renderUI({
    schedule <- bus_schedule()
    drivers <- unique(schedule$Driver)
    selectInput("selectedDriver", "Select Driver:", choices = drivers)
  })
  
  output$driverMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -60.98, lat = 13.9, zoom = 9)
  })
  
  updateMap <- function() {
    req(input$userType == "Passenger", input$selectedDriver)
    tryCatch({
      updated <- read_sheet(sheet_id, sheet = "BusData")
      bus_schedule(updated)
      
      schedule <- updated
      matched <- schedule %>% filter(Driver == input$selectedDriver)
      
      leafletProxy("driverMap") %>% clearMarkers()
      
      if (nrow(matched) > 0 &&
          all(c("latitude", "longitude") %in% colnames(matched)) &&
          !is.na(matched$latitude[1]) && !is.na(matched$longitude[1])) {
        
        leafletProxy("driverMap") %>%
          addPopups(
            lng = matched$longitude[1],
            lat = matched$latitude[1],
            popup = paste(
              "<b>Driver:</b>", input$selectedDriver,
              "<br><b>Region:</b>", matched$Region[1],
              "<br><b>Place:</b>", matched$Place[1]
            )
          ) %>%
          setView(lng = matched$longitude[1], lat = matched$latitude[1], zoom = 13)
      }
    }, error = function(e) {
      showNotification("Error loading location for driver.", type = "error")
    })
  }
  
  observeEvent(input$refreshMap, {
    updateMap()
  })
  
  auto_refresh <- reactiveTimer(15000)
  observe({
    auto_refresh()
    updateMap()
  })
  
  observeEvent(input$addEntry, {
    req(authorizedDriver(), input$driverName, input$time, input$plate)
    auth_data <- verified_driver_data()
    
    new_entry <- data.frame(
      Date = as.character(Sys.Date()),
      Driver = input$driverName,
      Phone = auth_data$Phone[1],
      Time = input$time,
      Plate = input$plate,
      Region = auth_data$Region[1],
      Place = auth_data$Place[1],
      latitude = input$driver_lat %||% NA,
      longitude = input$driver_lon %||% NA,
      Riders = 0
    )
    
    tryCatch({
      sheet_append(ss = sheet_id, sheet = "BusData", data = new_entry)
      updated <- read_sheet(sheet_id, sheet = "BusData")
      bus_schedule(updated)
      showNotification("Bus entry successfully added!", type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to add bus entry:", e$message), type = "error")
    })
  })
  
  observeEvent(input$stopTracking, {
    req(authorizedDriver())
    auth_data <- verified_driver_data()
    schedule <- bus_schedule()
    matched_row <- which(schedule$Driver == input$driverName & schedule$Phone == auth_data$Phone[1])[1]
    if (!is.na(matched_row)) {
      schedule$latitude[matched_row] <- NA
      schedule$longitude[matched_row] <- NA
      
      tryCatch({
        range_write(ss = sheet_id, sheet = "BusData", data = schedule, range = "A1", col_names = TRUE)
        bus_schedule(schedule)
        showNotification("Tracking stopped successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Failed to stop tracking:", e$message), type = "error")
      })
    }
  })
  
  observeEvent(input$getLocation, {
    session$sendCustomMessage("get_location", list())
  })
  
  observeEvent(input$submitProblem, {
    req(nzchar(input$selectedDriver), nzchar(input$problemReport))
    problem_entry <- data.frame(
      Date = as.character(Sys.time()),
      Driver = input$selectedDriver,
      Problem = input$problemReport
    )
    tryCatch({
      sheet_append(ss = sheet_id, sheet = "Problems", data = problem_entry)
      showNotification("Problem reported successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Failed to submit problem:", e$message), type = "error")
    })
  })
  
  observeEvent(input$submitPassengerInfo, {
    req(!is.null(input$selectedDriver), !is.null(input$numFriends))
    schedule <- bus_schedule()
    matched_row <- which(schedule$Driver == input$selectedDriver)[1]
    if (!is.na(matched_row)) {
      schedule$Riders[matched_row] <- schedule$Riders[matched_row] + 1 + input$numFriends
      tryCatch({
        range_write(ss = sheet_id, sheet = "BusData", data = schedule, range = "A1", col_names = TRUE)
        bus_schedule(schedule)
        showNotification("Passenger count updated.", type = "message")
      }, error = function(e) {
        showNotification(paste("Failed to update passenger count:", e$message), type = "error")
      })
    }
  })
  
  observeEvent(input$thumbsUp, {
    req(input$selectedDriver)
    ratings$data <- rbind(ratings$data, data.frame(Driver = input$selectedDriver, Thumbs = 1))
  })
  
  observeEvent(input$thumbsDown, {
    req(input$selectedDriver)
    ratings$data <- rbind(ratings$data, data.frame(Driver = input$selectedDriver, Thumbs = 0))
  })
  
  output$ratingOutput <- renderText({
    req(input$selectedDriver)
    driver_ratings <- ratings$data %>% filter(Driver == input$selectedDriver)
    if (nrow(driver_ratings) > 0) {
      avg <- round(mean(driver_ratings$Thumbs) * 5, 1)
      paste("Average rating for", input$selectedDriver, ":", avg, "stars")
    } else {
      "No ratings yet."
    }
  })
  
  output$busTable <- renderDT({
    datatable(bus_schedule(), options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)

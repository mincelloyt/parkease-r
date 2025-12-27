# app.R
# Load libraries
library(shiny)
library(DT)
library(dplyr)
library(RSQLite)
library(openxlsx)
library(lubridate)
library(shinyjs)

DB_FILE <- "hotel_reservations.db"

# Initialize database once (not in reactive)
initialize_database <- function() {
  conn <- dbConnect(SQLite(), dbname = DB_FILE)
  
  # Create tables if they don't exist
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS reservations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room_number INTEGER NOT NULL,
      room_type TEXT NOT NULL,
      guest_name TEXT NOT NULL,
      guests INTEGER NOT NULL,
      check_in TEXT NOT NULL,
      check_out TEXT NOT NULL,
      payment_status TEXT NOT NULL,
      notes TEXT,
      total_bill REAL,
      status TEXT NOT NULL DEFAULT 'Reserved',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create users table for authentication - SIMPLIFIED
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password TEXT NOT NULL,  -- Plain text for demo
      full_name TEXT NOT NULL,
      role TEXT NOT NULL DEFAULT 'staff',
      is_active INTEGER DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Clear and insert default users
  dbExecute(conn, "DELETE FROM users WHERE username IN ('admin', 'staff')")
  
  dbExecute(conn, "
    INSERT OR REPLACE INTO users (username, password, full_name, role)
    VALUES ('admin', 'admin123', 'Administrator', 'admin')
  ")
  
  dbExecute(conn, "
    INSERT OR REPLACE INTO users (username, password, full_name, role)
    VALUES ('staff', 'staff123', 'Staff User', 'staff')
  ")
  
  # Create status update trigger
  dbExecute(conn, "
    CREATE TRIGGER IF NOT EXISTS update_timestamp 
    AFTER UPDATE ON reservations
    BEGIN
      UPDATE reservations 
      SET updated_at = CURRENT_TIMESTAMP 
      WHERE id = NEW.id;
    END;
  ")
  
  dbDisconnect(conn)
}

# Initialize database on app start
initialize_database()

# Function to verify login credentials
verify_login <- function(username, password) {
  conn <- dbConnect(SQLite(), dbname = DB_FILE)
  on.exit(dbDisconnect(conn))
  
  # Simple plain text comparison for demo
  user <- dbGetQuery(conn, 
                     "SELECT id, username, full_name, role FROM users 
     WHERE username = ? AND password = ? AND is_active = 1",
                     params = list(username, password))
  
  if (nrow(user) == 1) {
    return(list(
      authenticated = TRUE,
      user_id = user$id,
      username = user$username,
      full_name = user$full_name,
      role = user$role
    ))
  } else {
    return(list(authenticated = FALSE))
  }
}

# Login UI
login_ui <- fluidPage(
  useShinyjs(),
  includeCSS("style.css"),
  
  div(class = "login-container",
      div(class = "login-card",
          div(class = "login-header",
              icon("hotel", class = "login-icon"),
              h2("Hotel Reservation System"),
              p("Please sign in to continue")
          ),
          
          div(class = "login-body",
              textInput("login_username", "Username", 
                        placeholder = "Enter your username",
                        width = "100%"),
              
              passwordInput("login_password", "Password", 
                            placeholder = "Enter your password",
                            width = "100%"),
              
              div(class = "login-error",
                  textOutput("login_error")
              ),
              
              actionButton("login_btn", "Sign In", 
                           icon = icon("sign-in-alt"),
                           class = "btn-primary btn-block login-btn"),
              
              div(class = "login-footer",
                  hr(),
                  p("Demo Credentials:"),
                  tags$ul(
                    tags$li("Admin: admin / admin123"),
                    tags$li("Staff: staff / staff123")
                  ),
                  p(class = "login-note",
                    "Note: For demonstration purposes only")
              )
          )
      )
  )
)

# Main application UI
main_ui <- fluidPage(
  useShinyjs(),
  
  # Load external CSS file
  tags$head(
    includeCSS("style.css"),
    tags$script(HTML("
  $(document).ready(function() {
    // Add focus effects to form inputs
    $('.form-control').on('focus', function() {
      $(this).parent().addClass('focused');
    }).on('blur', function() {
      if ($(this).val() === '') {
        $(this).parent().removeClass('focused');
      }
    });
    
    // Button click animation
    $('#login_btn').on('click', function() {
      $(this).addClass('loading');
      setTimeout(function() {
        $('#login_btn').removeClass('loading');
      }, 1000);
    });
    
    // Auto-focus on username field
    setTimeout(function() {
      $('#login_username').focus();
    }, 500);
  });
")),
    
    # Logout button styling
    tags$style(HTML("
      .logout-btn {
        background-color: #d32f2f !important;
        border-color: #c62828 !important;
        color: white !important;
        margin-left: 10px !important;
      }
      .logout-btn:hover {
        background-color: #b71c1c !important;
        border-color: #b71c1c !important;
      }
      .user-info {
        color: #4fc3f7 !important;
        font-weight: bold;
        margin-right: 10px;
      }
    "))
  ),
  
  # Application title with custom navbar
  div(class = "navbar navbar-default",
      div(class = "container-fluid",
          div(class = "navbar-header",
              span(class = "navbar-brand", 
                   icon("hotel"), 
                   " Hotel Reservation Management System"),
              span(class = "user-info",
                   icon("user"),
                   textOutput("current_user", inline = TRUE)
              )
          ),
          div(class = "navbar-right",
              span(class = "navbar-text",
                   textOutput("current_time")
              ),
              actionButton("logout_btn", "Logout", 
                           icon = icon("sign-out-alt"),
                           class = "logout-btn")
          )
      )
  ),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      div(class = "card",
          div(class = "card-header",
              h4(class = "card-title", icon("bed"), " Reservation Details")
          ),
          
          # Validation message area
          uiOutput("validation_message"),
          
          # Room Selection
          fluidRow(
            column(6,
                   div(class = "required-field",
                       selectInput("room_number", "Room Number", 
                                   choices = character(0),
                                   selected = character(0),
                                   width = "100%")
                   ),
                   uiOutput("room_status")
            ),
            column(6,
                   selectInput("room_type", "Room Type",
                               choices = c("Standard" = "Standard",
                                           "Deluxe" = "Deluxe",
                                           "Suite" = "Suite",
                                           "Presidential" = "Presidential"),
                               selected = "Standard",
                               width = "100%")
            )
          ),
          
          # Guest Information
          div(class = "required-field",
              textInput("guest_name", "Guest Name", 
                        placeholder = "Enter full name",
                        width = "100%")
          ),
          
          fluidRow(
            column(6,
                   numericInput("guests", "Number of Guests", 
                                value = 1, min = 1, max = 6, step = 1,
                                width = "100%")
            ),
            column(6,
                   selectInput("payment_status", "Payment Status",
                               choices = c("Unpaid" = "Unpaid",
                                           "Paid" = "Paid",
                                           "Pending" = "Pending"),
                               selected = "Unpaid",
                               width = "100%")
            )
          ),
          
          # Dates
          fluidRow(
            column(6,
                   dateInput("check_in", "Check-In Date", 
                             format = "yyyy-mm-dd",
                             value = Sys.Date(),
                             width = "100%")
            ),
            column(6,
                   dateInput("check_out", "Check-Out Date",
                             format = "yyyy-mm-dd",
                             value = Sys.Date() + 1,
                             width = "100%")
            )
          ),
          
          # Bill Calculation
          div(class = "bill-display",
              div(class = "label", "Total Amount:"),
              div(class = "value", textOutput("total_bill"))
          ),
          
          # Notes
          textAreaInput("notes", "Notes", 
                        rows = 3,
                        placeholder = "Additional notes or special requests...",
                        width = "100%"),
          
          # Status field (visible when updating)
          uiOutput("status_selector"),
          
          br(),
          
          # Action Buttons
          fluidRow(
            column(6,
                   actionButton("add_btn", "Add Reservation", 
                                icon = icon("plus"),
                                class = "btn-success btn-block")
            ),
            column(6,
                   actionButton("update_btn", "Update", 
                                icon = icon("edit"),
                                class = "btn-warning btn-block")
            )
          ),
          
          fluidRow(
            column(6,
                   actionButton("delete_btn", "Delete", 
                                icon = icon("trash"),
                                class = "btn-danger btn-block")
            ),
            column(6,
                   actionButton("clear_btn", "Clear Fields", 
                                icon = icon("broom"),
                                class = "btn-info btn-block")
            )
          ),
          
          # Reservation ID (hidden, for updates)
          uiOutput("current_id")
      ),
      
      # Statistics Card
      div(class = "card",
          div(class = "card-header",
              h4(class = "card-title", icon("chart-bar"), " Quick Stats")
          ),
          uiOutput("stats_display")
      )
    ),
    
    mainPanel(
      width = 8,
      
      # Search and Export Card
      div(class = "card",
          div(class = "card-header",
              h4(class = "card-title", icon("search"), " Search & Export")
          ),
          
          fluidRow(
            column(8,
                   div(style = "padding-top: 25px; height: 63px; display: flex; flex-direction: column; justify-content: flex-end;",
                       textInput("search_input", NULL,
                                 placeholder = "Search by guest name, room number...",
                                 width = "100%")
                   )
            ),
            column(2,
                   div(style = "padding-top: 25px; height: 63px; display: flex; flex-direction: column; justify-content: flex-end;",
                       actionButton("search_btn", "Search", 
                                    icon = icon("search"),
                                    class = "btn-primary btn-block")
                   )
            ),
            column(2,
                   div(style = "padding-top: 25px; height: 63px; display: flex; flex-direction: column; justify-content: flex-end;",
                       actionButton("reset_btn", "Reset", 
                                    icon = icon("sync"),
                                    class = "btn-default btn-block")
                   )
            )
          ),
          
          fluidRow(
            column(3,
                   div(style = "padding-top: 10px;",
                       selectInput("filter_status", "Filter by Status",
                                   choices = c("All", "Reserved", "Checked-Out", "Cancelled"),
                                   selected = "All",
                                   width = "100%")
                   )
            ),
            column(3,
                   div(style = "padding-top: 10px;",
                       selectInput("filter_payment", "Filter by Payment",
                                   choices = c("All", "Paid", "Unpaid", "Pending"),
                                   selected = "All",
                                   width = "100%")
                   )
            ),
            column(3,
                   div(style = "padding-top: 10px;",
                       downloadButton("export_csv", "CSV",
                                      icon = icon("file-csv"),
                                      class = "btn-success btn-block")
                   )
            ),
            column(3,
                   div(style = "padding-top: 10px;",
                       downloadButton("export_excel", "Excel",
                                      icon = icon("file-excel"),
                                      class = "btn-success btn-block")
                   )
            )
          )
      ),
      
      # Dashboard Card
      div(class = "card",
          div(class = "card-header",
              h4(class = "card-title", icon("dashboard"), " Dashboard Analytics")
          ),
          
          fluidRow(
            column(4,
                   dateInput("report_start", "Start Date", 
                             format = "yyyy-mm-dd",
                             value = Sys.Date() - 30,
                             width = "100%")
            ),
            column(4,
                   dateInput("report_end", "End Date",
                             format = "yyyy-mm-dd",
                             value = Sys.Date(),
                             width = "100%")
            ),
            column(4,
                   actionButton("calculate_btn", "Calculate Income",
                                icon = icon("calculator"),
                                class = "btn-primary btn-block",
                                style = "margin-top: 25px;")
            )
          ),
          
          fluidRow(
            column(6,
                   div(class = "income-display",
                       div(class = "label", "Total Income for Selected Period:"),
                       div(class = "value", textOutput("total_income"))
                   )
            ),
            column(6,
                   div(class = "income-display",
                       div(class = "label", "Occupancy Rate:"),
                       div(class = "value", textOutput("occupancy_rate"))
                   )
            )
          )
      ),
      
      # Reservations Table Card
      div(class = "card",
          div(class = "card-header",
              h4(class = "card-title", icon("table"), " Current Reservations")
          ),
          DTOutput("reservations_table")
      ),
      
      # Status Bar
      div(class = "card",
          div(style = "padding: 10px; background-color: #2d2d2d; border-radius: 6px;",
              verbatimTextOutput("status_message")
          )
      )
    )
  )
)

# Combined UI with conditional rendering
ui <- fluidPage(
  uiOutput("page")
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive values for authentication
  user <- reactiveValues(
    authenticated = FALSE,
    user_id = NULL,
    username = NULL,
    full_name = NULL,
    role = NULL,
    login_time = NULL
  )
  
  # Reactive values to store state
  rv <- reactiveValues(
    selected_id = NULL,
    all_rooms = as.character(c(101:120, 201:220, 301:320)),
    refresh_trigger = 0,
    validation_msg = NULL,
    search_trigger = 0
  )
  
  # Database helper functions
  db_connect <- function() {
    conn <- dbConnect(SQLite(), dbname = DB_FILE)
    return(conn)
  }
  
  db_disconnect <- function(conn) {
    tryCatch({
      if (dbIsValid(conn)) {
        dbDisconnect(conn)
      }
    }, error = function(e) {
      # Silently fail if connection is already closed
    })
  }
  
  # Get room rates
  get_room_rate <- function(type) {
    if (is.null(type) || type == "") return(1500)
    
    switch(type,
           "Standard" = 1500,
           "Deluxe" = 2500,
           "Suite" = 4000,
           "Presidential" = 6000,
           1500)
  }
  
  # Load reservations data - MOVED OUTSIDE observe({})
  reservations_data <- reactive({
    # Only run if authenticated
    req(user$authenticated)
    
    # Trigger refresh
    rv$refresh_trigger
    rv$search_trigger
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    tryCatch({
      # Base query - get ALL reservations
      query <- "SELECT * FROM reservations WHERE 1=1"
      params <- list()
      
      # Apply filters
      if (!is.null(input$filter_status) && input$filter_status != "All") {
        query <- paste(query, "AND status = ?")
        params <- c(params, input$filter_status)
      }
      
      if (!is.null(input$filter_payment) && input$filter_payment != "All") {
        query <- paste(query, "AND payment_status = ?")
        params <- c(params, input$filter_payment)
      }
      
      # Apply search if search button was clicked
      if (rv$search_trigger > 0 && !is.null(input$search_input) && input$search_input != "") {
        search_term <- input$search_input
        query <- paste(query, "AND (guest_name LIKE ? OR room_number LIKE ?)")
        search_pattern <- paste0("%", search_term, "%")
        params <- c(params, search_pattern, search_pattern)
      }
      
      query <- paste(query, "ORDER BY id DESC")
      
      if (length(params) > 0) {
        data <- dbGetQuery(conn, query, params = params)
      } else {
        data <- dbGetQuery(conn, query)
      }
      
      if (nrow(data) == 0) {
        return(data.frame(
          id = integer(),
          room_number = integer(),
          room_type = character(),
          guest_name = character(),
          guests = integer(),
          check_in = character(),
          check_out = character(),
          payment_status = character(),
          notes = character(),
          total_bill = numeric(),
          status = character(),
          created_at = character(),
          updated_at = character(),
          stringsAsFactors = FALSE
        ))
      }
      
      return(data)
    }, error = function(e) {
      showNotification(paste("Database error:", e$message), 
                       type = "error", duration = 5)
      return(data.frame())
    })
  })
  
  # Status message - MOVED HERE, OUTSIDE observe({})
  output$status_message <- renderText({
    req(user$authenticated)  # Only run when authenticated
    
    data <- reservations_data()  # This will now work
    
    paste(
      "System Status: Ready",
      "\nLogged in as:", user$full_name, "(", user$role, ")",
      "\nLogin Time:", if(!is.null(user$login_time)) format(user$login_time, "%Y-%m-%d %H:%M:%S") else "N/A",
      "\nTotal Reservations:", nrow(data),
      "\nDatabase:", DB_FILE,
      "\nLast Updated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "\nSelected Reservation:", ifelse(is.null(rv$selected_id), "None", rv$selected_id)
    )
  })
  
  # Render the appropriate UI based on authentication status
  output$page <- renderUI({
    if (!user$authenticated) {
      login_ui
    } else {
      main_ui
    }
  })
  
  # Display current user in navbar
  output$current_user <- renderText({
    if (user$authenticated) {
      paste(user$full_name, "(", user$role, ")")
    }
  })
  
  # Handle login
  observeEvent(input$login_btn, {
    username <- trimws(input$login_username)
    password <- input$login_password
    
    if (username == "" || password == "") {
      output$login_error <- renderText("Please enter both username and password")
      return()
    }
    
    # Verify credentials
    result <- verify_login(username, password)
    
    if (result$authenticated) {
      user$authenticated <- TRUE
      user$user_id <- result$user_id
      user$username <- result$username
      user$full_name <- result$full_name
      user$role <- result$role
      user$login_time <- Sys.time()
      
      # Clear login fields
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")
      
      # Show welcome notification
      showNotification(
        paste("Welcome,", user$full_name, "!"),
        type = "default", duration = 3
      )
    } else {
      output$login_error <- renderText("Invalid username or password")
    }
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    # Show logout notification
    showNotification(
      paste("Goodbye,", user$full_name, "!"),
      type = "default", duration = 3
    )
    
    # Reset user data
    user$authenticated <- FALSE
    user$user_id <- NULL
    user$username <- NULL
    user$full_name <- NULL
    user$role <- NULL
    user$login_time <- NULL
    
    # Reset all form fields
    rv$selected_id <- NULL
    rv$refresh_trigger <- rv$refresh_trigger + 1
  })
  
  # Only run main app logic if authenticated
  observe({
    req(user$authenticated)
    
    # Update current time
    output$current_time <- renderText({
      invalidateLater(60000) # Update every minute
      paste("Current Time:", format(Sys.time(), "%Y-%m-%d %H:%M"))
    })
    
    # Validation message display
    output$validation_message <- renderUI({
      if (!is.null(rv$validation_msg)) {
        div(class = paste("alert-custom", rv$validation_msg$type),
            icon("exclamation-circle"),
            rv$validation_msg$text)
      }
    })
    
    # Clear validation message
    clear_validation <- function() {
      rv$validation_msg <- NULL
    }
    
    # Set validation message
    set_validation <- function(text, type = "alert-error") {
      rv$validation_msg <- list(text = text, type = type)
    }
    
    # Calculate total bill
    output$total_bill <- renderText({
      req(input$check_in, input$check_out, input$room_type)
      
      tryCatch({
        check_in_date <- as.Date(input$check_in)
        check_out_date <- as.Date(input$check_out)
        
        if (check_out_date <= check_in_date) {
          return("₱0.00")
        }
        
        nights <- as.numeric(difftime(check_out_date, check_in_date, units = "days"))
        if (is.na(nights) || nights <= 0) return("₱0.00")
        
        rate <- get_room_rate(input$room_type)
        bill <- nights * rate
        
        return(paste0("₱", format(round(bill, 2), nsmall = 2)))
      }, error = function(e) {
        return("₱0.00")
      })
    })
    
    # Room availability status
    output$room_status <- renderUI({
      req(input$room_number, input$check_in, input$check_out)
      
      tryCatch({
        check_in_date <- as.Date(input$check_in)
        check_out_date <- as.Date(input$check_out)
        
        if (check_out_date <= check_in_date) {
          return(p(icon("times-circle"), "Invalid dates", 
                   style = "color: #ff6b6b; margin-top: 5px;"))
        }
        
        # Check availability
        conn <- db_connect()
        on.exit(db_disconnect(conn))
        
        query <- "
          SELECT guest_name, check_in, check_out 
          FROM reservations 
          WHERE room_number = ? 
            AND status = 'Reserved'
            AND ((? >= check_in AND ? < check_out) 
             OR (? > check_in AND ? <= check_out)
             OR (? <= check_in AND ? >= check_out))
        "
        
        if (!is.null(rv$selected_id)) {
          query <- paste(query, "AND id != ?")
          conflicts <- dbGetQuery(conn, query, params = list(
            as.integer(input$room_number),
            as.character(input$check_in), as.character(input$check_in),
            as.character(input$check_out), as.character(input$check_out),
            as.character(input$check_in), as.character(input$check_out),
            rv$selected_id
          ))
        } else {
          conflicts <- dbGetQuery(conn, query, params = list(
            as.integer(input$room_number),
            as.character(input$check_in), as.character(input$check_in),
            as.character(input$check_out), as.character(input$check_out),
            as.character(input$check_in), as.character(input$check_out)
          ))
        }
        
        if (nrow(conflicts) > 0) {
          return(p(icon("times-circle"), "Room not available", 
                   style = "color: #ff6b6b; margin-top: 5px;"))
        } else {
          return(p(icon("check-circle"), "Room available", 
                   style = "color: #4caf50; margin-top: 5px;"))
        }
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # Get available rooms
    observe({
      req(input$check_in, input$check_out)
      
      tryCatch({
        check_in_date <- as.Date(input$check_in)
        check_out_date <- as.Date(input$check_out)
        
        if (check_out_date <= check_in_date) {
          updateSelectInput(session, "room_number", 
                            choices = rv$all_rooms)
          return()
        }
        
        conn <- db_connect()
        on.exit(db_disconnect(conn))
        
        # Get all reservations for the date range
        query <- "
          SELECT room_number, check_in, check_out 
          FROM reservations 
          WHERE status = 'Reserved'
            AND ((? >= check_in AND ? < check_out) 
             OR (? > check_in AND ? <= check_out)
             OR (? <= check_in AND ? >= check_out))
        "
        
        # Include current reservation ID if updating
        if (!is.null(rv$selected_id)) {
          query <- paste(query, "AND id != ?")
          reservations <- dbGetQuery(conn, query, params = list(
            as.character(input$check_in), as.character(input$check_in),
            as.character(input$check_out), as.character(input$check_out),
            as.character(input$check_in), as.character(input$check_out),
            rv$selected_id
          ))
        } else {
          reservations <- dbGetQuery(conn, query, params = list(
            as.character(input$check_in), as.character(input$check_in),
            as.character(input$check_out), as.character(input$check_out),
            as.character(input$check_in), as.character(input$check_out)
          ))
        }
        
        # Find available rooms
        occupied_rooms <- unique(reservations$room_number)
        available_rooms <- setdiff(rv$all_rooms, as.character(occupied_rooms))
        
        # Sort and update dropdown
        if (length(available_rooms) == 0) {
          available_rooms <- "No rooms available"
          updateSelectInput(session, "room_number", 
                            choices = available_rooms,
                            selected = available_rooms[1])
        } else {
          available_rooms <- sort(as.numeric(available_rooms))
          updateSelectInput(session, "room_number", 
                            choices = available_rooms,
                            selected = ifelse(is.null(input$room_number) || 
                                                !(input$room_number %in% available_rooms),
                                              available_rooms[1],
                                              input$room_number))
        }
      }, error = function(e) {
        showNotification(paste("Error updating rooms:", e$message), 
                         type = "error", duration = 5)
      })
    })
    
    # Status selector (only show when updating)
    output$status_selector <- renderUI({
      if (!is.null(rv$selected_id)) {
        selectInput("status", "Reservation Status",
                    choices = c("Reserved" = "Reserved",
                                "Checked-Out" = "Checked-Out",
                                "Cancelled" = "Cancelled"),
                    selected = "Reserved",
                    width = "100%")
      }
    })
    
    # Current ID display
    output$current_id <- renderUI({
      if (!is.null(rv$selected_id)) {
        tags$div(
          style = "margin-top: 10px; padding: 10px; background-color: #2d2d2d; border-radius: 4px;",
          p(icon("info-circle"), 
            "Editing Reservation ID: ", 
            strong(rv$selected_id),
            style = "margin: 0; color: #4fc3f7;")
        )
      }
    })
    
    # Statistics display
    output$stats_display <- renderUI({
      data <- reservations_data()
      
      total_reservations <- nrow(data)
      total_guests <- if (nrow(data) > 0) sum(data$guests, na.rm = TRUE) else 0
      
      # Count available rooms from current selection
      if (!is.null(input$room_number) && input$room_number != "No rooms available") {
        available_count <- length(rv$all_rooms) - 
          nrow(data[data$status == "Reserved" & 
                      data$check_out >= Sys.Date(), ])
      } else {
        available_count <- 0
      }
      
      total_income <- if (nrow(data) > 0) {
        sum(data$total_bill[data$payment_status == "Paid"], na.rm = TRUE)
      } else {
        0
      }
      
      unpaid_count <- if (nrow(data) > 0) {
        sum(data$payment_status == "Unpaid", na.rm = TRUE)
      } else {
        0
      }
      
      tagList(
        fluidRow(
          column(6,
                 div(class = "value-box",
                     div(class = "value", total_reservations),
                     div(class = "label", "Total Reservations")
                 )
          ),
          column(6,
                 div(class = "value-box",
                     div(class = "value", total_guests),
                     div(class = "label", "Total Guests")
                 )
          )
        ),
        fluidRow(
          column(6,
                 div(class = "value-box",
                     div(class = "value", available_count),
                     div(class = "label", "Available Rooms")
                 )
          ),
          column(6,
                 div(class = "value-box",
                     div(class = "value", unpaid_count),
                     div(class = "label", "Unpaid Bills")
                 )
          )
        ),
        div(class = "value-box",
            div(class = "value", paste0("₱", format(round(total_income, 2), nsmall = 2))),
            div(class = "label", "Total Income (Paid)")
        )
      )
    })
    
    # Reservations table
    output$reservations_table <- renderDT({
      data <- reservations_data()
      
      if (nrow(data) > 0) {
        # Format data
        data_display <- data %>%
          mutate(
            check_in = format(as.Date(check_in), "%Y-%m-%d"),
            check_out = format(as.Date(check_out), "%Y-%m-%d"),
            
            # Create status badges
            status_badge = paste0(
              '<span class="status-badge status-', 
              tolower(gsub("-", "", status)), 
              '">', 
              status, 
              '</span>'
            ),
            payment_badge = paste0(
              '<span class="status-badge status-', 
              tolower(payment_status), 
              '">', 
              payment_status, 
              '</span>'
            ),
            
            # Format bill
            total_bill_formatted = paste0("₱", format(round(total_bill, 2), nsmall = 2)),
            
            # Format timestamps
            created_at = format(as.POSIXct(created_at), "%Y-%m-%d %H:%M"),
            updated_at = format(as.POSIXct(updated_at), "%Y-%m-%d %H:%M")
          ) %>%
          select(
            ID = id,
            Room = room_number,
            Type = room_type,
            Guest = guest_name,
            Guests = guests,
            `Check-In` = check_in,
            `Check-Out` = check_out,
            Payment = payment_badge,
            `Total Bill` = total_bill_formatted,
            Status = status_badge,
            Created = created_at,
            Updated = updated_at
          )
      } else {
        data_display <- data.frame(
          ID = numeric(),
          Room = numeric(),
          Type = character(),
          Guest = character(),
          Guests = numeric(),
          `Check-In` = character(),
          `Check-Out` = character(),
          Payment = character(),
          `Total Bill` = character(),
          Status = character(),
          Created = character(),
          Updated = character(),
          stringsAsFactors = FALSE
        )
      }
      
      # Create DataTable
      datatable(
        data_display,
        rownames = FALSE,
        escape = FALSE,
        selection = 'single',
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50, 100),
          autoWidth = TRUE,
          scrollX = TRUE,
          searching = FALSE,
          info = TRUE,
          paging = TRUE,
          order = list(0, 'desc'),
          columnDefs = list(
            list(targets = c(10, 11), visible = FALSE)
          ),
          language = list(
            emptyTable = "No reservations found",
            info = "Showing _START_ to _END_ of _TOTAL_ reservations",
            infoEmpty = "Showing 0 to 0 of 0 reservations",
            infoFiltered = "(filtered from _MAX_ total reservations)",
            lengthMenu = "Show _MENU_ reservations",
            zeroRecords = "No matching reservations found"
          )
        )
      )
    })
    
    # Row selection handler
    observeEvent(input$reservations_table_rows_selected, {
      clear_validation()
      
      data <- reservations_data()
      if (nrow(data) > 0 && length(input$reservations_table_rows_selected) > 0) {
        row <- data[input$reservations_table_rows_selected, ]
        
        # Store selected ID
        rv$selected_id <- row$id
        
        # Update form fields
        updateSelectInput(session, "room_number", selected = as.character(row$room_number))
        updateSelectInput(session, "room_type", selected = row$room_type)
        updateTextInput(session, "guest_name", value = row$guest_name)
        updateNumericInput(session, "guests", value = row$guests)
        updateDateInput(session, "check_in", value = as.Date(row$check_in))
        updateDateInput(session, "check_out", value = as.Date(row$check_out))
        updateSelectInput(session, "payment_status", selected = row$payment_status)
        updateTextAreaInput(session, "notes", value = ifelse(is.na(row$notes), "", row$notes))
        
      }
    })
    
    # Add reservation
    observeEvent(input$add_btn, {
      clear_validation()
      
      # Validate inputs
      if (is.null(input$guest_name) || trimws(input$guest_name) == "") {
        set_validation("Guest name is required.")
        return()
      }
      
      if (input$room_number == "No rooms available") {
        set_validation("No rooms available for selected dates.")
        return()
      }
      
      if (input$check_out <= input$check_in) {
        set_validation("Check-out date must be after check-in date.")
        return()
      }
      
      # Calculate bill
      nights <- as.numeric(difftime(input$check_out, input$check_in, units = "days"))
      if (nights <= 0) {
        set_validation("Invalid date range.")
        return()
      }
      
      rate <- get_room_rate(input$room_type)
      bill <- nights * rate
      
      # Check room availability one more time
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query <- "
        SELECT COUNT(*) as count FROM reservations 
        WHERE room_number = ? 
          AND status = 'Reserved'
          AND ((? >= check_in AND ? < check_out) 
           OR (? > check_in AND ? <= check_out)
           OR (? <= check_in AND ? >= check_out))
      "
      
      conflicts <- tryCatch({
        dbGetQuery(conn, query, params = list(
          as.integer(input$room_number),
          as.character(input$check_in), as.character(input$check_in),
          as.character(input$check_out), as.character(input$check_out),
          as.character(input$check_in), as.character(input$check_out)
        ))
      }, error = function(e) {
        return(data.frame(count = 0))
      })
      
      if (conflicts$count > 0) {
        set_validation(paste("Room", input$room_number, "is no longer available for the selected dates."))
        return()
      }
      
      # Insert into database
      query <- "
        INSERT INTO reservations 
        (room_number, room_type, guest_name, guests, check_in, check_out, 
         payment_status, notes, total_bill, status)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'Reserved')
      "
      
      tryCatch({
        dbExecute(conn, query, params = list(
          as.integer(input$room_number),
          input$room_type,
          trimws(input$guest_name),
          as.integer(input$guests),
          as.character(input$check_in),
          as.character(input$check_out),
          input$payment_status,
          ifelse(trimws(input$notes) == "", NA, trimws(input$notes)),
          bill
        ))
        
        # Success notification
        showNotification(
          paste("Reservation added successfully for", input$guest_name, 
                "in Room", input$room_number),
          type = "default", duration = 3
        )
        
        # Clear form
        updateTextInput(session, "guest_name", value = "")
        updateTextAreaInput(session, "notes", value = "")
        
        # Refresh data
        rv$refresh_trigger <- rv$refresh_trigger + 1
        
      }, error = function(e) {
        showNotification(paste("Error adding reservation:", e$message), 
                         type = "error", duration = 5)
      })
    })
    
    # Update reservation
    observeEvent(input$update_btn, {
      clear_validation()
      
      # Check if a reservation is selected
      if (is.null(rv$selected_id)) {
        set_validation("Please select a reservation to update.", "alert-warning")
        return()
      }
      
      # Validate inputs
      if (is.null(input$guest_name) || trimws(input$guest_name) == "") {
        set_validation("Guest name is required.")
        return()
      }
      
      if (input$check_out <= input$check_in) {
        set_validation("Check-out date must be after check-in date.")
        return()
      }
      
      # Calculate bill
      nights <- as.numeric(difftime(input$check_out, input$check_in, units = "days"))
      if (nights <= 0) {
        set_validation("Invalid date range.")
        return()
      }
      
      rate <- get_room_rate(input$room_type)
      bill <- nights * rate
      
      # Check room availability (excluding current reservation)
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query <- "
        SELECT COUNT(*) as count FROM reservations 
        WHERE room_number = ? 
          AND status = 'Reserved'
          AND id != ?
          AND ((? >= check_in AND ? < check_out) 
           OR (? > check_in AND ? <= check_out)
           OR (? <= check_in AND ? >= check_out))
      "
      
      conflicts <- dbGetQuery(conn, query, params = list(
        as.integer(input$room_number),
        rv$selected_id,
        as.character(input$check_in), as.character(input$check_in),
        as.character(input$check_out), as.character(input$check_out),
        as.character(input$check_in), as.character(input$check_out)
      ))
      
      if (conflicts$count > 0) {
        set_validation(paste("Room", input$room_number, "is not available for the selected dates."))
        return()
      }
      
      # Update database
      query <- "
        UPDATE reservations SET
        room_number = ?, room_type = ?, guest_name = ?, guests = ?,
        check_in = ?, check_out = ?, payment_status = ?, notes = ?,
        total_bill = ?, status = ?
        WHERE id = ?
      "
      
      tryCatch({
        dbExecute(conn, query, params = list(
          as.integer(input$room_number),
          input$room_type,
          trimws(input$guest_name),
          as.integer(input$guests),
          as.character(input$check_in),
          as.character(input$check_out),
          input$payment_status,
          ifelse(trimws(input$notes) == "", NA, trimws(input$notes)),
          bill,
          ifelse(!is.null(input$status), input$status, "Reserved"),
          rv$selected_id
        ))
        
        showNotification(
          paste("Reservation updated successfully for", input$guest_name),
          type = "default", duration = 3
        )
        
        # Clear selection
        rv$selected_id <- NULL
        
        # Refresh data
        rv$refresh_trigger <- rv$refresh_trigger + 1
        
      }, error = function(e) {
        showNotification(paste("Error updating reservation:", e$message), 
                         type = "error", duration = 5)
      })
    })
    
    # Delete reservation
    observeEvent(input$delete_btn, {
      clear_validation()
      
      # Check if a reservation is selected
      if (is.null(rv$selected_id)) {
        set_validation("Please select a reservation to delete.", "alert-warning")
        return()
      }
      
      # Get reservation details for confirmation
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query <- "SELECT room_number, guest_name FROM reservations WHERE id = ?"
      res_details <- dbGetQuery(conn, query, params = list(rv$selected_id))
      
      if (nrow(res_details) == 0) {
        set_validation("Reservation not found.")
        return()
      }
      
      # Show confirmation dialog
      showModal(
        modalDialog(
          title = tags$div(icon("exclamation-triangle"), " Confirm Delete"),
          tags$div(
            style = "padding: 15px; background-color: #2d2d2d; border-radius: 6px;",
            tags$p(style = "color: #ff6b6b; font-weight: bold;",
                   "WARNING: This action cannot be undone!"),
            tags$p(style = "margin: 10px 0;",
                   "You are about to delete the reservation for:"),
            tags$div(style = "padding: 10px; background-color: #1e1e1e; border-radius: 4px;",
                     tags$p(style = "margin: 5px 0;",
                            tags$strong("Guest: "), res_details$guest_name),
                     tags$p(style = "margin: 5px 0;",
                            tags$strong("Room: "), res_details$room_number),
                     tags$p(style = "margin: 5px 0; color: #ff6b6b;",
                            "All associated data will be permanently removed.")
            )
          ),
          footer = tagList(
            modalButton("Cancel", icon = icon("times")),
            actionButton("confirm_delete", "Delete Reservation", 
                         icon = icon("trash"),
                         class = "btn-danger")
          ),
          easyClose = TRUE,
          size = "m"
        )
      )
    })
    
    # Confirm delete
    observeEvent(input$confirm_delete, {
      tryCatch({
        conn <- db_connect()
        on.exit(db_disconnect(conn))
        
        # Get reservation details before deleting for notification
        query_select <- "SELECT guest_name, room_number FROM reservations WHERE id = ?"
        res_info <- dbGetQuery(conn, query_select, params = list(rv$selected_id))
        
        # Delete the reservation
        query_delete <- "DELETE FROM reservations WHERE id = ?"
        dbExecute(conn, query_delete, params = list(rv$selected_id))
        
        # Show success notification
        showNotification(
          paste("Reservation for", res_info$guest_name, 
                "(Room", res_info$room_number, ") deleted successfully."),
          type = "default", duration = 3
        )
        
        # Clear form and selection
        rv$selected_id <- NULL
        updateTextInput(session, "guest_name", value = "")
        updateTextAreaInput(session, "notes", value = "")
        
        # Refresh data
        rv$refresh_trigger <- rv$refresh_trigger + 1
        
      }, error = function(e) {
        showNotification(paste("Error deleting reservation:", e$message), 
                         type = "error", duration = 5)
      })
      
      removeModal()
    })
    
    # Clear fields
    observeEvent(input$clear_btn, {
      clear_validation()
      
      # Clear form fields
      updateTextInput(session, "guest_name", value = "")
      updateNumericInput(session, "guests", value = 1)
      updateTextAreaInput(session, "notes", value = "")
      
      # Clear selection
      rv$selected_id <- NULL
      
      # Clear table selection
      dataTableProxy("reservations_table") %>%
        selectRows(NULL)
      
      showNotification("Fields cleared.", type = "message", duration = 2)
    })
    
    # Search
    observeEvent(input$search_btn, {
      clear_validation()
      rv$search_trigger <- rv$search_trigger + 1
      showNotification("Search applied.", type = "message", duration = 2)
    })
    
    # Reset search
    observeEvent(input$reset_btn, {
      clear_validation()
      updateTextInput(session, "search_input", value = "")
      updateSelectInput(session, "filter_status", selected = "All")
      updateSelectInput(session, "filter_payment", selected = "All")
      rv$search_trigger <- 0
      showNotification("Filters reset.", type = "message", duration = 2)
    })
    
    # Export CSV
    output$export_csv <- downloadHandler(
      filename = function() {
        paste("hotel_reservations_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- reservations_data()
        if (nrow(data) > 0) {
          write.csv(data, file, row.names = FALSE)
        }
      }
    )
    
    # Export Excel
    output$export_excel <- downloadHandler(
      filename = function() {
        paste("hotel_reservations_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        data <- reservations_data()
        if (nrow(data) > 0) {
          write.xlsx(data, file)
        }
      }
    )
    
    # Calculate income
    total_income <- reactiveVal(0)
    
    observeEvent(input$calculate_btn, {
      clear_validation()
      
      req(input$report_start, input$report_end)
      
      if (input$report_end < input$report_start) {
        set_validation("End date must be after start date.")
        return()
      }
      
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query <- "
        SELECT SUM(total_bill) as total FROM reservations 
        WHERE check_out >= ? AND check_out <= ? 
        AND payment_status = 'Paid'
      "
      
      result <- tryCatch({
        dbGetQuery(conn, query, params = list(
          as.character(input$report_start),
          as.character(input$report_end)
        ))
      }, error = function(e) {
        data.frame(total = 0)
      })
      
      income <- ifelse(is.na(result$total), 0, result$total)
      total_income(income)
      
      showNotification(
        paste("Income calculated for period", 
              format(input$report_start, "%Y-%m-%d"), 
              "to", 
              format(input$report_end, "%Y-%m-%d")),
        type = "default", duration = 3
      )
    })
    
    # Display total income
    output$total_income <- renderText({
      paste0("₱", format(round(total_income(), 2), nsmall = 2))
    })
    
    # Occupancy rate
    output$occupancy_rate <- renderText({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # Get today's occupancy
      query <- "
        SELECT COUNT(DISTINCT room_number) as occupied 
        FROM reservations 
        WHERE ? >= check_in AND ? <= check_out
        AND status = 'Reserved'
      "
      
      occupied <- dbGetQuery(conn, query, params = list(
        as.character(Sys.Date()),
        as.character(Sys.Date())
      ))
      
      total_rooms <- length(rv$all_rooms)
      rate <- if (total_rooms > 0) round((occupied$occupied / total_rooms) * 100, 1) else 0
      
      paste0(rate, "%")
    })
  })
}

shinyApp(ui = ui, server = server)
# app.R - FIXED VERSION WITH PROPER NOTIFICATION TYPES
# Load libraries
library(shiny)
library(DT)
library(dplyr)
library(RSQLite)
library(openxlsx)
library(lubridate)
library(shinyjs)
library(base64enc)  # For image handling
library(shinyjs)

# Load security functions
source("security.R")

DB_FILE <- "hotel_reservations.db"

# Add safe plotting function
safe_plot <- function(expr) {
  tryCatch({
    expr
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, paste("Error in plot:\n", e$message), 
         cex = 1, col = "red")
  })
}

# Function to create default colored placeholder images
create_placeholder_image <- function(color_hex, width = 400, height = 300) {
  # Create a simple colored rectangle as a placeholder
  # This creates a 1x1 pixel image of the specified color
  img <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 
                  0x00, 0x0D, 0x49, 0x48, 0x44, 0x52, 0x00, 0x00, 0x00, 0x01, 
                  0x00, 0x00, 0x00, 0x01, 0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 
                  0x77, 0x53, 0xDE, 0x00, 0x00, 0x00, 0x01, 0x73, 0x52, 0x47, 
                  0x42, 0x00, 0xAE, 0xCE, 0x1C, 0xE9, 0x00, 0x00, 0x00, 0x04, 
                  0x67, 0x41, 0x4D, 0x41, 0x00, 0x00, 0xB1, 0x8F, 0x0B, 0xFC, 
                  0x61, 0x05, 0x00, 0x00, 0x00, 0x09, 0x70, 0x48, 0x59, 0x73, 
                  0x00, 0x00, 0x0E, 0xC3, 0x00, 0x00, 0x0E, 0xC3, 0x01, 0xC7, 
                  0x6F, 0xA8, 0x64, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41, 
                  0x54, 0x08, 0xD7, 0x63, 0xF8, 0xCF, 0x00, 0x00, 0x00, 0x04, 
                  0x00, 0x01, 0xE2, 0x7C, 0x65, 0xFB, 0x00, 0x00, 0x00, 0x00, 
                  0x49, 0x45, 0x4E, 0x44, 0xAE, 0x42, 0x60, 0x82))
  
  # For simplicity, we'll use colored rectangle placeholders
  # You can replace these with actual image files later
  placeholders <- list(
    "Standard" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5/hPwAHAwL/ZIScFAAAAABJRU5ErkJggg==",
    "Deluxe" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=",
    "Suite" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
    "Presidential" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
  )
  
  # Convert hex color to RGB and create a simple colored image
  color_to_placeholder <- function(color_name, hex_color) {
    # Different colored placeholders for each room type
    switch(color_name,
           "Standard" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5/hPwAHAwL/ZIScFAAAAABJRU5ErkJggg==",
           "Deluxe" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=",
           "Suite" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
           "Presidential" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==")
  }
  
  return(color_to_placeholder(color_hex, color_hex))
}

# Function to setup default room images from www folder
setup_default_room_images <- function() {
  conn <- dbConnect(SQLite(), dbname = DB_FILE)
  on.exit(dbDisconnect(conn))
  
  # Define room types and their corresponding floor ranges
  room_types <- list(
    "Standard" = 101:120,
    "Deluxe" = 201:220,
    "Suite" = 301:320,
    "Presidential" = 401:420
  )
  
  # File paths for default images in www folder
  image_files <- list(
    "Standard" = "www/standard_room.jpg",
    "Deluxe" = "www/deluxe_room.jpg",
    "Suite" = "www/suite_room.jpg",
    "Presidential" = "www/presidential_room.jpg"
  )
  
  # Clear existing default images
  dbExecute(conn, "DELETE FROM room_images WHERE image_name LIKE 'default_%'")
  
  # Check which image files exist
  available_images <- list()
  for (room_type in names(image_files)) {
    if (file.exists(image_files[[room_type]])) {
      available_images[[room_type]] <- image_files[[room_type]]
      cat(paste("Found image for", room_type, "rooms:", image_files[[room_type]], "\n"))
    } else {
      cat(paste("Warning: Image file not found for", room_type, "rooms:", image_files[[room_type]], "\n"))
    }
  }
  
  # Create and assign default images for each room
  for (room_type in names(room_types)) {
    if (room_type %in% names(available_images)) {
      # Read the actual image file
      img_path <- available_images[[room_type]]
      tryCatch({
        img_data <- readBin(img_path, "raw", file.info(img_path)$size)
        base64_data <- base64enc::base64encode(img_data)
        file_ext <- tools::file_ext(img_path)
        
        for (room_number in room_types[[room_type]]) {
          # Check if room already has any image
          existing <- dbGetQuery(conn, 
                                 "SELECT COUNT(*) as count FROM room_images WHERE room_number = ?",
                                 params = list(room_number)
          )
          
          if (existing$count == 0) {
            # Insert actual image
            dbExecute(conn, "
              INSERT INTO room_images (room_number, image_data, image_name, image_type, description)
              VALUES (?, ?, ?, ?, ?)
            ", params = list(
              room_number,
              base64_data,
              paste0("default_", tolower(room_type), ".", file_ext),
              file_ext,
              paste("Default", room_type, "Room Image")
            ))
          }
        }
        
        cat(paste("Successfully set default images for", room_type, "rooms\n"))
        
      }, error = function(e) {
        cat(paste("Error loading image for", room_type, ":", e$message, "\n"))
        # Fall back to colored placeholder
        create_default_placeholder(room_type, room_types[[room_type]], conn)
      })
    } else {
      # Use colored placeholder if image file not found
      cat(paste("Using colored placeholder for", room_type, "rooms\n"))
      create_default_placeholder(room_type, room_types[[room_type]], conn)
    }
  }
  
  cat("Default room images setup completed.\n")
}

# Helper function to create colored placeholder images
create_default_placeholder <- function(room_type, room_numbers, conn) {
  # Colored placeholders as fallback
  placeholders <- list(
    "Standard" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5/hPwAHAwL/ZIScFAAAAABJRU5ErkJggg==",
    "Deluxe" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=",
    "Suite" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
    "Presidential" = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
  )
  
  placeholder <- placeholders[[room_type]]
  
  for (room_number in room_numbers) {
    # Check if room already has any image
    existing <- dbGetQuery(conn, 
                           "SELECT COUNT(*) as count FROM room_images WHERE room_number = ?",
                           params = list(room_number)
    )
    
    if (existing$count == 0) {
      # Insert colored placeholder
      dbExecute(conn, "
        INSERT INTO room_images (room_number, image_data, image_name, image_type, description)
        VALUES (?, ?, ?, 'png', ?)
      ", params = list(
        room_number,
        placeholder,
        paste0("default_", tolower(room_type), "_placeholder.png"),
        paste("Default", room_type, "Room - Colored Placeholder")
      ))
    }
  }
}

# Initialize database
initialize_database <- function() {
  conn <- dbConnect(SQLite(), dbname = DB_FILE)
  
  # Create tables if they don't exist
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS reservations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room_number INTEGER NOT NULL,
      room_type TEXT NOT NULL,
      guest_name TEXT NOT NULL,
      email TEXT,
      phone TEXT,
      guests INTEGER NOT NULL,
      check_in TEXT NOT NULL,
      check_out TEXT NOT NULL,
      payment_status TEXT NOT NULL,
      amount_paid REAL DEFAULT 0,
      notes TEXT,
      total_bill REAL,
      status TEXT NOT NULL DEFAULT 'Reserved',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create users table with hashed passwords
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      full_name TEXT NOT NULL,
      email TEXT,
      role TEXT NOT NULL DEFAULT 'staff',
      is_active INTEGER DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create room_images table for storing room pictures
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS room_images (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room_number INTEGER NOT NULL,
      image_data TEXT,  -- Base64 encoded image
      image_name TEXT,
      image_type TEXT,
      description TEXT,
      uploaded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (room_number) REFERENCES reservations(room_number)
    )
  ")
  
  # Clear and insert default users with hashed passwords
  dbExecute(conn, "DELETE FROM users WHERE username IN ('admin', 'staff')")
  
  # Insert admin user with hashed password
  admin_hash <- hash_password("admin123")
  dbExecute(conn, "
    INSERT OR REPLACE INTO users (username, password_hash, full_name, role, email)
    VALUES ('admin', ?, 'Administrator', 'admin', 'admin@hotel.com')
  ", params = list(admin_hash))
  
  # Insert staff user with hashed password
  staff_hash <- hash_password("staff123")
  dbExecute(conn, "
    INSERT OR REPLACE INTO users (username, password_hash, full_name, role, email)
    VALUES ('staff', ?, 'Staff User', 'staff', 'staff@hotel.com')
  ", params = list(staff_hash))
  
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
  
  # Setup default room images
  setup_default_room_images()
}

# Initialize database on app start
initialize_database()

# Function to verify login credentials
verify_login <- function(username, password) {
  conn <- dbConnect(SQLite(), dbname = DB_FILE)
  on.exit(dbDisconnect(conn))
  
  # Get user by username
  user <- dbGetQuery(conn, 
                     "SELECT id, username, password_hash, full_name, role 
                      FROM users 
                      WHERE username = ? AND is_active = 1",
                     params = list(username))
  
  if (nrow(user) == 1) {
    # Verify password hash
    if (verify_password(password, user$password_hash)) {
      return(list(
        authenticated = TRUE,
        user_id = user$id,
        username = user$username,
        full_name = user$full_name,
        role = user$role
      ))
    }
  }
  
  return(list(authenticated = FALSE))
}

# ========== UI COMPONENTS ==========

# Login UI
login_ui <- fluidPage(
  useShinyjs(),
  includeCSS("style.css"),
  
  tags$head(
    tags$script(HTML("
      $(document).ready(function() {
        // Auto-focus on username field
        setTimeout(function() {
          $('#login_username').focus();
        }, 500);
      });
    "))
  ),
  
  # In the login_ui, update the tags$script section to include:
  tags$script(HTML("
  $(document).ready(function() {
    // Initially hide the error container
    $('.login-error').hide();
    
    // Auto-focus on username field
    setTimeout(function() {
      $('#login_username').focus();
    }, 500);
    
    // Clear error when user starts typing in either field
    $('#login_username, #login_password').on('input', function() {
      // Only clear if there's actual text in the error
      if($('#login_error').text().trim() !== '') {
        $('#login_error').text('');
        $('.login-error').slideUp(200);
      }
    });
  });
  
  // Custom message handlers for login errors
  Shiny.addCustomMessageHandler('showLoginError', function(message) {
    $('#login_error').text(message);
    $('.login-error').slideDown(200);
  });
  
  Shiny.addCustomMessageHandler('hideLoginError', function(message) {
    $('#login_error').text('');
    $('.login-error').slideUp(200);
  });
")),
  # Add this to your UI (in the navbar_css section or as a separate tags$script):
  tags$script(HTML("
  $(document).ready(function() {
    // Force selectize dropdowns to have highest z-index
    $(document).on('click', '.selectize-input', function() {
      var $dropdown = $(this).siblings('.selectize-dropdown');
      $dropdown.css('z-index', '100001');
    });
    
    // Fix for payment status dropdown specifically
    $(document).on('focus', '#payment_status + .selectize-control .selectize-input', function() {
      $('.selectize-dropdown').css('z-index', '100001');
    });
    
    // Also fix when date picker opens
    $(document).on('show', '.datepicker', function() {
      $('.selectize-dropdown').css('z-index', '100001');
    });
  });
")),
  # In your UI, add this JavaScript:
  tags$script(HTML("
  $(document).ready(function() {
    // Force close any open date pickers when room modal opens
    $(document).on('click', '.room-card', function() {
      // Close any open date pickers
      $('.datepicker').hide();
      $('.bootstrap-datetimepicker-widget').hide();
      $('.datepicker-dropdown').hide();
    });
    
    // Prevent date pickers from opening in Rooms tab when not needed
    $('#rooms-tab').on('click', function() {
      // Close any date pickers that might be open
      $('.datepicker').hide();
      $('.bootstrap-datetimepicker-widget').hide();
    });
    
    // Ensure date pickers are closed when opening room details modal
    Shiny.addCustomMessageHandler('closeDatePickers', function(message) {
      $('.datepicker').hide();
      $('.bootstrap-datetimepicker-widget').hide();
      $('.datepicker-dropdown').hide();
      $('.dropdown-menu').hide();
    });
  });
")), 
  
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
                    icon("info-circle"), 
                    "Note: For demonstration purposes only")
              )
          )
      )
  )
)

# ========== MAIN APPLICATION UI WITH NAVIGATION BAR ==========

# Custom navbar CSS
navbar_css <- tags$head(
  tags$style(HTML("
    /* Custom Navigation Bar Styles */
    .navbar-default {
      background-color: #1e1e1e !important;
      border-color: #333 !important;
      border-radius: 0 !important;
      margin-bottom: 0 !important;
    }
    
    .navbar-default .navbar-brand {
      color: #ffffff !important;
      font-weight: bold;
      font-size: 18px;
      padding: 15px;
    }
    
    .navbar-default .navbar-brand:hover {
      color: #4fc3f7 !important;
    }
    
    .navbar-default .navbar-nav > li > a {
      color: #e0e0e0 !important;
      font-weight: 500;
      padding: 15px 20px;
    }
    
    .navbar-default .navbar-nav > li > a:hover {
      color: #4fc3f7 !important;
      background-color: #2d2d2d !important;
    }
    
    .navbar-default .navbar-nav > .active > a {
      color: #ffffff !important;
      background-color: #0d47a1 !important;
      font-weight: bold;
    }
    
    .navbar-default .navbar-nav > .active > a:hover {
      color: #ffffff !important;
      background-color: #1565c0 !important;
    }
    
    .navbar-default .navbar-toggle {
      border-color: #444;
    }
    
    .navbar-default .navbar-toggle .icon-bar {
      background-color: #e0e0e0;
    }
    
    .navbar-default .navbar-toggle:hover {
      background-color: #2d2d2d;
    }
    
    /* User info in navbar */
    .user-info-container {
      display: flex;
      align-items: center;
      padding: 15px;
    }
    
    .user-info {
      color: #4fc3f7 !important;
      font-weight: bold;
      margin-right: 15px;
    }
    
    .logout-btn {
      background-color: #d32f2f !important;
      border-color: #c62828 !important;
      color: white !important;
      padding: 6px 12px !important;
      margin: 0 !important;
    }
    
    .logout-btn:hover {
      background-color: #b71c1c !important;
      border-color: #b71c1c !important;
    }
    
    /* Ensure proper spacing */
    .navbar-right {
      margin-right: 0 !important;
    }
    
    /* Room card styles */
    .room-card {
      cursor: pointer;
      transition: all 0.3s ease;
      border-radius: 8px;
      padding: 15px;
      margin-bottom: 15px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      min-height: 120px;
    }
    
    .room-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
    }
    
    .room-available {
      background-color: #d4edda;
      border: 2px solid #28a745;
    }
    
    .room-occupied {
      background-color: #f8d7da;
      border: 2px solid #dc3545;
    }
    
    .room-number {
      font-size: 20px;
      font-weight: bold;
      color: #333;
      margin-bottom: 5px;
    }
    
    .room-status {
      font-size: 14px;
      font-weight: bold;
      padding: 3px 8px;
      border-radius: 4px;
      display: inline-block;
      margin-bottom: 5px;
    }
    
    .room-available .room-status {
      background-color: #28a745;
      color: white;
    }
    
    .room-occupied .room-status {
      background-color: #dc3545;
      color: white;
    }
    
    .room-type {
      font-size: 14px;
      color: #666;
      margin-bottom: 5px;
    }
    
    .guest-name {
      font-size: 12px;
      color: #555;
      font-style: italic;
    }
    
    /* Room image badges in cards */
    .room-image-badge {
      position: absolute;
      top: 10px;
      right: 10px;
      background-color: rgba(0,0,0,0.7);
      color: white;
      padding: 2px 6px;
      border-radius: 12px;
      font-size: 10px;
    }
    
    /* Modal styles for room details */
    .room-image {
      max-width: 100%;
      max-height: 300px;
      border-radius: 8px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
    }
    
    .no-image {
      background-color: #f5f5f5;
      padding: 50px 20px;
      border-radius: 8px;
      text-align: center;
      color: #666;
      border: 2px dashed #ddd;
    }
    
    .current-image-container {
      position: relative;
      margin-bottom: 20px;
    }
    
    .remove-image-btn {
      position: absolute;
      top: 10px;
      right: 10px;
      background-color: rgba(220, 53, 69, 0.9);
      color: white;
      border: none;
      border-radius: 50%;
      width: 30px;
      height: 30px;
      display: flex;
      align-items: center;
      justify-content: center;
      cursor: pointer;
    }
    
    .remove-image-btn:hover {
      background-color: #c82333;
    }
    
    .image-upload-container {
      border: 2px dashed #007bff;
      padding: 20px;
      border-radius: 8px;
      text-align: center;
      background-color: #f8f9fa;
      margin-bottom: 20px;
    }
    
    .room-details-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 15px;
      margin-bottom: 20px;
    }
    
    .room-detail-item {
      background-color: #f8f9fa;
      padding: 10px;
      border-radius: 4px;
      border-left: 4px solid #007bff;
    }
    
    .room-detail-label {
      font-weight: bold;
      color: #495057;
      font-size: 12px;
      text-transform: uppercase;
    }
    
    .room-detail-value {
      font-size: 16px;
      color: #212529;
    }
    
    /* Colored room type indicators */
    .room-type-standard { border-left-color: #4CAF50 !important; }
    .room-type-deluxe { border-left-color: #2196F3 !important; }
    .room-type-suite { border-left-color: #9C27B0 !important; }
    .room-type-presidential { border-left-color: #FF9800 !important; }
    
    .room-badge-standard { background-color: #4CAF50; color: white; }
    .room-badge-deluxe { background-color: #2196F3; color: white; }
    .room-badge-suite { background-color: #9C27B0; color: white; }
    .room-badge-presidential { background-color: #FF9800; color: white; }
    
    /* ========== CRITICAL Z-INDEX FIXES ========== */
    /* Fix payment status dropdown in reservations tab */
    .selectize-dropdown {
      z-index: 10050 !important;
    }
    
    .selectize-dropdown-content {
      z-index: 10050 !important;
    }
    
    .selectize-input {
      z-index: 10049 !important;
    }
    
    .selectize-control.single .selectize-input,
    .selectize-control.single .selectize-input.input-active {
      z-index: 10049 !important;
    }
    
    /* Make sure all dropdowns are above date pickers */
    .selectize-dropdown,
    .bootstrap-select .dropdown-menu,
    .select2-container--open .select2-dropdown {
      z-index: 1060 !important;
    }
    
    /* Fix select inputs specifically in reservations tab */
    #reservations-tab .selectize-dropdown {
      z-index: 99999 !important;
    }
    
    #reservations-tab .selectize-control {
      position: relative;
      z-index: 9999;
    }
    
    /* Ensure all form controls in reservations tab have proper z-index */
    #reservations-tab .form-control,
    #reservations-tab .selectize-control,
    #reservations-tab .selectize-input {
      position: relative;
      z-index: 100;
    }
    
    /* Higher z-index for dropdowns when opened */
    #reservations-tab .selectize-dropdown {
      z-index: 100000 !important;
    }
    
    #reservations-tab .bootstrap-select .dropdown-menu {
      z-index: 100000 !important;
    }
  "))
)

# Add to navbar_css or create separate CSS - UPDATE THIS ENTIRE BLOCK:
navbar_css <- tags$head(
  tags$style(HTML("
    /* Fix date picker calendar z-index */
    .datepicker {
      z-index: 9999 !important;
    }
    
    .bootstrap-datetimepicker-widget {
      z-index: 9999 !important;
    }
    
    /* Specific fix for Rooms tab date pickers */
    .shiny-date-input {
      z-index: 1000 !important;
    }
    
    /* Ensure calendar dropdown is on top */
    .datepicker-dropdown {
      z-index: 10000 !important;
    }
    
    /* Fix for modal backdrop interference */
    .modal-backdrop {
      z-index: 999 !important;
    }
    
    .modal {
      z-index: 1000 !important;
    }
    
    /* Room cards modal specific fixes */
    #shiny-modal .datepicker {
      z-index: 99999 !important;
    }
    
    #shiny-modal .bootstrap-datetimepicker-widget {
      z-index: 99999 !important;
    }
    
    /* Ensure room detail modal doesn't interfere */
    .room-details-modal .datepicker {
      z-index: 99999 !important;
    }
    
    /* ===== CRITICAL FIX FOR PAYMENT STATUS DROPDOWN ===== */
    /* Make selectize dropdowns appear above date pickers */
    .selectize-dropdown {
      z-index: 10050 !important;
    }
    
    .selectize-dropdown-content {
      z-index: 10050 !important;
    }
    
    .selectize-input {
      z-index: 10049 !important;
    }
    
    /* Specific fix for payment status in reservations */
    #payment_status + .selectize-control .selectize-dropdown {
      z-index: 100000 !important;
    }
    
    #payment_status + .selectize-control {
      position: relative;
      z-index: 9999;
    }
    
    /* Force higher z-index for all selectize in reservations tab */
    #reservations-tab .selectize-dropdown {
      z-index: 100001 !important;
    }
    
    #reservations-tab .selectize-input {
      position: relative;
      z-index: 100000 !important;
    }
    
    /* Bootstrap select dropdown fixes */
    .bootstrap-select .dropdown-menu {
      z-index: 10050 !important;
    }
    
    /* Higher specificity for payment status */
    select#payment_status + .selectize-control .selectize-dropdown {
      z-index: 100001 !important;
    }
    
    /* Make sure the dropdown appears above everything */
    .selectize-dropdown,
    .bootstrap-select .dropdown-menu {
      position: absolute !important;
      z-index: 100001 !important;
    }
  "))
)
# Add to navbar_css or create separate CSS
navbar_css <- tags$head(
  tags$style(HTML("
    /* Fix date picker calendar z-index */
    .datepicker {
      z-index: 9999 !important;
    }
    
    .bootstrap-datetimepicker-widget {
      z-index: 9999 !important;
    }
    
    /* Specific fix for Rooms tab date pickers */
    .shiny-date-input {
      z-index: 1000 !important;
    }
    
    /* Ensure calendar dropdown is on top */
    .datepicker-dropdown {
      z-index: 10000 !important;
    }
    
    /* Fix for modal backdrop interference */
    .modal-backdrop {
      z-index: 999 !important;
    }
    
    .modal {
      z-index: 1000 !important;
    }
    
    /* Room cards modal specific fixes */
    #shiny-modal .datepicker {
      z-index: 99999 !important;
    }
    
    #shiny-modal .bootstrap-datetimepicker-widget {
      z-index: 99999 !important;
    }
    
    /* Ensure room detail modal doesn't interfere */
    .room-details-modal .datepicker {
      z-index: 99999 !important;
    }
  "))
)

# Main UI with custom navbar
main_ui <- tagList(
  navbar_css,
  navbarPage(
    title = div(
      style = "width: 100%; position: relative;",
      div(
        style = "display: flex; align-items: center;",
        icon("hotel", style = "margin-right: 10px;"),
        "Hotel Reservation Management System"
      ),
      div(
        class = "navbar-user-container",
        span(class = "navbar-user-info",
             icon("user"),
             textOutput("current_user", inline = TRUE)
        ),
        actionButton("logout_btn", "Logout", 
                     icon = icon("sign-out-alt"),
                     class = "navbar-logout-btn")
      )
    ),
    id = "main_navbar",
    collapsible = TRUE,
    inverse = TRUE,
    windowTitle = "Hotel Reservation System",
    
    # ===== DASHBOARD TAB =====
    tabPanel(
      title = icon("dashboard", style = "margin-right: 5px;"),
      "Dashboard",
      value = "dashboard",
      
      fluidPage(
        fluidRow(
          column(4,
                 div(class = "card",
                     div(class = "card-header",
                         h4(class = "card-title", icon("chart-bar"), " Quick Stats")
                     ),
                     uiOutput("stats_display")
                 ),
                 
                 div(class = "card",
                     div(class = "card-header",
                         h4(class = "card-title", icon("calendar-alt"), " Today's Activity")
                     ),
                     uiOutput("todays_activity")
                 )
          ),
          
          column(8,
                 div(class = "card",
                     div(class = "card-header",
                         h4(class = "card-title", icon("chart-line"), " Analytics Dashboard")
                     ),
                     
                     fluidRow(
                       column(6,
                              dateInput("report_start", "Start Date", 
                                        format = "yyyy-mm-dd",
                                        value = Sys.Date() - 30,
                                        width = "100%")
                       ),
                       column(6,
                              dateInput("report_end", "End Date",
                                        format = "yyyy-mm-dd",
                                        value = Sys.Date(),
                                        width = "100%")
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
                     ),
                     
                     fluidRow(
                       column(12,
                              actionButton("calculate_btn", "Calculate Analytics",
                                           icon = icon("calculator"),
                                           class = "btn-primary btn-block",
                                           style = "margin-top: 10px;")
                       )
                     )
                 ),
                 
                 div(class = "card",
                     div(class = "card-header",
                         h4(class = "card-title", icon("table"), " Recent Reservations")
                     ),
                     DTOutput("recent_reservations_table")
                 )
          )
        )
      )
    ),
    
    # ===== RESERVATIONS TAB =====
    tabPanel(
      title = icon("bed", style = "margin-right: 5px;"),
      "Reservations",
      value = "reservations",
      
      fluidPage(
        fluidRow(
          column(4,
                 div(class = "card",
                     div(class = "card-header",
                         h4(class = "card-title", icon("plus-circle"), " New Reservation")
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
                              uiOutput("room_status"),
                              uiOutput("room_type_info")
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
                              textInput("guest_email", "Email", 
                                        placeholder = "guest@example.com",
                                        width = "100%")
                       ),
                       column(6,
                              textInput("guest_phone", "Phone", 
                                        placeholder = "+63 123 456 7890",
                                        width = "100%")
                       )
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
                 )
          ),
          
          column(8,
                 div(class = "card",
                     div(class = "card-header",
                         h4(class = "card-title", icon("search"), " Search & Filter")
                     ),
                     
                     fluidRow(
                       class = "search-row",
                       column(8,
                              div(class = "search-input-container",
                                  textInput("search_input", NULL,
                                            placeholder = "Search by guest name, room number...",
                                            width = "100%")
                              )
                       ),
                       column(2,
                              div(class = "button-container",
                                  actionButton("search_btn", "Search", 
                                               icon = icon("search"),
                                               class = "btn-primary btn-block")
                              )
                       ),
                       column(2,
                              div(class = "button-container",
                                  actionButton("reset_btn", "Reset", 
                                               icon = icon("sync"),
                                               class = "btn-default btn-block")
                              )
                       )
                     ),
                     
                     fluidRow(
                       column(3,
                              selectInput("filter_status", "Filter by Status",
                                          choices = c("All", "Reserved", "Checked-In", "Checked-Out", "Cancelled"),
                                          selected = "All",
                                          width = "100%")
                       ),
                       column(3,
                              selectInput("filter_payment", "Filter by Payment",
                                          choices = c("All", "Paid", "Unpaid", "Pending"),
                                          selected = "All",
                                          width = "100%")
                       ),
                       column(3,
                              div(class = "export-button",
                                  downloadButton("export_csv", "CSV",
                                                 icon = icon("file-csv"),
                                                 class = "btn-success btn-block")
                              )
                       ),
                       column(3,
                              div(class = "export-button",
                                  downloadButton("export_excel", "Excel",
                                                 icon = icon("file-excel"),
                                                 class = "btn-success btn-block")
                              )
                       )
                     )
                 ),
                 
                 div(class = "card",
                     div(class = "card-header",
                         h4(class = "card-title", icon("table"), " All Reservations")
                     ),
                     DTOutput("reservations_table")
                 )
          )
        )
      )
    ),
    
    # ===== ROOMS TAB =====
    # ===== ROOMS TAB =====
    tabPanel(
      title = icon("door-closed", style = "margin-right: 5px;"),
      "Rooms",
      value = "rooms",
      
      fluidPage(
        div(class = "card",
            div(class = "card-header",
                h4(class = "card-title", icon("door-closed"), " Room Management")
            ),
            
            fluidRow(
              column(12,
                     h4("Room Availability for Selected Dates:"),
                     fluidRow(
                       column(6,
                              div(style = "position: relative; z-index: 9999;",  # ADD THIS WRAPPER
                                  dateInput("room_check_in", "Check-In Date", 
                                            format = "yyyy-mm-dd",
                                            value = Sys.Date(),
                                            width = "100%")
                              )
                       ),
                       column(6,
                              div(style = "position: relative; z-index: 9999;",  # ADD THIS WRAPPER
                                  dateInput("room_check_out", "Check-Out Date",
                                            format = "yyyy-mm-dd",
                                            value = Sys.Date() + 1,
                                            width = "100%")
                              )
                       )
                     ),
                     actionButton("refresh_rooms", "Refresh Availability",
                                  icon = icon("sync"),
                                  class = "btn-primary",
                                  style = "margin-bottom: 20px;")
              )
            ),
            
            uiOutput("rooms_display")
        )
      )
    ),
    
    # ===== REPORTS TAB =====
    tabPanel(
      title = icon("file-alt", style = "margin-right: 5px;"),
      "Reports",
      value = "reports",
      
      fluidPage(
        div(class = "card",
            div(class = "card-header",
                h4(class = "card-title", icon("chart-pie"), " Financial Reports & Analytics")
            ),
            
            fluidRow(
              column(4,
                     dateInput("report_start_date", "Start Date", 
                               format = "yyyy-mm-dd",
                               value = Sys.Date() - 365,  # Default to 1 year
                               width = "100%")
              ),
              column(4,
                     dateInput("report_end_date", "End Date",
                               format = "yyyy-mm-dd",
                               value = Sys.Date(),
                               width = "100%")
              ),
              column(4,
                     actionButton("generate_report", "Generate Report",
                                  icon = icon("chart-bar"),
                                  class = "btn-primary btn-block",
                                  style = "margin-top: 25px;")
              )
            ),
            
            # Financial Summary Card
            fluidRow(
              column(12,
                     div(class = "card",
                         div(class = "card-header",
                             h5("Financial Summary")
                         ),
                         verbatimTextOutput("financial_summary")
                     )
              )
            ),
            
            # Visual Charts Section
            fluidRow(
              column(4,
                     div(class = "card",
                         div(class = "card-header",
                             h5(icon("chart-bar"), " Revenue by Room Type")
                         ),
                         plotOutput("revenue_by_room_type", height = "300px")
                     )
              ),
              column(4,
                     div(class = "card",
                         div(class = "card-header",
                             h5(icon("chart-line"), " Monthly Revenue Trends")
                         ),
                         plotOutput("monthly_revenue_trends", height = "300px")
                     )
              ),
              column(4,
                     div(class = "card",
                         div(class = "card-header",
                             h5(icon("chart-pie"), " Payment Status Distribution")
                         ),
                         plotOutput("payment_status_dist", height = "300px")
                     )
              )
            ),
            
            # Additional Charts
            fluidRow(
              column(6,
                     div(class = "card",
                         div(class = "card-header",
                             h5(icon("calendar"), " Bookings by Month")
                         ),
                         plotOutput("bookings_by_month", height = "300px")
                     )
              ),
              column(6,
                     div(class = "card",
                         div(class = "card-header",
                             h5(icon("bed"), " Room Type Occupancy")
                         ),
                         plotOutput("room_type_occupancy", height = "300px")
                     )
              )
            ),
            
            # Download Section
            fluidRow(
              column(12,
                     div(class = "card",
                         div(class = "card-header",
                             h5(icon("download"), " Export Reports")
                         ),
                         fluidRow(
                           column(3,
                                  downloadButton("download_summary", "Summary Report",
                                                 icon = icon("file-pdf"),
                                                 class = "btn-danger btn-block")
                           ),
                           column(3,
                                  downloadButton("download_excel", "Full Excel Report",
                                                 icon = icon("file-excel"),
                                                 class = "btn-success btn-block")
                           ),
                           column(3,
                                  downloadButton("download_charts", "Charts as PDF",
                                                 icon = icon("chart-bar"),
                                                 class = "btn-info btn-block")
                           ),
                           column(3,
                                  downloadButton("download_csv", "Raw Data CSV",
                                                 icon = icon("file-csv"),
                                                 class = "btn-warning btn-block")
                           )
                         )
                     )
              )
            )
        )
      )
    ),
    
    # ===== SETTINGS TAB =====
    tabPanel(
      title = icon("cog", style = "margin-right: 5px;"),
      "Settings",
      value = "settings",
      
      fluidPage(
        div(class = "card",
            div(class = "card-header",
                h4(class = "card-title", icon("user-cog"), " User Settings")
            ),
            
            fluidRow(
              column(6,
                     div(class = "value-box",
                         div(class = "value", textOutput("current_user_name")),
                         div(class = "label", "Logged in as")
                     )
              ),
              column(6,
                     div(class = "value-box",
                         div(class = "value", textOutput("user_role")),
                         div(class = "label", "Role")
                     )
              )
            ),
            
            fluidRow(
              column(12,
                     h4("Change Password"),
                     passwordInput("current_password", "Current Password",
                                   width = "100%"),
                     passwordInput("new_password", "New Password",
                                   width = "100%"),
                     passwordInput("confirm_password", "Confirm New Password",
                                   width = "100%"),
                     actionButton("change_password", "Change Password",
                                  icon = icon("key"),
                                  class = "btn-warning")
              )
            )
        )
      )
    )
  )
)

# Combined UI with conditional rendering
ui <- fluidPage(
  useShinyjs(),
  includeCSS("style.css"),
  uiOutput("page")
)

# ========== SERVER LOGIC ==========
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
    selected_room = NULL,
    current_room_image = NULL,
    temp_image_data = NULL,
    all_rooms = as.character(c(101:120, 201:220, 301:320, 401:420)),  # Added 4th floor
    refresh_trigger = 0,
    validation_msg = NULL,
    calculated_income = 0,
    refresh_counter = 0,
    last_db_update = Sys.time(),
    report_generated = FALSE
  )
  
  # Helper functions
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
  
  get_room_rate <- function(type) {
    if (is.null(type) || type == "") return(1500)
    
    switch(type,
           "Standard" = 1500,
           "Deluxe" = 2500,
           "Suite" = 4000,
           "Presidential" = 6000,
           1500)
  }
  
  format_currency <- function(amount) {
    paste0("₱", format(round(amount, 2), nsmall = 2, big.mark = ","))
  }
  
  # Get room amenities based on type
  get_room_amenities <- function(room_type) {
    switch(room_type,
           "Standard" = c("Queen Bed", "TV", "AC", "Free WiFi", "Private Bathroom"),
           "Deluxe" = c("King Bed", "Smart TV", "AC", "Free WiFi", "Mini Bar", "Work Desk", "Private Bathroom"),
           "Suite" = c("King Bed", "Smart TV", "AC", "Free WiFi", "Mini Bar", "Work Desk", "Sofa", "Coffee Maker", "Jacuzzi"),
           "Presidential" = c("King Bed", "Smart TV", "AC", "Free WiFi", "Mini Bar", "Work Desk", "Sofa", "Coffee Maker", "Jacuzzi", "Balcony", "Kitchenette", "Living Area"),
           c("Queen Bed", "TV", "AC", "Free WiFi", "Private Bathroom")
    )
  }
  
  # Get rooms by type
  get_rooms_by_type <- function(room_type) {
    switch(room_type,
           "Standard" = as.character(101:120),
           "Deluxe" = as.character(201:220),
           "Suite" = as.character(301:320),
           "Presidential" = as.character(401:420),
           as.character(101:120)  # Default to Standard
    )
  }
  
  # Get room image from database
  get_room_image <- function(room_number) {
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    query <- "SELECT image_data, image_name, image_type FROM room_images WHERE room_number = ? ORDER BY uploaded_at DESC LIMIT 1"
    result <- dbGetQuery(conn, query, params = list(room_number))
    
    if (nrow(result) > 0) {
      return(list(
        data = result$image_data[1],
        name = result$image_name[1],
        type = result$image_type[1]
      ))
    } else {
      return(NULL)
    }
  }
  
  # Save room image to database
  save_room_image <- function(room_number, image_path, image_name) {
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    # Read image file and convert to base64
    if (!is.null(image_path) && file.exists(image_path)) {
      img_data <- readBin(image_path, "raw", file.info(image_path)$size)
      base64_data <- base64enc::base64encode(img_data)
      
      # Get file extension
      file_ext <- tools::file_ext(image_name)
      
      # Delete existing image for this room
      dbExecute(conn, "DELETE FROM room_images WHERE room_number = ?", 
                params = list(room_number))
      
      # Insert new image
      dbExecute(conn, "
        INSERT INTO room_images (room_number, image_data, image_name, image_type)
        VALUES (?, ?, ?, ?)
      ", params = list(
        room_number,
        base64_data,
        image_name,
        file_ext
      ))
      
      return(TRUE)
    }
    return(FALSE)
  }
  
  # Delete room image
  delete_room_image <- function(room_number) {
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    dbExecute(conn, "DELETE FROM room_images WHERE room_number = ?", 
              params = list(room_number))
    
    return(TRUE)
  }
  
  # Safe NULL coalescing operator
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Validation helper functions
  clear_validation <- function() {
    rv$validation_msg <- NULL
  }
  
  set_validation <- function(text, type = "alert-error") {
    rv$validation_msg <- list(text = text, type = type)
  }
  
  # Load reservations data
  reservations_data <- reactive({
    req(user$authenticated)
    
    rv$refresh_trigger
    rv$refresh_counter
    rv$last_db_update
    
    input$filter_status
    input$filter_payment
    input$search_input
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    tryCatch({
      query <- "SELECT * FROM reservations WHERE 1=1"
      
      f_status <- input$filter_status
      f_payment <- input$filter_payment
      f_search <- input$search_input
      
      params <- list()
      
      if (!is.null(f_status) && f_status != "All") {
        query <- paste(query, "AND status = ?")
        params <- c(params, f_status)
      }
      
      if (!is.null(f_payment) && f_payment != "All") {
        query <- paste(query, "AND payment_status = ?")
        params <- c(params, f_payment)
      }
      
      if (!is.null(f_search) && f_search != "") {
        query <- paste(query, "AND (guest_name LIKE ? OR room_number LIKE ? OR email LIKE ? OR phone LIKE ?)")
        search_pattern <- paste0("%", f_search, "%")
        params <- c(params, search_pattern, search_pattern, search_pattern, search_pattern)
      }
      
      query <- paste(query, "ORDER BY id DESC")
      
      if (length(params) > 0) {
        data <- dbGetQuery(conn, query, params = params)
      } else {
        data <- dbGetQuery(conn, query)
      }
      
      return(data)
      
    }, error = function(e) {
      showNotification(paste("Database error:", e$message), type = "error", duration = 5)
      return(data.frame())
    })
  })
  
  # ========== UI RENDERING ==========
  
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
  
  # Handle login - FIXED VERSION
  # Handle login - SIMPLIFIED AND WORKING VERSION
  observeEvent(input$login_btn, {
    # Clear any previous error message
    output$login_error <- renderText("")
    
    username <- trimws(input$login_username)
    password <- input$login_password
    
    # Show validation messages only if fields are empty
    if (username == "" && password == "") {
      output$login_error <- renderText("Please enter both username and password")
      # Force the error to show immediately
      session$sendCustomMessage(type = "showLoginError", message = "Please enter both username and password")
      return()
    } else if (username == "") {
      output$login_error <- renderText("Please enter username")
      session$sendCustomMessage(type = "showLoginError", message = "Please enter username")
      return()
    } else if (password == "") {
      output$login_error <- renderText("Please enter password")
      session$sendCustomMessage(type = "showLoginError", message = "Please enter password")
      return()
    }
    
    # Attempt login only if both fields have content
    result <- verify_login(username, password)
    
    if (result$authenticated) {
      user$authenticated <- TRUE
      user$user_id <- result$user_id
      user$username <- result$username
      user$full_name <- result$full_name
      user$role <- result$role
      user$login_time <- Sys.time()
      
      # Clear the login form
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")
      
      # Hide error message
      session$sendCustomMessage(type = "hideLoginError", message = "")
      
      showNotification(
        paste("Welcome,", user$full_name, "!"),
        type = "default", duration = 3
      )
    } else {
      # Show invalid credentials error
      output$login_error <- renderText("Invalid username or password. Please try again.")
      session$sendCustomMessage(type = "showLoginError", message = "Invalid username or password. Please try again.")
    }
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    showNotification(
      paste("Goodbye,", user$full_name, "!"),
      type = "warning", duration = 3
    )
    
    user$authenticated = FALSE
    user$user_id = NULL
    user$username = NULL
    user$full_name = NULL
    user$role = NULL
    user$login_time = NULL
    
    rv$selected_id <- NULL
    rv$selected_room <- NULL
    rv$current_room_image <- NULL
    rv$temp_image_data <- NULL
    rv$refresh_trigger <- rv$refresh_trigger + 1
  })
  
  # ========== DASHBOARD TAB ==========
  
  # Statistics display
  output$stats_display <- renderUI({
    req(user$authenticated)
    
    data <- reservations_data()
    
    total_reservations <- nrow(data)
    total_guests <- if (nrow(data) > 0) sum(data$guests, na.rm = TRUE) else 0
    
    available_count <- length(rv$all_rooms) - 
      nrow(data[data$status %in% c("Reserved", "Checked-In") & 
                  as.Date(data$check_out) >= Sys.Date(), ])
    
    total_paid_income <- if (nrow(data) > 0) {
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
        column(12,
               div(class = "value-box",
                   div(class = "value", total_reservations),
                   div(class = "label", "Total Reservations")
               )
        )
      ),
      fluidRow(
        column(6,
               div(class = "value-box",
                   div(class = "value", total_guests),
                   div(class = "label", "Total Guests")
               )
        ),
        column(6,
               div(class = "value-box",
                   div(class = "value", available_count),
                   div(class = "label", "Available Rooms")
               )
        )
      ),
      fluidRow(
        column(6,
               div(class = "value-box",
                   div(class = "value", unpaid_count),
                   div(class = "label", "Unpaid Bills")
               )
        ),
        column(6,
               div(class = "value-box",
                   div(class = "value", format_currency(total_paid_income)),
                   div(class = "label", "Total Income")
               )
        )
      )
    )
  })
  
  # Today's activity
  output$todays_activity <- renderUI({
    req(user$authenticated)
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    today <- Sys.Date()
    
    # Today's arrivals
    arrivals <- dbGetQuery(conn, "
      SELECT COUNT(*) as count FROM reservations 
      WHERE DATE(check_in) = ? AND status = 'Reserved'
    ", params = list(as.character(today)))
    
    # Today's departures
    departures <- dbGetQuery(conn, "
      SELECT COUNT(*) as count FROM reservations 
      WHERE DATE(check_out) = ? AND status IN ('Checked-In', 'Reserved')
    ", params = list(as.character(today)))
    
    # Today's check-ins
    checkins <- dbGetQuery(conn, "
      SELECT COUNT(*) as count FROM reservations 
      WHERE status = 'Checked-In' AND DATE(updated_at) = ?
    ", params = list(as.character(today)))
    
    tagList(
      fluidRow(
        column(6,
               div(class = "value-box",
                   div(class = "value", arrivals$count),
                   div(class = "label", "Today's Arrivals")
               )
        ),
        column(6,
               div(class = "value-box",
                   div(class = "value", departures$count),
                   div(class = "label", "Today's Departures")
               )
        )
      ),
      fluidRow(
        column(12,
               div(class = "value-box",
                   div(class = "value", checkins$count),
                   div(class = "label", "Checked-In Today")
               )
        )
      )
    )
  })
  
  # Recent reservations table
  output$recent_reservations_table <- renderDT({
    req(user$authenticated)
    
    data <- reservations_data()
    
    if (nrow(data) > 0) {
      data_display <- head(data, 10) %>%
        mutate(
          check_in = format(as.Date(check_in), "%Y-%m-%d"),
          check_out = format(as.Date(check_out), "%Y-%m-%d"),
          total_bill_formatted = format_currency(total_bill)
        ) %>%
        select(
          ID = id,
          Room = room_number,
          Guest = guest_name,
          `Check-In` = check_in,
          `Check-Out` = check_out,
          Status = status,
          Payment = payment_status,
          `Total Bill` = total_bill_formatted
        )
    } else {
      data_display <- data.frame(
        Message = "No recent reservations"
      )
    }
    
    datatable(
      data_display,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        ordering = FALSE
      )
    )
  })
  
  # Calculate income
  observeEvent(input$calculate_btn, {
    req(user$authenticated)
    
    clear_validation()
    
    tryCatch({
      if (is.null(input$report_start) || is.null(input$report_end)) {
        set_validation("Please select both start and end dates.")
        return()
      }
      
      if (input$report_end < input$report_start) {
        set_validation("End date must be after start date.")
        return()
      }
      
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query <- "
        SELECT SUM(total_bill) as total 
        FROM reservations 
        WHERE payment_status = 'Paid'
          AND (check_out >= ? AND check_in <= ?)
      "
      
      result <- dbGetQuery(conn, query, params = list(
        as.character(input$report_start),
        as.character(input$report_end)
      ))
      
      income <- ifelse(is.na(result$total), 0, result$total)
      
      rv$calculated_income <- income
      
      showNotification(
        paste("Income calculated: ", format_currency(income)),
        type = "default", duration = 5
      )
      
    }, error = function(e) {
      showNotification(paste("Error calculating income:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # Display total income
  output$total_income <- renderText({
    req(user$authenticated)
    format_currency(rv$calculated_income)
  })
  
  # Occupancy rate
  output$occupancy_rate <- renderText({
    req(user$authenticated)
    
    tryCatch({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query <- "
        SELECT COUNT(DISTINCT room_number) as occupied 
        FROM reservations 
        WHERE ? >= check_in AND ? <= check_out
        AND status IN ('Reserved', 'Checked-In')
      "
      
      occupied <- dbGetQuery(conn, query, params = list(
        as.character(Sys.Date()),
        as.character(Sys.Date())
      ))
      
      total_rooms <- length(rv$all_rooms)
      rate <- if (total_rooms > 0) round((occupied$occupied / total_rooms) * 100, 1) else 0
      
      paste0(rate, "%")
    }, error = function(e) {
      "0%"
    })
  })
  
  # ========== RESERVATIONS TAB ==========
  
  # Validation message display
  output$validation_message <- renderUI({
    req(user$authenticated)
    
    if (!is.null(rv$validation_msg)) {
      div(class = paste("alert-custom", rv$validation_msg$type),
          icon("exclamation-circle"),
          rv$validation_msg$text)
    }
  })
  
  # Calculate total bill
  output$total_bill <- renderText({
    req(user$authenticated, input$check_in, input$check_out, input$room_type)
    
    tryCatch({
      check_in_date <- as.Date(input$check_in)
      check_out_date <- as.Date(input$check_out)
      
      if (check_out_date <= check_in_date) {
        return(format_currency(0))
      }
      
      nights <- as.numeric(difftime(check_out_date, check_in_date, units = "days"))
      if (is.na(nights) || nights <= 0) return(format_currency(0))
      
      rate <- get_room_rate(input$room_type)
      bill <- nights * rate
      
      if (nights >= 7) {
        bill <- bill * 0.9
      }
      
      return(format_currency(bill))
    }, error = function(e) {
      return(format_currency(0))
    })
  })
  
  # Room availability status
  output$room_status <- renderUI({
    req(user$authenticated, input$room_number, input$check_in, input$check_out, input$room_type)
    
    tryCatch({
      check_in_date <- as.Date(input$check_in)
      check_out_date <- as.Date(input$check_out)
      
      if (check_out_date <= check_in_date) {
        return(p(icon("times-circle"), "Invalid dates", 
                 style = "color: #ff6b6b; margin-top: 5px;"))
      }
      
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query <- "
        SELECT guest_name, check_in, check_out 
        FROM reservations 
        WHERE room_number = ? 
          AND status IN ('Reserved', 'Checked-In')
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
  
  # Room type information display
  output$room_type_info <- renderUI({
    req(user$authenticated, input$room_number, input$room_type)
    
    if (input$room_number == "No available rooms of this type") {
      return(NULL)
    }
    
    tryCatch({
      room_num <- as.numeric(input$room_number)
      floor_num <- floor(room_num / 100)
      
      room_type_mapping <- list(
        "1" = "Standard",
        "2" = "Deluxe",
        "3" = "Suite", 
        "4" = "Presidential"
      )
      
      actual_type <- room_type_mapping[[as.character(floor_num)]]
      
      if (!is.null(actual_type)) {
        if (actual_type == input$room_type) {
          div(style = "background-color: rgba(79, 195, 247, 0.1); border: 1px solid #4fc3f7; border-radius: 6px; padding: 8px 12px; margin-top: 10px; font-size: 13px; color: #4fc3f7;",
              icon("door-closed"),
              paste("This is a", actual_type, "room"),
              br(),
              small(paste("Floor:", floor_num, "| Rooms:", 
                          paste(get_rooms_by_type(actual_type)[1], "-", 
                                tail(get_rooms_by_type(actual_type), 1))))
          )
        } else {
          div(style = "background-color: rgba(255, 152, 0, 0.1); border: 1px solid #ff9800; border-radius: 6px; padding: 8px 12px; margin-top: 10px; font-size: 12px; color: #ff9800;",
              icon("exclamation-triangle"),
              paste("Warning: Selected room is actually", actual_type, "type"),
              br(),
              small("Consider changing room type or selecting a different room")
          )
        }
      }
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Get available rooms - FILTERED BY SELECTED ROOM TYPE
  observe({
    req(user$authenticated, input$check_in, input$check_out, input$room_type)
    
    rv$last_db_update
    
    tryCatch({
      check_in_date <- as.Date(input$check_in)
      check_out_date <- as.Date(input$check_out)
      
      if (check_out_date <= check_in_date) {
        # Get all rooms of selected type
        rooms_of_selected_type <- get_rooms_by_type(input$room_type)
        updateSelectInput(session, "room_number", 
                          choices = rooms_of_selected_type)
        return()
      }
      
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # First, get all occupied rooms during the selected dates
      query <- "
        SELECT DISTINCT room_number 
        FROM reservations 
        WHERE status IN ('Reserved', 'Checked-In')
          AND ((? >= check_in AND ? < check_out) 
           OR (? > check_in AND ? <= check_out)
           OR (? <= check_in AND ? >= check_out))
      "
      
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
      
      occupied_rooms <- as.character(reservations$room_number)
      
      # Get all rooms of the SELECTED TYPE
      all_rooms_of_type <- get_rooms_by_type(input$room_type)
      
      # Filter out occupied rooms
      available_rooms <- setdiff(all_rooms_of_type, occupied_rooms)
      
      if (length(available_rooms) == 0) {
        updateSelectInput(session, "room_number", 
                          choices = c("No available rooms of this type"),
                          selected = "No available rooms of this type")
        
        # Show warning message
        showNotification(
          paste("No", input$room_type, "rooms available for selected dates"),
          type = "warning", duration = 3
        )
      } else {
        available_rooms <- sort(as.numeric(available_rooms))
        
        # Update room number selection
        current_selection <- input$room_number
        if (is.null(current_selection) || 
            !(current_selection %in% as.character(available_rooms)) ||
            !(current_selection %in% all_rooms_of_type)) {
          # Select first available room of this type
          new_selection <- as.character(available_rooms[1])
        } else {
          new_selection <- current_selection
        }
        
        updateSelectInput(session, "room_number", 
                          choices = as.character(available_rooms),
                          selected = new_selection)
        
        # Auto-update rate display
        rate <- get_room_rate(input$room_type)
        nights <- as.numeric(difftime(check_out_date, check_in_date, units = "days"))
        if (nights > 0) {
          bill <- nights * rate
          if (nights >= 7) bill <- bill * 0.9
          output$total_bill <- renderText({
            format_currency(bill)
          })
        }
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating rooms:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # Observer for room type changes
  observeEvent(input$room_type, {
    req(user$authenticated, input$room_type)
    
    # Force refresh of room availability
    rv$refresh_counter <- rv$refresh_counter + 1
    
    # Show message about room type change
    showNotification(
      paste("Now showing only", input$room_type, "rooms"),
      type = "default", duration = 2
    )
  })
  
  # Status selector (only show when updating)
  output$status_selector <- renderUI({
    req(user$authenticated)
    
    if (!is.null(rv$selected_id)) {
      selectInput("status", "Reservation Status",
                  choices = c("Reserved" = "Reserved",
                              "Checked-In" = "Checked-In",
                              "Checked-Out" = "Checked-Out",
                              "Cancelled" = "Cancelled"),
                  selected = "Reserved",
                  width = "100%")
    }
  })
  
  # Current ID display
  output$current_id <- renderUI({
    req(user$authenticated)
    
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
  
  # Reservations table
  output$reservations_table <- renderDT({
    req(user$authenticated)
    
    data <- reservations_data()
    
    if (nrow(data) > 0) {
      data_display <- data %>%
        mutate(
          check_in = format(as.Date(check_in), "%Y-%m-%d"),
          check_out = format(as.Date(check_out), "%Y-%m-%d"),
          
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
          
          total_bill_formatted = format_currency(total_bill),
          
          created_at = format(as.POSIXct(created_at), "%Y-%m-%d %H:%M"),
          updated_at = format(as.POSIXct(updated_at), "%Y-%m-%d %H:%M")
        ) %>%
        select(
          ID = id,
          Room = room_number,
          Type = room_type,
          Guest = guest_name,
          Email = email,
          Phone = phone,
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
        Message = "No reservations found"
      )
    }
    
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
    req(user$authenticated)
    
    clear_validation()
    
    data <- reservations_data()
    if (nrow(data) > 0 && length(input$reservations_table_rows_selected) > 0) {
      row <- data[input$reservations_table_rows_selected, ]
      
      rv$selected_id <- row$id
      
      updateSelectInput(session, "room_number", selected = as.character(row$room_number))
      updateSelectInput(session, "room_type", selected = row$room_type)
      updateTextInput(session, "guest_name", value = row$guest_name)
      updateTextInput(session, "guest_email", value = row$email %||% "")
      updateTextInput(session, "guest_phone", value = row$phone %||% "")
      updateNumericInput(session, "guests", value = row$guests)
      updateDateInput(session, "check_in", value = as.Date(row$check_in))
      updateDateInput(session, "check_out", value = as.Date(row$check_out))
      updateSelectInput(session, "payment_status", selected = row$payment_status)
      updateTextAreaInput(session, "notes", value = row$notes %||% "")
      
    }
  })
  
  # ========== BUTTON HANDLERS ==========
  
  # Add reservation
  observeEvent(input$add_btn, {
    req(user$authenticated)
    
    clear_validation()
    
    # Validate room type selection
    if (input$room_number == "No available rooms of this type") {
      set_validation(paste("No", input$room_type, "rooms available for selected dates."))
      return()
    }
    
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
    
    nights <- as.numeric(difftime(input$check_out, input$check_in, units = "days"))
    if (nights <= 0) {
      set_validation("Invalid date range.")
      return()
    }
    
    rate <- get_room_rate(input$room_type)
    bill <- nights * rate
    
    if (nights >= 7) {
      bill <- bill * 0.9
    }
    
    # Check if selected room matches selected type
    room_num <- as.numeric(input$room_number)
    floor_num <- floor(room_num / 100)
    room_type_mapping <- list(
      "1" = "Standard",
      "2" = "Deluxe",
      "3" = "Suite",
      "4" = "Presidential"
    )
    actual_type <- room_type_mapping[[as.character(floor_num)]]
    
    # If room type doesn't match, ask for confirmation
    if (!is.null(actual_type) && actual_type != input$room_type) {
      showModal(
        modalDialog(
          title = tags$div(icon("exclamation-triangle"), " Room Type Mismatch"),
          size = "s",
          tags$div(
            style = "padding: 15px; background-color: #2d2d2d; border-radius: 6px;",
            tags$p("You have selected:"),
            tags$div(style = "padding: 10px; background-color: #1e1e1e; border-radius: 4px; margin: 10px 0;",
                     tags$p(style = "margin: 5px 0;",
                            tags$strong("Room Type: "), input$room_type),
                     tags$p(style = "margin: 5px 0;",
                            tags$strong("Room Number: "), input$room_number, 
                            " (actually ", actual_type, ")")),
            tags$p(style = "color: #ff9800;",
                   "Do you want to proceed with this mismatch?")
          ),
          footer = tagList(
            modalButton("Cancel", icon = icon("times")),
            actionButton("confirm_mismatch_add", "Proceed Anyway", 
                         icon = icon("check"),
                         class = "btn-warning")
          ),
          easyClose = FALSE
        )
      )
      return()
    }
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    query <- "
      SELECT COUNT(*) as count FROM reservations 
      WHERE room_number = ? 
        AND status IN ('Reserved', 'Checked-In')
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
    
    query <- "
      INSERT INTO reservations 
      (room_number, room_type, guest_name, email, phone, guests, check_in, check_out, 
       payment_status, notes, total_bill, status)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Reserved')
    "
    
    tryCatch({
      dbExecute(conn, query, params = list(
        as.integer(input$room_number),
        input$room_type,
        trimws(input$guest_name),
        ifelse(trimws(input$guest_email) == "", NA, trimws(input$guest_email)),
        ifelse(trimws(input$guest_phone) == "", NA, trimws(input$guest_phone)),
        as.integer(input$guests),
        as.character(input$check_in),
        as.character(input$check_out),
        input$payment_status,
        ifelse(trimws(input$notes) == "", NA, trimws(input$notes)),
        bill
      ))
      
      rv$last_db_update <- Sys.time()
      
      showNotification(
        paste("Reservation added successfully for", input$guest_name, 
              "in Room", input$room_number),
        type = "default", duration = 3
      )
      
      updateTextInput(session, "guest_name", value = "")
      updateTextInput(session, "guest_email", value = "")
      updateTextInput(session, "guest_phone", value = "")
      updateTextAreaInput(session, "notes", value = "")
      
      rv$refresh_trigger <- rv$refresh_trigger + 1
      rv$refresh_counter <- rv$refresh_counter + 1
      
    }, error = function(e) {
      showNotification(paste("Error adding reservation:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # Handle room type mismatch confirmation for add
  observeEvent(input$confirm_mismatch_add, {
    removeModal()
    
    # Continue with the add reservation logic
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    nights <- as.numeric(difftime(input$check_out, input$check_in, units = "days"))
    rate <- get_room_rate(input$room_type)
    bill <- nights * rate
    
    if (nights >= 7) {
      bill <- bill * 0.9
    }
    
    # Check for conflicts
    query <- "
      SELECT COUNT(*) as count FROM reservations 
      WHERE room_number = ? 
        AND status IN ('Reserved', 'Checked-In')
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
    
    # Insert reservation
    query <- "
      INSERT INTO reservations 
      (room_number, room_type, guest_name, email, phone, guests, check_in, check_out, 
       payment_status, notes, total_bill, status)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Reserved')
    "
    
    tryCatch({
      dbExecute(conn, query, params = list(
        as.integer(input$room_number),
        input$room_type,
        trimws(input$guest_name),
        ifelse(trimws(input$guest_email) == "", NA, trimws(input$guest_email)),
        ifelse(trimws(input$guest_phone) == "", NA, trimws(input$guest_phone)),
        as.integer(input$guests),
        as.character(input$check_in),
        as.character(input$check_out),
        input$payment_status,
        ifelse(trimws(input$notes) == "", NA, trimws(input$notes)),
        bill
      ))
      
      rv$last_db_update <- Sys.time()
      
      showNotification(
        paste("Reservation added successfully for", input$guest_name, 
              "in Room", input$room_number, "(Note: Room type mismatch)"),
        type = "default", duration = 3
      )
      
      updateTextInput(session, "guest_name", value = "")
      updateTextInput(session, "guest_email", value = "")
      updateTextInput(session, "guest_phone", value = "")
      updateTextAreaInput(session, "notes", value = "")
      
      rv$refresh_trigger <- rv$refresh_trigger + 1
      rv$refresh_counter <- rv$refresh_counter + 1
      
    }, error = function(e) {
      showNotification(paste("Error adding reservation:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # Update reservation
  observeEvent(input$update_btn, {
    req(user$authenticated)
    
    clear_validation()
    
    if (is.null(rv$selected_id)) {
      set_validation("Please select a reservation to update.", "alert-warning")
      return()
    }
    
    if (is.null(input$guest_name) || trimws(input$guest_name) == "") {
      set_validation("Guest name is required.")
      return()
    }
    
    if (input$check_out <= input$check_in) {
      set_validation("Check-out date must be after check-in date.")
      return()
    }
    
    nights <- as.numeric(difftime(input$check_out, input$check_in, units = "days"))
    if (nights <= 0) {
      set_validation("Invalid date range.")
      return()
    }
    
    rate <- get_room_rate(input$room_type)
    bill <- nights * rate
    
    if (nights >= 7) {
      bill <- bill * 0.9
    }
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    query <- "
      SELECT COUNT(*) as count FROM reservations 
      WHERE room_number = ? 
        AND status IN ('Reserved', 'Checked-In')
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
    
    query <- "
      UPDATE reservations SET
      room_number = ?, room_type = ?, guest_name = ?, email = ?, phone = ?, guests = ?,
      check_in = ?, check_out = ?, payment_status = ?, notes = ?,
      total_bill = ?, status = ?
      WHERE id = ?
    "
    
    tryCatch({
      dbExecute(conn, query, params = list(
        as.integer(input$room_number),
        input$room_type,
        trimws(input$guest_name),
        ifelse(trimws(input$guest_email) == "", NA, trimws(input$guest_email)),
        ifelse(trimws(input$guest_phone) == "", NA, trimws(input$guest_phone)),
        as.integer(input$guests),
        as.character(input$check_in),
        as.character(input$check_out),
        input$payment_status,
        ifelse(trimws(input$notes) == "", NA, trimws(input$notes)),
        bill,
        ifelse(!is.null(input$status), input$status, "Reserved"),
        rv$selected_id
      ))
      
      rv$last_db_update <- Sys.time()
      
      showNotification(
        paste("Reservation updated successfully for", input$guest_name),
        type = "default", duration = 3
      )
      
      rv$selected_id <- NULL
      
      rv$refresh_trigger <- rv$refresh_trigger + 1
      rv$refresh_counter <- rv$refresh_counter + 1
      
    }, error = function(e) {
      showNotification(paste("Error updating reservation:", e$message), 
                       type = "error", duration = 5)
    })
  })
  
  # Delete reservation
  observeEvent(input$delete_btn, {
    req(user$authenticated)
    
    clear_validation()
    
    if (is.null(rv$selected_id)) {
      set_validation("Please select a reservation to delete.", "alert-warning")
      return()
    }
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    query <- "SELECT room_number, guest_name FROM reservations WHERE id = ?"
    res_details <- dbGetQuery(conn, query, params = list(rv$selected_id))
    
    if (nrow(res_details) == 0) {
      set_validation("Reservation not found.")
      return()
    }
    
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
    req(user$authenticated)
    
    tryCatch({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      query_select <- "SELECT guest_name, room_number FROM reservations WHERE id = ?"
      res_info <- dbGetQuery(conn, query_select, params = list(rv$selected_id))
      
      query_delete <- "DELETE FROM reservations WHERE id = ?"
      dbExecute(conn, query_delete, params = list(rv$selected_id))
      
      rv$last_db_update <- Sys.time()
      
      showNotification(
        paste("Reservation for", res_info$guest_name, 
              "(Room", res_info$room_number, ") deleted successfully."),
        type = "default", duration = 3
      )
      
      rv$selected_id <- NULL
      updateTextInput(session, "guest_name", value = "")
      updateTextInput(session, "guest_email", value = "")
      updateTextInput(session, "guest_phone", value = "")
      updateTextAreaInput(session, "notes", value = "")
      
      rv$refresh_trigger <- rv$refresh_trigger + 1
      rv$refresh_counter <- rv$refresh_counter + 1
      
    }, error = function(e) {
      showNotification(paste("Error deleting reservation:", e$message), 
                       type = "error", duration = 5)
    })
    
    removeModal()
  })
  
  # Clear fields
  observeEvent(input$clear_btn, {
    req(user$authenticated)
    
    tryCatch({
      clear_validation()
      
      updateTextInput(session, "guest_name", value = "")
      updateTextInput(session, "guest_email", value = "")
      updateTextInput(session, "guest_phone", value = "")
      updateNumericInput(session, "guests", value = 1)
      updateTextAreaInput(session, "notes", value = "")
      
      rv$selected_id <- NULL
      
      dataTableProxy("reservations_table") %>%
        selectRows(NULL)
      
      showNotification("Fields cleared.", type = "default", duration = 2)
    }, error = function(e) {
      print(paste("Clear error:", e$message))
    })
  })
  
  # Search button handler
  observeEvent(input$search_btn, {
    req(user$authenticated)
    
    tryCatch({
      showNotification("Search applied.", type = "default", duration = 2)
      
      rv$refresh_counter <- rv$refresh_counter + 1
    }, error = function(e) {
      print(paste("Search error:", e$message))
    })
  })
  
  # Reset button handler
  observeEvent(input$reset_btn, {
    req(user$authenticated)
    
    tryCatch({
      updateTextInput(session, "search_input", value = "")
      updateSelectInput(session, "filter_status", selected = "All")
      updateSelectInput(session, "filter_payment", selected = "All")
      
      showNotification("Filters reset.", type = "default", duration = 2)
      
      rv$refresh_counter <- rv$refresh_counter + 1
    }, error = function(e) {
      print(paste("Reset error:", e$message))
    })
  })
  
  # Export CSV
  output$export_csv <- downloadHandler(
    filename = function() {
      paste("hotel_reservations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(user$authenticated)
      
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
      req(user$authenticated)
      
      data <- reservations_data()
      if (nrow(data) > 0) {
        write.xlsx(data, file)
      }
    }
  )
  
  # ========== ROOMS TAB ==========
  
  # Rooms display
  output$rooms_display <- renderUI({
    req(user$authenticated)
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    check_in <- input$room_check_in %||% Sys.Date()
    check_out <- input$room_check_out %||% (Sys.Date() + 1)
    
    query <- "
      SELECT room_number, status, guest_name, check_in, check_out 
      FROM reservations 
      WHERE status IN ('Reserved', 'Checked-In')
        AND ((? >= check_in AND ? < check_out) 
         OR (? > check_in AND ? <= check_out)
         OR (? <= check_in AND ? >= check_out))
      ORDER BY room_number
    "
    
    occupied <- dbGetQuery(conn, query, params = list(
      as.character(check_in), as.character(check_in),
      as.character(check_out), as.character(check_out),
      as.character(check_in), as.character(check_out)
    ))
    
    occupied_rooms <- occupied$room_number
    
    # Create room cards
    room_cards <- lapply(rv$all_rooms, function(room) {
      room_num <- as.numeric(room)
      is_occupied <- room_num %in% occupied_rooms
      
      # Determine room type based on floor
      floor_num <- floor(room_num / 100)
      room_type <- switch(as.character(floor_num),
                          "1" = "Standard",
                          "2" = "Deluxe",
                          "3" = "Suite",
                          "4" = "Presidential",
                          "Standard")
      
      # Get room color class
      room_color_class <- paste0("room-type-", tolower(room_type))
      
      if (is_occupied) {
        room_info <- occupied[occupied$room_number == room_num, ]
        room_class <- "room-occupied"
        room_status <- if (room_info$status[1] == "Checked-In") "Checked-In" else "Reserved"
        guest_name <- room_info$guest_name[1]
      } else {
        room_class <- "room-available"
        room_status <- "Available"
        guest_name <- ""
      }
      
      # Room card with onclick handler
      div(
        class = paste("room-card", room_class, room_color_class),
        onclick = paste0("Shiny.setInputValue('room_clicked', '", room, "', {priority: 'event'});"),
        div(class = "room-number", paste("Room", room)),
        div(class = paste("room-status", "room-badge", tolower(room_type)), room_status),
        div(class = "room-type", room_type),
        if (guest_name != "") div(class = "guest-name", paste("Guest:", guest_name))
      )
    })
    
    # Arrange in rows of 4
    rows <- split(room_cards, ceiling(seq_along(room_cards)/4))
    
    tagList(
      lapply(rows, function(row) {
        fluidRow(
          lapply(row, function(card) {
            column(3, card)
          })
        )
      })
    )
  })
  
  # Handle room click
  observeEvent(input$room_clicked, {
    req(user$authenticated, input$room_clicked)
    
    tryCatch({
      rv$selected_room <- as.numeric(input$room_clicked)
      
      # Get current reservations for this room
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # Get current reservation if any
      current_reservation <- dbGetQuery(conn, "
        SELECT guest_name, check_in, check_out, status 
        FROM reservations 
        WHERE room_number = ? 
          AND status IN ('Reserved', 'Checked-In')
          AND ? BETWEEN check_in AND check_out
        LIMIT 1
      ", params = list(rv$selected_room, as.character(Sys.Date())))
      
      # Get room image
      room_image <- get_room_image(rv$selected_room)
      rv$current_room_image <- room_image
      
      # Determine room type based on floor
      floor_num <- floor(rv$selected_room / 100)
      room_type <- switch(as.character(floor_num),
                          "1" = "Standard",
                          "2" = "Deluxe",
                          "3" = "Suite",
                          "4" = "Presidential",
                          "Standard")
      
      # Get room rate
      room_rate <- get_room_rate(room_type)
      
      # Get amenities
      amenities <- get_room_amenities(room_type)
      
      # Show modal with enhanced room details
      showModal(
        modalDialog(
          title = tags$div(
            icon("door-closed"),
            paste("Room", rv$selected_room, "Details"),
            style = "display: flex; align-items: center; gap: 10px;"
          ),
          size = "l",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close", icon = icon("times")),
            actionButton("save_room_image", "Save Image", 
                         icon = icon("save"),
                         class = "btn-primary")
          ),
          
          # ===== ENHANCED ROOM IMAGE SECTION =====
          div(class = "room-image-section",
              h4(icon("camera"), "Room Image", 
                 style = "color: #4fc3f7; margin-bottom: 15px;"),
              
              # Current image display - LARGER
              div(class = "room-image-display",
                  if (!is.null(room_image) && !is.null(room_image$data)) {
                    div(class = "current-image-wrapper",
                        tags$img(
                          src = paste0("data:image/", room_image$type, ";base64,", room_image$data),
                          class = "room-image-large",
                          alt = paste("Room", rv$selected_room)
                        ),
                        actionButton("remove_image_btn", "", 
                                     icon = icon("trash"),
                                     class = "remove-image-btn-enhanced",
                                     onclick = "Shiny.setInputValue('remove_image_clicked', true, {priority: 'event'});")
                    )
                  } else {
                    div(class = "no-image-placeholder",
                        icon("camera", style = "font-size: 64px; margin-bottom: 15px;"),
                        p("No image uploaded for this room"),
                        p(style = "font-size: 12px; margin-top: 10px; color: #666;", 
                          "Upload an image to enhance the room display")
                    )
                  }
              ),
              
              # Image upload section - SMALLER AND COMPACT
              div(class = "image-upload-compact",
                  h5("Upload New Room Image"),
                  fileInput("room_image_upload", NULL,
                            accept = c("image/png", "image/jpeg", "image/jpg", "image/gif"),
                            buttonLabel = tags$span(icon("upload"), "Choose File"),
                            placeholder = "No file selected",
                            width = "100%"),
                  div(class = "file-input-info",
                      "Supported: PNG, JPG, GIF | Max: 5MB")
              )
          ),
          
          hr(style = "border-color: #333; margin: 25px 0;"),
          
          # ===== ENHANCED ROOM INFORMATION SECTION =====
          div(class = "room-details-enhanced",
              h4(icon("info-circle"), "Room Information", 
                 style = "color: #4fc3f7; margin-bottom: 15px;"),
              
              div(class = "room-details-grid-enhanced",
                  div(class = "room-detail-item-enhanced",
                      div(class = "room-detail-label-enhanced", "Room Number"),
                      div(class = "room-detail-value-enhanced", 
                          tags$span(style = "font-size: 24px; font-weight: bold;", rv$selected_room))
                  ),
                  
                  div(class = "room-detail-item-enhanced",
                      div(class = "room-detail-label-enhanced", "Room Type"),
                      div(class = "room-detail-value-enhanced",
                          span(class = paste("room-type-badge", paste0("room-type-", tolower(room_type))),
                               room_type)
                      )
                  ),
                  
                  div(class = "room-detail-item-enhanced",
                      div(class = "room-detail-label-enhanced", "Daily Rate"),
                      div(class = "room-detail-value-enhanced", 
                          tags$span(style = "color: #4caf50; font-weight: bold;", format_currency(room_rate)))
                  ),
                  
                  div(class = "room-detail-item-enhanced",
                      div(class = "room-detail-label-enhanced", "Floor"),
                      div(class = "room-detail-value-enhanced", 
                          tags$span(style = "font-size: 18px;", floor_num))
                  ),
                  
                  div(class = "room-detail-item-enhanced",
                      div(class = "room-detail-label-enhanced", "Max Guests"),
                      div(class = "room-detail-value-enhanced",
                          tags$span(icon("users"), 
                                    switch(room_type,
                                           "Standard" = "2 Adults",
                                           "Deluxe" = "3 Adults",
                                           "Suite" = "4 Adults",
                                           "Presidential" = "6 Adults",
                                           "2 Adults")))
                  ),
                  
                  div(class = "room-detail-item-enhanced",
                      div(class = "room-detail-label-enhanced", "Size"),
                      div(class = "room-detail-value-enhanced",
                          tags$span(switch(room_type,
                                           "Standard" = "25 m²",
                                           "Deluxe" = "35 m²",
                                           "Suite" = "50 m²",
                                           "Presidential" = "80 m²",
                                           "25 m²")))
                  )
              )
          ),
          
          # ===== ENHANCED AMENITIES SECTION (IMPROVED VISIBILITY) =====
          div(class = "amenities-section",
              h4(icon("star"), "Room Amenities"),
              
              div(class = "amenities-grid",
                  lapply(amenities, function(amenity) {
                    div(class = "amenity-item",
                        icon("check-circle"),
                        span(class = "amenity-text", amenity)
                    )
                  })
              )
          ),
          
          # ===== CURRENT RESERVATION SECTION =====
          if (nrow(current_reservation) > 0) {
            div(class = "current-reservation-section",
                h4(icon("calendar-check"), "Current Reservation"),
                
                div(class = "reservation-details",
                    div(class = "room-detail-item-enhanced",
                        div(class = "room-detail-label-enhanced", "Guest Name"),
                        div(class = "room-detail-value-enhanced", current_reservation$guest_name)
                    ),
                    
                    div(class = "room-detail-item-enhanced",
                        div(class = "room-detail-label-enhanced", "Status"),
                        div(class = "room-detail-value-enhanced",
                            span(class = paste0("status-badge status-", tolower(current_reservation$status)),
                                 current_reservation$status))
                    ),
                    
                    div(class = "room-detail-item-enhanced",
                        div(class = "room-detail-label-enhanced", "Check-In Date"),
                        div(class = "room-detail-value-enhanced",
                            icon("sign-in-alt"),
                            format(as.Date(current_reservation$check_in), "%B %d, %Y"))
                    ),
                    
                    div(class = "room-detail-item-enhanced",
                        div(class = "room-detail-label-enhanced", "Check-Out Date"),
                        div(class = "room-detail-value-enhanced",
                            icon("sign-out-alt"),
                            format(as.Date(current_reservation$check_out), "%B %d, %Y"))
                    ),
                    
                    div(class = "room-detail-item-enhanced",
                        div(class = "room-detail-label-enhanced", "Stay Duration"),
                        div(class = "room-detail-value-enhanced",
                            icon("moon"),
                            as.numeric(difftime(current_reservation$check_out, 
                                                current_reservation$check_in, 
                                                units = "days")),
                            " nights")
                    )
                )
            )
          } else {
            div(class = "amenities-section",
                style = "background-color: rgba(76, 175, 80, 0.1); border-color: #4CAF50;",
                h4(icon("check-circle"), "Availability", style = "color: #4CAF50;"),
                p(style = "color: #ffffff; font-size: 15px; margin: 0;",
                  icon("smile"),
                  "This room is available for the selected dates!")
            )
          }
        )
      )
      
    }, error = function(e) {
      showNotification(paste("Error loading room details:", e$message), type = "error", duration = 5)
    })
  })
  
  # Refresh rooms button
  observeEvent(input$refresh_rooms, {
    showNotification("Room availability refreshed.", type = "default", duration = 2)
  })
  
  # ========== REPORTS TAB ==========
  
  # Financial summary
  output$financial_summary <- renderText({
    req(user$authenticated)
    
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    tryCatch({
      # Get data for selected period
      start_date <- input$report_start_date %||% (Sys.Date() - 365)
      end_date <- input$report_end_date %||% Sys.Date()
      
      # Get total paid income for period
      total_paid <- dbGetQuery(conn, "
        SELECT SUM(total_bill) as total 
        FROM reservations 
        WHERE payment_status = 'Paid'
          AND check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))
      
      total_paid <- ifelse(is.na(total_paid$total), 0, total_paid$total)
      
      # Get unpaid amount for period
      total_unpaid <- dbGetQuery(conn, "
        SELECT SUM(total_bill) as total 
        FROM reservations 
        WHERE payment_status = 'Unpaid'
          AND check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))
      
      total_unpaid <- ifelse(is.na(total_unpaid$total), 0, total_unpaid$total)
      
      # Get pending amount for period
      total_pending <- dbGetQuery(conn, "
        SELECT SUM(total_bill) as total 
        FROM reservations 
        WHERE payment_status = 'Pending'
          AND check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))
      
      total_pending <- ifelse(is.na(total_pending$total), 0, total_pending$total)
      
      # Get total bookings for period
      total_bookings <- dbGetQuery(conn, "
        SELECT COUNT(*) as total 
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))
      
      total_bookings <- ifelse(is.na(total_bookings$total), 0, total_bookings$total)
      
      # Get average daily rate
      avg_daily_rate <- dbGetQuery(conn, "
        SELECT AVG(total_bill / (julianday(check_out) - julianday(check_in))) as avg_rate
        FROM reservations 
        WHERE payment_status = 'Paid'
          AND check_out >= ? AND check_in <= ?
          AND julianday(check_out) > julianday(check_in)
      ", params = list(as.character(start_date), as.character(end_date)))
      
      avg_daily_rate <- ifelse(is.na(avg_daily_rate$avg_rate), 0, avg_daily_rate$avg_rate)
      
      paste(
        "FINANCIAL SUMMARY (", format(start_date, "%b %d, %Y"), " - ", format(end_date, "%b %d, %Y"), ")",
        "\n==========================================",
        "\nTotal Paid Income: ", format_currency(total_paid),
        "\nTotal Unpaid: ", format_currency(total_unpaid),
        "\nTotal Pending: ", format_currency(total_pending),
        "\nTotal Bookings: ", total_bookings,
        "\nAverage Daily Rate: ", format_currency(avg_daily_rate),
        "\n==========================================",
        "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      
    }, error = function(e) {
      paste("Error generating financial summary:", e$message)
    })
  })
  
  # Generate report button handler - FIXED VERSION
  # Generate report button handler - FIXED VERSION
  observeEvent(input$generate_report, {
    req(user$authenticated)
    
    tryCatch({
      # Update the report generated flag
      rv$report_generated <- TRUE
      
      # Force refresh of all charts
      rv$refresh_counter <- rv$refresh_counter + 1
      
      # Show notification with selected date range
      start_date <- input$report_start_date %||% (Sys.Date() - 365)
      end_date <- input$report_end_date %||% Sys.Date()
      
      showNotification(
        paste("Report generated for period:", 
              format(start_date, "%b %d, %Y"), "to", 
              format(end_date, "%b %d, %Y")), 
        type = "default", duration = 3
      )
      
    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message), type = "error", duration = 5)
    })
  })
  
  # Revenue by Room Type - Bar Chart
  output$revenue_by_room_type <- renderPlot({
    req(user$authenticated)
    
    # Only show plot if report has been generated or if data is available
    if (!rv$report_generated) {
      plot.new()
      text(0.5, 0.5, "Click 'Generate Report' to view charts", 
           cex = 1.2, col = "gray")
      return()
    }
    
    safe_plot({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # Get data for selected period
      start_date <- input$report_start_date %||% (Sys.Date() - 365)
      end_date <- input$report_end_date %||% Sys.Date()
      
      query <- "
        SELECT room_type, SUM(total_bill) as revenue, COUNT(*) as bookings
        FROM reservations 
        WHERE payment_status = 'Paid'
          AND check_out >= ? AND check_in <= ?
        GROUP BY room_type
        ORDER BY revenue DESC
      "
      
      data <- dbGetQuery(conn, query, params = list(
        as.character(start_date),
        as.character(end_date)
      ))
      
      if (nrow(data) > 0 && sum(data$revenue, na.rm = TRUE) > 0) {
        # Define colors for each room type
        room_colors <- c(
          "Standard" = "#4CAF50",
          "Deluxe" = "#2196F3", 
          "Suite" = "#9C27B0",
          "Presidential" = "#FF9800"
        )
        
        # Create bar chart
        par(mar = c(5, 4, 4, 8), xpd = TRUE)
        
        # Use actual room types from data
        colors <- room_colors[data$room_type]
        colors[is.na(colors)] <- "#CCCCCC"  # Default color for unknown types
        
        bar_data <- barplot(data$revenue, 
                            names.arg = data$room_type,
                            col = colors,
                            main = paste("Revenue by Room Type\n", 
                                         format(start_date, "%b %d, %Y"), "to", 
                                         format(end_date, "%b %d, %Y")),
                            xlab = "Room Type",
                            ylab = "Revenue (₱)",
                            las = 2,
                            ylim = c(0, max(data$revenue) * 1.2),
                            cex.names = 0.8)
        
        # Add revenue values on top of bars
        text(x = bar_data, 
             y = data$revenue, 
             labels = format_currency(data$revenue),
             pos = 3, 
             cex = 0.8, 
             col = "black")
        
        # Add legend
        legend("topright", 
               inset = c(-0.2, 0),
               legend = paste(data$room_type, "-", format_currency(data$revenue)),
               fill = colors,
               title = "Room Types",
               cex = 0.8,
               bg = "white")
        
      } else {
        plot.new()
        text(0.5, 0.5, "No revenue data available\nfor selected period", 
             cex = 1.2, col = "gray")
      }
    })
  })
  
  # Monthly Revenue Trends - Line Chart
  # Monthly Revenue Trends - Line Chart - FIXED VERSION
  output$monthly_revenue_trends <- renderPlot({
    req(user$authenticated)
    
    # Only show plot if report has been generated or if data is available
    if (!rv$report_generated) {
      plot.new()
      text(0.5, 0.5, "Click 'Generate Report' to view charts", 
           cex = 1.2, col = "gray")
      return()
    }
    
    safe_plot({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # Get data for selected period
      start_date <- input$report_start_date %||% (Sys.Date() - 365)
      end_date <- input$report_end_date %||% Sys.Date()
      
      # First, generate all months in the range
      all_months <- seq(from = as.Date(format(start_date, "%Y-%m-01")), 
                        to = as.Date(format(end_date, "%Y-%m-01")), 
                        by = "month")
      
      # Format months for display
      month_labels <- format(all_months, "%Y-%m")
      
      # Get revenue data
      query <- "
      SELECT 
        strftime('%Y-%m', check_in) as month,
        SUM(total_bill) as revenue,
        COUNT(*) as bookings
      FROM reservations 
      WHERE payment_status = 'Paid'
        AND check_in >= ? 
        AND check_in <= ?
      GROUP BY month
      ORDER BY month
    "
      
      data <- dbGetQuery(conn, query, params = list(
        as.character(start_date),
        as.character(end_date)
      ))
      
      if (nrow(data) > 0) {
        # Merge with all months to ensure we have all months in the range
        all_data <- data.frame(month = month_labels)
        merged_data <- merge(all_data, data, by = "month", all.x = TRUE)
        
        # Replace NA with 0
        merged_data$revenue[is.na(merged_data$revenue)] <- 0
        merged_data$bookings[is.na(merged_data$bookings)] <- 0
        
        # Order by month
        merged_data <- merged_data[order(merged_data$month), ]
        
        # Create line chart
        par(mar = c(5, 4, 4, 4))
        plot(1:nrow(merged_data), merged_data$revenue, 
             type = "o",
             col = "#2196F3",
             lwd = 2,
             main = paste("Monthly Revenue Trends\n", 
                          format(start_date, "%b %d, %Y"), "to", 
                          format(end_date, "%b %d, %Y")),
             xlab = "Month",
             ylab = "Revenue (₱)",
             xaxt = "n",
             ylim = c(0, max(merged_data$revenue, na.rm = TRUE) * 1.1))
        
        # Add month labels (format nicely)
        month_display <- format(as.Date(paste0(merged_data$month, "-01")), "%b %Y")
        axis(1, at = 1:nrow(merged_data), labels = month_display, las = 2, cex.axis = 0.8)
        
        # Add revenue points
        points(1:nrow(merged_data), merged_data$revenue, pch = 19, col = "#2196F3", cex = 1.5)
        
        # Add grid
        grid()
        
        # Add revenue values on the points
        if (nrow(merged_data) <= 12) {  # Only show values if not too many points
          text(1:nrow(merged_data), merged_data$revenue, 
               labels = format_currency(merged_data$revenue),
               pos = 3, cex = 0.7, col = "#2196F3")
        }
        
        # Add trend line if we have at least 2 months with data
        months_with_data <- sum(merged_data$revenue > 0)
        if (months_with_data >= 2) {
          trend_indices <- which(merged_data$revenue > 0)
          trend_values <- merged_data$revenue[merged_data$revenue > 0]
          
          if (length(trend_indices) >= 2) {
            abline(lm(trend_values ~ trend_indices), 
                   col = "#FF9800", lwd = 2, lty = 2)
            legend("topleft", legend = c("Revenue", "Trend"), 
                   col = c("#2196F3", "#FF9800"), lwd = 2, lty = c(1, 2))
          }
        }
        
        # Add total revenue annotation
        total_rev <- sum(merged_data$revenue, na.rm = TRUE)
        mtext(paste("Total:", format_currency(total_rev)), 
              side = 3, col = "#4CAF50", cex = 0.9)
        
      } else {
        plot.new()
        text(0.5, 0.5, "No revenue data available for the selected period", 
             cex = 1.2, col = "gray")
      }
    })
  })
  
  # Payment Status Distribution - Pie Chart
  output$payment_status_dist <- renderPlot({
    req(user$authenticated)
    
    # Only show plot if report has been generated or if data is available
    if (!rv$report_generated) {
      plot.new()
      text(0.5, 0.5, "Click 'Generate Report' to view charts", 
           cex = 1.2, col = "gray")
      return()
    }
    
    safe_plot({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # Get data for selected period
      start_date <- input$report_start_date %||% (Sys.Date() - 365)
      end_date <- input$report_end_date %||% Sys.Date()
      
      query <- "
        SELECT 
          payment_status,
          COUNT(*) as count,
          SUM(total_bill) as amount
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        GROUP BY payment_status
      "
      
      data <- dbGetQuery(conn, query, params = list(
        as.character(start_date),
        as.character(end_date)
      ))
      
      if (nrow(data) > 0 && sum(data$amount, na.rm = TRUE) > 0) {
        # Define colors for payment status
        status_colors <- c(
          "Paid" = "#4CAF50",
          "Unpaid" = "#F44336",
          "Pending" = "#FFC107"
        )
        
        # Filter out zero amounts
        data <- data[data$amount > 0, ]
        
        if (nrow(data) > 0) {
          # Create pie chart
          par(mar = c(5, 4, 4, 8))
          slices <- data$amount
          lbls <- paste(data$payment_status, "\n", 
                        format_currency(data$amount), "\n",
                        "(", data$count, " bookings)")
          pct <- round(slices/sum(slices)*100, 1)
          lbls <- paste(lbls, "\n", pct, "%")
          
          pie(slices, 
              labels = lbls,
              col = status_colors[data$payment_status],
              main = paste("Payment Status Distribution\n", 
                           format(start_date, "%b %d, %Y"), "to", 
                           format(end_date, "%b %d, %Y")),
              cex = 0.8)
          
          # Add legend
          legend("topright", 
                 inset = c(-0.2, 0),
                 legend = paste(data$payment_status, "-", 
                                format_currency(data$amount)),
                 fill = status_colors[data$payment_status],
                 title = "Payment Status",
                 cex = 0.8,
                 bg = "white")
        } else {
          plot.new()
          text(0.5, 0.5, "No payment data available\nfor selected period", 
               cex = 1.2, col = "gray")
        }
        
      } else {
        plot.new()
        text(0.5, 0.5, "No payment data available\nfor selected period", 
             cex = 1.2, col = "gray")
      }
    })
  })
  
  # Additional Chart 1: Bookings by Month
  output$bookings_by_month <- renderPlot({
    req(user$authenticated)
    
    # Only show plot if report has been generated or if data is available
    if (!rv$report_generated) {
      plot.new()
      text(0.5, 0.5, "Click 'Generate Report' to view charts", 
           cex = 1.2, col = "gray")
      return()
    }
    
    safe_plot({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # Get data for selected period
      start_date <- input$report_start_date %||% (Sys.Date() - 365)
      end_date <- input$report_end_date %||% Sys.Date()
      
      query <- "
        SELECT 
          strftime('%Y-%m', check_in) as month,
          COUNT(*) as bookings,
          SUM(guests) as total_guests
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        GROUP BY month
        ORDER BY month
      "
      
      data <- dbGetQuery(conn, query, params = list(
        as.character(start_date),
        as.character(end_date)
      ))
      
      if (nrow(data) > 0 && sum(data$bookings, na.rm = TRUE) > 0) {
        # Create bar chart with dual y-axis
        par(mar = c(5, 4, 4, 4))
        
        # Plot bookings (bars)
        bar_data <- barplot(data$bookings, 
                            names.arg = data$month,
                            col = "#2196F3",
                            main = paste("Bookings & Guests by Month\n", 
                                         format(start_date, "%b %d, %Y"), "to", 
                                         format(end_date, "%b %d, %Y")),
                            xlab = "Month",
                            ylab = "Number of Bookings",
                            las = 2,
                            cex.names = 0.7,
                            ylim = c(0, max(data$bookings) * 1.3))
        
        # Add guests line (second y-axis)
        par(new = TRUE)
        plot(bar_data, data$total_guests, 
             type = "o", 
             col = "#FF9800", 
             lwd = 2,
             axes = FALSE, 
             xlab = "", 
             ylab = "",
             ylim = c(0, max(data$total_guests) * 1.3))
        
        # Add right y-axis for guests
        axis(4, col = "#FF9800", col.axis = "#FF9800")
        mtext("Total Guests", side = 4, line = 3, col = "#FF9800")
        
        # Add legend
        legend("topright", 
               legend = c("Bookings", "Guests"),
               fill = c("#2196F3", "#FF9800"),
               cex = 0.8,
               bg = "white")
        
        # Add total annotations
        mtext(paste("Total Bookings:", sum(data$bookings)), 
              side = 3, col = "#2196F3", cex = 0.8, line = 0.5)
        mtext(paste("Total Guests:", sum(data$total_guests)), 
              side = 3, col = "#FF9800", cex = 0.8, line = -0.5)
        
      } else {
        plot.new()
        text(0.5, 0.5, "No booking data available\nfor selected period", 
             cex = 1.2, col = "gray")
      }
    })
  })
  
  # Additional Chart 2: Room Type Occupancy
  output$room_type_occupancy <- renderPlot({
    req(user$authenticated)
    
    # Only show plot if report has been generated or if data is available
    if (!rv$report_generated) {
      plot.new()
      text(0.5, 0.5, "Click 'Generate Report' to view charts", 
           cex = 1.2, col = "gray")
      return()
    }
    
    safe_plot({
      conn <- db_connect()
      on.exit(db_disconnect(conn))
      
      # Get data for selected period
      start_date <- input$report_start_date %||% (Sys.Date() - 365)
      end_date <- input$report_end_date %||% Sys.Date()
      
      query <- "
        SELECT 
          room_type,
          COUNT(*) as total_bookings,
          SUM(CASE WHEN status IN ('Reserved', 'Checked-In') THEN 1 ELSE 0 END) as active_bookings,
          AVG(guests) as avg_guests,
          AVG(julianday(check_out) - julianday(check_in)) as avg_stay_length
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        GROUP BY room_type
        ORDER BY total_bookings DESC
      "
      
      data <- dbGetQuery(conn, query, params = list(
        as.character(start_date),
        as.character(end_date)
      ))
      
      if (nrow(data) > 0 && sum(data$total_bookings, na.rm = TRUE) > 0) {
        # Calculate occupancy rate
        data$occupancy_rate <- (data$active_bookings / data$total_bookings) * 100
        
        # Create grouped bar chart
        par(mar = c(5, 4, 4, 8))
        
        # Prepare data for grouped bars
        bar_data <- rbind(data$total_bookings, data$active_bookings)
        colnames(bar_data) <- data$room_type
        
        # Create grouped bar plot
        bar_positions <- barplot(bar_data,
                                 beside = TRUE,
                                 col = c("#2196F3", "#4CAF50"),
                                 main = paste("Room Type Performance\n", 
                                              format(start_date, "%b %d, %Y"), "to", 
                                              format(end_date, "%b %d, %Y")),
                                 xlab = "Room Type",
                                 ylab = "Number of Bookings",
                                 las = 1,
                                 ylim = c(0, max(bar_data) * 1.3),
                                 legend.text = c("Total Bookings", "Active Bookings"),
                                 args.legend = list(x = "topright", 
                                                    inset = c(-0.2, 0),
                                                    bg = "white"))
        
        # Add occupancy rate as text
        text(x = colMeans(bar_positions), 
             y = apply(bar_data, 2, max) * 1.1,
             labels = paste(round(data$occupancy_rate, 1), "%"),
             col = "#FF9800",
             font = 2)
        
        # Add average stay length annotation
        avg_stay <- round(mean(data$avg_stay_length, na.rm = TRUE), 1)
        mtext(paste("Avg Stay:", ifelse(is.na(avg_stay), "N/A", paste(avg_stay, "days"))), 
              side = 3, col = "#9C27B0", cex = 0.8)
        
      } else {
        plot.new()
        text(0.5, 0.5, "No occupancy data available\nfor selected period", 
             cex = 1.2, col = "gray")
      }
    })
  })
  
  # Download Summary Report (PDF format) - IMPLEMENTED VERSION
  # Download Summary Report (PDF format) - FIXED VERSION
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("hotel_summary_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(user$authenticated)
      
      tryCatch({
        # Create a PDF report
        conn <- db_connect()
        on.exit(db_disconnect(conn))
        
        # Get data for selected period
        start_date <- input$report_start_date %||% (Sys.Date() - 365)
        end_date <- input$report_end_date %||% Sys.Date()
        
        # Calculate period days correctly
        period_days <- as.numeric(difftime(end_date, start_date, units = "days"))
        
        # Get summary data
        total_reservations <- dbGetQuery(conn, "
        SELECT COUNT(*) as count 
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))$count
        
        total_income <- dbGetQuery(conn, "
        SELECT SUM(total_bill) as total 
        FROM reservations 
        WHERE payment_status = 'Paid'
          AND check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))$total
        
        total_income <- ifelse(is.na(total_income), 0, total_income)
        
        unpaid_income <- dbGetQuery(conn, "
        SELECT SUM(total_bill) as total 
        FROM reservations 
        WHERE payment_status = 'Unpaid'
          AND check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))$total
        
        unpaid_income <- ifelse(is.na(unpaid_income), 0, unpaid_income)
        
        pending_income <- dbGetQuery(conn, "
        SELECT SUM(total_bill) as total 
        FROM reservations 
        WHERE payment_status = 'Pending'
          AND check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))$total
        
        pending_income <- ifelse(is.na(pending_income), 0, pending_income)
        
        # Get room type breakdown
        room_type_data <- dbGetQuery(conn, "
        SELECT room_type, COUNT(*) as bookings, SUM(total_bill) as revenue
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        GROUP BY room_type
        ORDER BY revenue DESC
      ", params = list(as.character(start_date), as.character(end_date)))
        
        # Get payment status summary
        payment_data <- dbGetQuery(conn, "
        SELECT payment_status, COUNT(*) as count, SUM(total_bill) as amount
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        GROUP BY payment_status
      ", params = list(as.character(start_date), as.character(end_date)))
        
        # Get recent reservations
        recent_reservations <- dbGetQuery(conn, "
        SELECT room_number, guest_name, check_in, check_out, total_bill, payment_status
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        ORDER BY id DESC
        LIMIT 10
      ", params = list(as.character(start_date), as.character(end_date)))
        
        # Get average stay
        avg_stay <- dbGetQuery(conn, "
        SELECT AVG(julianday(check_out) - julianday(check_in)) as avg_stay
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
          AND julianday(check_out) > julianday(check_in)
      ", params = list(as.character(start_date), as.character(end_date)))$avg_stay
        
        # Create PDF content
        pdf(file, paper = "a4", width = 8.3, height = 11.7)
        
        # Title Page
        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
        plot.new()
        text(0.5, 0.9, "HOTEL RESERVATION SUMMARY REPORT", cex = 1.5, font = 2)
        text(0.5, 0.85, paste("Period:", format(start_date, "%B %d, %Y"), "to", 
                              format(end_date, "%B %d, %Y")), cex = 1.2)
        text(0.5, 0.8, paste("Report Period:", period_days, "days"), cex = 1)
        text(0.5, 0.75, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), cex = 0.9)
        
        # Executive Summary
        text(0.1, 0.65, "EXECUTIVE SUMMARY", cex = 1.2, font = 2, adj = 0)
        text(0.1, 0.60, paste("• Total Reservations:", total_reservations), adj = 0)
        text(0.1, 0.57, paste("• Total Revenue:", format_currency(total_income)), adj = 0)
        text(0.1, 0.54, paste("• Unpaid Amount:", format_currency(unpaid_income)), adj = 0)
        text(0.1, 0.51, paste("• Pending Amount:", format_currency(pending_income)), adj = 0)
        text(0.1, 0.48, paste("• Average Stay:", 
                              ifelse(is.na(avg_stay), "N/A", paste(round(avg_stay, 1), "days"))), 
             adj = 0)
        
        # Room Type Analysis
        if (nrow(room_type_data) > 0) {
          text(0.1, 0.40, "REVENUE BY ROOM TYPE", cex = 1.2, font = 2, adj = 0)
          
          y_pos <- 0.35
          for (i in 1:nrow(room_type_data)) {
            text(0.1, y_pos, 
                 paste("• ", room_type_data$room_type[i], ": ", 
                       room_type_data$bookings[i], " bookings, ",
                       format_currency(room_type_data$revenue[i])), 
                 adj = 0)
            y_pos <- y_pos - 0.03
          }
        }
        
        # Payment Status Summary
        if (nrow(payment_data) > 0) {
          text(0.1, 0.20, "PAYMENT STATUS SUMMARY", cex = 1.2, font = 2, adj = 0)
          
          y_pos <- 0.15
          for (i in 1:nrow(payment_data)) {
            text(0.1, y_pos, 
                 paste("• ", payment_data$payment_status[i], ": ", 
                       payment_data$count[i], " reservations, ",
                       format_currency(payment_data$amount[i])), 
                 adj = 0)
            y_pos <- y_pos - 0.03
          }
        }
        
        # Footer
        text(0.5, 0.05, "--- End of Report ---", cex = 0.8)
        
        dev.off()
        
        showNotification("PDF report generated successfully!", type = "default", duration = 3)
        
      }, error = function(e) {
        # Create a simple error PDF if something goes wrong
        try({
          pdf(file, paper = "a4")
          plot.new()
          text(0.5, 0.5, paste("Error generating report:\n", e$message), cex = 1.2, col = "red")
          dev.off()
        }, silent = TRUE)
        
        showNotification(paste("Error generating PDF:", e$message), 
                         type = "error", duration = 5)
      })
    }
  )
  
  # Download Excel Report
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("hotel_report_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(user$authenticated)
      
      tryCatch({
        conn <- db_connect()
        on.exit(db_disconnect(conn))
        
        # Get all data for selected period
        start_date <- input$report_start_date %||% (Sys.Date() - 365)
        end_date <- input$report_end_date %||% Sys.Date()
        
        data <- dbGetQuery(conn, "
          SELECT * FROM reservations 
          WHERE check_out >= ? AND check_in <= ?
          ORDER BY id DESC
        ", params = list(as.character(start_date), as.character(end_date)))
        
        if (nrow(data) > 0) {
          # Create summary sheet
          summary_data <- data.frame(
            Metric = c("Total Reservations", "Total Income (Paid)", "Total Income (Unpaid)", 
                       "Total Income (Pending)", "Average Stay Length", "Occupancy Rate"),
            Value = c(
              nrow(data),
              format_currency(sum(data$total_bill[data$payment_status == "Paid"], na.rm = TRUE)),
              format_currency(sum(data$total_bill[data$payment_status == "Unpaid"], na.rm = TRUE)),
              format_currency(sum(data$total_bill[data$payment_status == "Pending"], na.rm = TRUE)),
              paste(round(mean(as.numeric(difftime(data$check_out, data$check_in, units = "days")), na.rm = TRUE), 1), "days"),
              paste(round((length(unique(data$room_number)) / length(rv$all_rooms)) * 100, 1), "%")
            )
          )
          
          # Create room type analysis
          room_analysis <- data %>%
            group_by(room_type) %>%
            summarise(
              Bookings = n(),
              Revenue = sum(total_bill, na.rm = TRUE),
              Avg_Revenue = mean(total_bill, na.rm = TRUE),
              Avg_Guests = mean(guests, na.rm = TRUE),
              Avg_Stay = round(mean(as.numeric(difftime(check_out, check_in, units = "days")), na.rm = TRUE), 1)
            ) %>%
            arrange(desc(Revenue))
          
          # Write to Excel
          wb <- createWorkbook()
          addWorksheet(wb, "Summary")
          addWorksheet(wb, "Reservations")
          addWorksheet(wb, "Room Analysis")
          writeData(wb, "Summary", summary_data)
          writeData(wb, "Reservations", data)
          writeData(wb, "Room Analysis", room_analysis)
          saveWorkbook(wb, file)
        }
      }, error = function(e) {
        showNotification(paste("Error creating Excel report:", e$message), 
                         type = "error", duration = 5)
      })
    }
  )
  
  # Download Charts as PDF - IMPLEMENTED VERSION
  # Download Charts as PDF - FIXED VERSION
  output$download_charts <- downloadHandler(
    filename = function() {
      paste("hotel_charts_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(user$authenticated)
      
      tryCatch({
        # Get data for selected period
        start_date <- input$report_start_date %||% (Sys.Date() - 365)
        end_date <- input$report_end_date %||% Sys.Date()
        
        conn <- db_connect()
        on.exit(db_disconnect(conn))
        
        # Get data for charts
        revenue_data <- dbGetQuery(conn, "
        SELECT room_type, SUM(total_bill) as revenue, COUNT(*) as bookings
        FROM reservations 
        WHERE payment_status = 'Paid'
          AND check_out >= ? AND check_in <= ?
        GROUP BY room_type
        ORDER BY revenue DESC
      ", params = list(as.character(start_date), as.character(end_date)))
        
        monthly_data <- dbGetQuery(conn, "
        SELECT 
          strftime('%Y-%m', check_in) as month,
          SUM(total_bill) as revenue,
          COUNT(*) as bookings
        FROM reservations 
        WHERE payment_status = 'Paid'
          AND check_out >= ? AND check_in <= ?
        GROUP BY month
        ORDER BY month
      ", params = list(as.character(start_date), as.character(end_date)))
        
        payment_data <- dbGetQuery(conn, "
        SELECT 
          payment_status,
          COUNT(*) as count,
          SUM(total_bill) as amount
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        GROUP BY payment_status
      ", params = list(as.character(start_date), as.character(end_date)))
        
        # Get bookings data
        bookings_data <- dbGetQuery(conn, "
        SELECT 
          strftime('%Y-%m', check_in) as month,
          COUNT(*) as bookings,
          SUM(guests) as total_guests
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
        GROUP BY month
        ORDER BY month
      ", params = list(as.character(start_date), as.character(end_date)))
        
        # Create PDF with charts
        pdf(file, paper = "a4", width = 8.3, height = 11.7)
        
        # Page 1: Title and Overview
        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
        plot.new()
        text(0.5, 0.9, "HOTEL ANALYTICS DASHBOARD", cex = 1.8, font = 2)
        text(0.5, 0.85, paste("Period:", format(start_date, "%B %d, %Y"), "to", 
                              format(end_date, "%B %d, %Y")), cex = 1.3)
        text(0.5, 0.8, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), cex = 1)
        text(0.5, 0.7, "Charts and Analytics Report", cex = 1.2)
        
        # Calculate period days correctly
        period_days <- as.numeric(difftime(end_date, start_date, units = "days"))
        
        # Page 2: Revenue by Room Type
        if (nrow(revenue_data) > 0) {
          par(mfrow = c(1, 1), mar = c(7, 4, 4, 2))
          
          # Define colors
          room_colors <- c("Standard" = "#4CAF50", "Deluxe" = "#2196F3", 
                           "Suite" = "#9C27B0", "Presidential" = "#FF9800")
          
          # Create color vector matching the data
          colors <- sapply(revenue_data$room_type, function(type) {
            if (type %in% names(room_colors)) room_colors[type] else "#CCCCCC"
          })
          
          # Create bar plot
          bar_positions <- barplot(revenue_data$revenue, 
                                   names.arg = revenue_data$room_type,
                                   col = colors,
                                   main = paste("Revenue by Room Type\n", 
                                                format(start_date, "%b %d, %Y"), "to", 
                                                format(end_date, "%b %d, %Y")),
                                   xlab = "",
                                   ylab = "Revenue (₱)",
                                   las = 2,
                                   ylim = c(0, max(revenue_data$revenue) * 1.2),
                                   cex.names = 0.8)
          
          # Add value labels
          if (nrow(revenue_data) > 0) {
            text(x = bar_positions, 
                 y = revenue_data$revenue, 
                 labels = format_currency(revenue_data$revenue),
                 pos = 3, 
                 cex = 0.8)
          }
        } else {
          plot.new()
          text(0.5, 0.5, "No revenue data available for the selected period", cex = 1.2)
        }
        
        # Page 3: Monthly Revenue Trends
        if (nrow(monthly_data) > 1) {
          par(mfrow = c(1, 1), mar = c(5, 4, 4, 4))
          plot(1:nrow(monthly_data), monthly_data$revenue, 
               type = "o",
               col = "#2196F3",
               lwd = 2,
               main = paste("Monthly Revenue Trends\n", 
                            format(start_date, "%b %d, %Y"), "to", 
                            format(end_date, "%b %d, %Y")),
               xlab = "Month",
               ylab = "Revenue (₱)",
               xaxt = "n",
               ylim = c(0, max(monthly_data$revenue) * 1.1))
          
          axis(1, at = 1:nrow(monthly_data), labels = monthly_data$month, las = 2, cex.axis = 0.8)
          points(1:nrow(monthly_data), monthly_data$revenue, pch = 19, col = "#2196F3", cex = 1.5)
          grid()
        } else {
          plot.new()
          text(0.5, 0.5, "Insufficient data for monthly trend analysis", cex = 1.2)
        }
        
        # Page 4: Payment Status Distribution
        if (nrow(payment_data) > 0 && sum(payment_data$amount, na.rm = TRUE) > 0) {
          # Filter out zero amounts and ensure proper data
          payment_data <- payment_data[payment_data$amount > 0 & !is.na(payment_data$amount), ]
          
          if (nrow(payment_data) > 0) {
            par(mfrow = c(1, 1), mar = c(5, 4, 4, 8))
            
            # Define colors
            status_colors <- c("Paid" = "#4CAF50", "Unpaid" = "#F44336", "Pending" = "#FFC107")
            
            # Create color vector
            colors <- sapply(payment_data$payment_status, function(status) {
              if (status %in% names(status_colors)) status_colors[status] else "#CCCCCC"
            })
            
            # Prepare labels
            slices <- payment_data$amount
            lbls <- paste(payment_data$payment_status, "\n", 
                          format_currency(payment_data$amount))
            
            # Create pie chart
            pie(slices, 
                labels = lbls,
                col = colors,
                main = paste("Payment Status Distribution\n", 
                             format(start_date, "%b %d, %Y"), "to", 
                             format(end_date, "%b %d, %Y")),
                cex = 0.8)
          } else {
            plot.new()
            text(0.5, 0.5, "No payment data available for the selected period", cex = 1.2)
          }
        } else {
          plot.new()
          text(0.5, 0.5, "No payment data available for the selected period", cex = 1.2)
        }
        
        # Page 5: Bookings Analysis
        if (nrow(bookings_data) > 0 && sum(bookings_data$bookings, na.rm = TRUE) > 0) {
          par(mfrow = c(1, 1), mar = c(7, 4, 4, 4))
          
          # Create bar chart for bookings
          bar_positions <- barplot(bookings_data$bookings, 
                                   names.arg = bookings_data$month,
                                   col = "#4CAF50",
                                   main = paste("Monthly Bookings\n", 
                                                format(start_date, "%b %d, %Y"), "to", 
                                                format(end_date, "%b %d, %Y")),
                                   xlab = "",
                                   ylab = "Number of Bookings",
                                   las = 2,
                                   cex.names = 0.7,
                                   ylim = c(0, max(bookings_data$bookings) * 1.3))
          
          # Add booking counts on bars
          text(x = bar_positions, 
               y = bookings_data$bookings, 
               labels = bookings_data$bookings,
               pos = 3, 
               cex = 0.7)
          
          # Add total guests line (second y-axis)
          par(new = TRUE)
          plot(bar_positions, bookings_data$total_guests, 
               type = "o", 
               col = "#FF9800", 
               lwd = 2,
               axes = FALSE, 
               xlab = "", 
               ylab = "",
               ylim = c(0, max(bookings_data$total_guests) * 1.3))
          
          axis(4, col = "#FF9800", col.axis = "#FF9800")
          mtext("Total Guests", side = 4, line = 3, col = "#FF9800")
        }
        
        # Page 6: Summary Statistics
        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
        plot.new()
        text(0.5, 0.9, "SUMMARY STATISTICS", cex = 1.5, font = 2)
        
        # Get total statistics
        total_bookings <- dbGetQuery(conn, "
        SELECT COUNT(*) as total
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))$total
        
        total_guests <- dbGetQuery(conn, "
        SELECT SUM(guests) as total_guests
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
      ", params = list(as.character(start_date), as.character(end_date)))$total_guests
        
        total_guests <- ifelse(is.na(total_guests), 0, total_guests)
        
        avg_stay <- dbGetQuery(conn, "
        SELECT AVG(julianday(check_out) - julianday(check_in)) as avg_stay
        FROM reservations 
        WHERE check_out >= ? AND check_in <= ?
          AND julianday(check_out) > julianday(check_in)
      ", params = list(as.character(start_date), as.character(end_date)))$avg_stay
        
        # Display statistics
        y_pos <- 0.8
        stats <- c(
          paste("Report Period:", period_days, "days"),
          paste("Total Bookings:", total_bookings),
          paste("Total Guests:", total_guests),
          paste("Average Stay:", ifelse(is.na(avg_stay), "N/A", paste(round(avg_stay, 1), "days"))),
          paste("Data Range:", nrow(revenue_data), "room types"),
          paste("Monthly Data Points:", nrow(monthly_data))
        )
        
        for (stat in stats) {
          text(0.1, y_pos, stat, adj = 0, cex = 1.2)
          y_pos <- y_pos - 0.07
        }
        
        # Footer
        text(0.5, 0.1, "--- End of Charts Report ---", cex = 0.8)
        
        dev.off()
        
        showNotification("Charts PDF generated successfully!", type = "default", duration = 3)
        
      }, error = function(e) {
        # Create a simple error PDF if something goes wrong
        try({
          pdf(file, paper = "a4")
          plot.new()
          text(0.5, 0.5, paste("Error generating report:\n", e$message), cex = 1.2, col = "red")
          dev.off()
        }, silent = TRUE)
        
        showNotification(paste("Error generating charts PDF:", e$message), 
                         type = "error", duration = 5)
      })
    }
  )
  
  # Download Raw Data CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("hotel_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(user$authenticated)
      
      tryCatch({
        conn <- db_connect()
        on.exit(db_disconnect(conn))
        
        # Get all data for selected period
        start_date <- input$report_start_date %||% (Sys.Date() - 365)
        end_date <- input$report_end_date %||% Sys.Date()
        
        data <- dbGetQuery(conn, "
          SELECT * FROM reservations 
          WHERE check_out >= ? AND check_in <= ?
          ORDER BY id DESC
        ", params = list(as.character(start_date), as.character(end_date)))
        
        if (nrow(data) > 0) {
          write.csv(data, file, row.names = FALSE)
        }
      }, error = function(e) {
        showNotification(paste("Error downloading CSV:", e$message), 
                         type = "error", duration = 5)
      })
    }
  )
  
  # ========== SETTINGS TAB ==========
  
  # User settings
  output$current_user_name <- renderText({
    req(user$authenticated)
    user$full_name
  })
  
  output$user_role <- renderText({
    req(user$authenticated)
    user$role
  })
  
  # Change password
  observeEvent(input$change_password, {
    req(user$authenticated)
    
    current_pass <- input$current_password
    new_pass <- input$new_password
    confirm_pass <- input$confirm_password
    
    if (current_pass == "" || new_pass == "" || confirm_pass == "") {
      showNotification("Please fill all password fields.", type = "error", duration = 3)
      return()
    }
    
    if (new_pass != confirm_pass) {
      showNotification("New passwords do not match.", type = "error", duration = 3)
      return()
    }
    
    if (nchar(new_pass) < 6) {
      showNotification("Password must be at least 6 characters.", type = "error", duration = 3)
      return()
    }
    
    # Verify current password
    conn <- db_connect()
    on.exit(db_disconnect(conn))
    
    user_data <- dbGetQuery(conn, 
                            "SELECT password_hash FROM users WHERE id = ?",
                            params = list(user$user_id))
    
    if (nrow(user_data) == 0) {
      showNotification("User not found.", type = "error", duration = 3)
      return()
    }
    
    if (!verify_password(current_pass, user_data$password_hash)) {
      showNotification("Current password is incorrect.", type = "error", duration = 3)
      return()
    }
    
    # Update password
    new_hash <- hash_password(new_pass)
    dbExecute(conn, 
              "UPDATE users SET password_hash = ? WHERE id = ?",
              params = list(new_hash, user$user_id))
    
    showNotification("Password changed successfully!", type = "default", duration = 3)
    
    # Clear password fields
    updateTextInput(session, "current_password", value = "")
    updateTextInput(session, "new_password", value = "")
    updateTextInput(session, "confirm_password", value = "")
  })
  
  # Handle image upload preview
  observeEvent(input$room_image_upload, {
    req(user$authenticated, input$room_image_upload)
    
    # Check file size (max 5MB)
    if (input$room_image_upload$size > 5 * 1024 * 1024) {
      showNotification("File size too large. Maximum size is 5MB.", type = "error", duration = 5)
      return()
    }
    
    # Store temporary image data for preview
    rv$temp_image_data <- input$room_image_upload
  })
  
  # Handle remove image button
  observeEvent(input$remove_image_btn, {
    req(user$authenticated, rv$selected_room)
    
    # Show confirmation dialog
    showModal(
      modalDialog(
        title = tags$div(icon("exclamation-triangle"), " Remove Image"),
        size = "s",
        "Are you sure you want to remove the image for this room?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_remove_image", "Remove", 
                       class = "btn-danger")
        )
      )
    )
  })
  
  # Confirm remove image
  observeEvent(input$confirm_remove_image, {
    req(user$authenticated, rv$selected_room)
    
    tryCatch({
      # Delete image from database
      success <- delete_room_image(rv$selected_room)
      
      if (success) {
        showNotification("Image removed successfully!", type = "default", duration = 3)
        rv$current_room_image <- NULL
        
        # Update last db update to refresh displays
        rv$last_db_update <- Sys.time()
      } else {
        showNotification("Failed to remove image.", type = "error", duration = 3)
      }
      
      removeModal()
      
    }, error = function(e) {
      showNotification(paste("Error removing image:", e$message), type = "error", duration = 5)
    })
  })
  
  # Handle save room image
  observeEvent(input$save_room_image, {
    req(user$authenticated, rv$selected_room)
    
    tryCatch({
      if (is.null(rv$temp_image_data) && is.null(rv$current_room_image)) {
        showNotification("Please select an image to upload.", type = "warning", duration = 3)
        return()
      }
      
      if (!is.null(rv$temp_image_data)) {
        # Save new image
        success <- save_room_image(rv$selected_room, 
                                   rv$temp_image_data$datapath, 
                                   rv$temp_image_data$name)
        
        if (success) {
          showNotification("Image uploaded successfully!", type = "default", duration = 3)
          
          # Clear temporary data
          rv$temp_image_data <- NULL
          
          # Update current image
          rv$current_room_image <- get_room_image(rv$selected_room)
          
          # Update last db update to refresh displays
          rv$last_db_update <- Sys.time()
          
          # Close the modal
          removeModal()
          
        } else {
          showNotification("Failed to upload image.", type = "error", duration = 3)
        }
      } else {
        showNotification("No new image selected.", type = "warning", duration = 3)
      }
      
    }, error = function(e) {
      showNotification(paste("Error saving image:", e$message), type = "error", duration = 5)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
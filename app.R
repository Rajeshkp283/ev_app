# Load necessary libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinythemes)
  library(leaflet)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(shinyjs)
  library(leaflet.extras)
  library(geosphere)
  library(plotly)
  library(shinyWidgets)
  library(colourpicker)
})

# Set the environment variable for OpenChargeMap API key
Sys.setenv(OPENCHARGEMAP_API_KEY = "34558211-9937-4add-8661-6a042c661513")

# Sample initial data for demonstration
initial_charging_data <- data.frame(
  date = as.Date(c('2023-10-26', '2023-10-25', '2023-10-24')),
  kWh = c(55, 62, 48),
  cost = c(5.5, 6.2, 4.8),
  time = c("12:00", "13:00", "14:00"),
  stringsAsFactors = FALSE
)

initial_payment_history <- data.frame(
  Date = as.Date(c('2023-06-01', '2023-05-15', '2023-05-01')),
  Amount = c(20.00, 15.00, 25.00),
  Method = c('Credit Card', 'PayPal', 'Bank Transfer'),
  Status = c('Completed', 'Completed', 'Pending'),
  stringsAsFactors = FALSE
)

# Function to fetch EV charging stations from OpenChargeMap API
fetch_charging_stations <- function(lat, lon, api_key) {
  url <- sprintf("https://api.openchargemap.io/v3/poi/?output=json&latitude=%s&longitude=%s&maxresults=10&compact=true&verbose=false&key=%s", lat, lon, api_key)
  res <- GET(url)
  
  if (http_error(res)) {
    return(list(success = FALSE, message = sprintf("Error fetching data: %s", http_status(res)$message)))
  }
  
  data <- try(fromJSON(content(res, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE), silent = TRUE)
  if (inherits(data, "try-error") || is.null(data) || length(data) == 0) {
    return(list(success = FALSE, message = "No data found or unexpected format."))
  }
  
  stations_data <- lapply(data, function(station) {
    if (!is.null(station$AddressInfo) && !is.null(station$AddressInfo$Title)) {
      list(
        title = station$AddressInfo$Title,
        latitude = station$AddressInfo$Latitude,
        longitude = station$AddressInfo$Longitude,
        address = station$AddressInfo$AddressLine1,
        distance = if (!is.null(station$AddressInfo$Distance)) station$AddressInfo$Distance else NA
      )
    } else {
      NULL
    }
  })
  
  # Remove NULL entries
  stations_data <- Filter(Negate(is.null), stations_data)
  
  list(success = TRUE, data = stations_data)
}

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "EV Stations Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Charging Status", tabName = "real_time_charging", icon = icon("battery-full")),
      menuItem("Charging Trends", tabName = "charging_trends", icon = icon("bolt")),
      menuItem("Payment", tabName = "payment", icon = icon("credit-card")),
      menuItem("Customize Pod", tabName = "customize_pod", icon = icon("cog")),
      menuItem("Place Order", tabName = "place_order", icon = icon("shopping-cart")),
      menuItem("Order Tracking", tabName = "order_tracking", icon = icon("truck")),
      menuItem("Payment History", tabName = "payment_history", icon = icon("history")),
      menuItem("Community", tabName = "community", icon = icon("users")),
      menuItem("Contact", tabName = "contact", icon = icon("envelope"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f4f6f9;
      }
      .main-header .logo {
        background-color: #3c8dbc;
        color: white;
        font-size: 18px;
      }
      .main-header .navbar {
        background-color: #3c8dbc;
      }
      .box {
        border-top: 3px solid #3c8dbc;
      }
      .small-box.bg-aqua, .small-box.bg-green, .small-box.bg-yellow, .small-box.bg-red {
        background: linear-gradient(45deg, #3c8dbc, #00c0ef);
      }
      .faded-tile {
        background-color: white;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        padding: 20px;
      }
      .content {
        font-size: 16px;
        line-height: 1.6;
      }
      .title {
        font-size: 24px;
        font-weight: bold;
        color: #3c8dbc;
        text-align: center;
        margin-bottom: 20px;
      }
      .charging-status {
        font-size: 20px;
        font-weight: bold;
        color: #3c8dbc;
        text-align: center;
        margin-top: 20px;
      }
      .charging-info {
        font-size: 18px;
        text-align: center;
        margin-top: 10px;
      }
      .charging-button {
        width: 48%; 
        margin-right: 4%; 
        text-align: center;
      }
      .charging-button-last {
        width: 48%; 
        text-align: center;
      }
    "))),
    tags$script('
      // Function to get current location and pass it to Shiny server
      function getLocation() {
        if (navigator.geolocation) {
          navigator.geolocation.getCurrentPosition(onSuccess, onError);
        } else {
          Shiny.setInputValue("geolocation", {error: "Geolocation is not supported by this browser."});
        }
      }

      function onSuccess(position) {
        Shiny.setInputValue("geolocation", {
          lat: position.coords.latitude,
          lon: position.coords.longitude
        });
      }

      function onError(error) {
        Shiny.setInputValue("geolocation", {error: "Geolocation has been disabled in this document by permissions policy. Please enable location services in your browser settings."});
      }

      // Call the getLocation function when the Shiny app is initialized
      $(document).on("shiny:connected", function(event) {
        getLocation();
      });
    '),
    
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       div(class = "faded-tile",
                           h1("Welcome to Charging Pod e-portal", class = "title"),
                           img(src = "ev_charging_pod.png", alt = "EV Charging Pod Image", height = "300px", style = "display: block; margin: 0 auto;"),
                           div(class = "content", style = "margin-top: 20px;",
                               p("Charging Pod is an innovative electric vehicle (EV) charging pod manufacturing and customization platform, created by Rajesh Kumar Padhy and Professor Kai Cheng from Brunel University London."),
                               p("Our platform leverages the latest technologies and a commitment to sustainability, aiming to reduce carbon footprints while delivering exceptional EV charging pod services. Whether you're looking for reliable home charging solutions or advanced commercial installations, Charging Pod has you covered."),
                               p("Thank you for choosing Charging Pod. We are excited to revolutionize the way you power your EV!"),
                               p("For inquiries and support, please visit the 'Contact Support' tab."),
                               p("Follow us on our social media channels to stay updated with the latest news and features!")
                           ),
                           div(class = "social-media-icons", style = "text-align: center; margin-top: 20px;",
                               icon("twitter", lib = "font-awesome", class = "fa-2x", style = "margin-right: 10px;"),
                               icon("facebook", lib = "font-awesome", class = "fa-2x", style = "margin-right: 10px;"),
                               icon("instagram", lib = "font-awesome", class = "fa-2x", style = "margin-right: 10px;"),
                               icon("linkedin", lib = "font-awesome", class = "fa-2x")
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "login",
              fluidRow(
                column(12,
                       div(class = "faded-tile",
                           h2("Login", style = "text-align: center; margin-bottom: 20px;"),
                           div(class = "form-group",
                               tags$label("Email:", `for` = "login_email"),
                               tags$input(id = "login_email", type = "email", class = "form-control", placeholder = "Enter your email address", style = "margin-bottom: 10px;")
                           ),
                           div(class = "form-group",
                               tags$label("Password:", `for` = "login_password"),
                               tags$input(id = "login_password", type = "password", class = "form-control", placeholder = "Enter your password", style = "margin-bottom: 10px;")
                           ),
                           actionButton("login_button", "Login", class = "btn btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                           tags$a(href = "#", "Forgot Password?", style = "display: block; text-align: center;"),
                           useShinyjs()
                       )
                )
              )
      ),
      tabItem(tabName = "real_time_charging",
              fluidRow(
                column(12,
                       div(class = "faded-tile",
                           h2("Charging Status", class = "charging-status"),
                           fluidRow(
                             column(6, plotlyOutput("charging_percentage_graph")),
                             column(6, plotlyOutput("charging_details_graph"))
                           ),
                           fluidRow(
                             column(6, plotlyOutput("total_time_graph")),
                             column(6, plotlyOutput("time_last_graph"))
                           ),
                           fluidRow(
                             column(6, class = "charging-button",
                                    actionButton("start_charging", "Start Charging", class = "btn btn-success")),
                             column(6, class = "charging-button-last",
                                    actionButton("stop_charging", "Stop Charging", class = "btn btn-danger"))
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Last Charging Information", status = "primary", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("last_charging_info")),
                box(title = "Statistics", status = "primary", solidHeader = TRUE, width = 6,
                    p("Total Energy Used: ", textOutput("total_energy_used", inline = TRUE), class = "content"),
                    p("Average Cost: $", textOutput("average_cost", inline = TRUE), class = "content"),
                    p("CO2 Savings: ", textOutput("co2_savings", inline = TRUE), " kg", class = "content"),
                    p("Peak Usage Time: ", textOutput("peak_usage_time", inline = TRUE), class = "content"))
              ),
              fluidRow(
                box(title = "Usage Trend", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("trend_plot", height = "300px")),
                box(title = "Monthly Energy Consumption", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("monthly_energy_plot", height = "300px"))
              ),
              fluidRow(
                tabBox(
                  title = "Location and Nearest Charging Pod Details",
                  width = 12,
                  id = "tabset1", height = "500px",
                  tabPanel("Map", leafletOutput("location_map", height = "450px")),
                  tabPanel("Details", DTOutput("nearest_stations_table"))
                )
              )
      ),
      tabItem(tabName = "charging_trends",
              fluidRow(
                box(title = "Add New Charging Data", status = "primary", solidHeader = TRUE, width = 12,
                    numericInput("new_kwh", "Enter kWh:", value = 50, min = 0),
                    numericInput("new_cost", "Enter Cost:", value = 5, min = 0),
                    textInput("new_time", "Enter Time (HH:MM):", value = "12:00"),
                    actionButton("add_data", "Add Data"))
              )
      ),
      tabItem(tabName = "payment",
              fluidRow(
                box(title = "Payment Methods", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("show_payment_modal", "Add Payment Method"),
                    DT::dataTableOutput("payment_history_table"))
              ),
              fluidRow(
                box(title = "Payment Trends", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("payment_trends_plot", height = "300px"))
              )
      ),
      tabItem(tabName = "customize_pod",
              fluidRow(
                column(6, wellPanel(
                  selectInput("charging_speed", "Charging Speed:", choices = c("Standard", "Fast", "Super Fast")),
                  sliderInput("cable_length", "Cable Length (meters):", min = 1, max = 10, value = 5),
                  selectInput("energy_source", "Energy Source:", choices = c("Solar", "Wind", "Hydro", "Grid")),
                  numericInput("charging_power", "Charging Power (kW):", value = 50, min = 10, max = 200),
                  colourInput("pod_colour", "Pod Colour", value = "#0000FF"),
                  actionButton("save_customization", "Save Customization", class = "btn btn-success")
                )),
                column(6,
                       imageOutput("custom_pod_image")
                )
              )
      ),
      tabItem(tabName = "place_order",
              fluidRow(
                column(6,
                       textInput("customer_email", "Email", value = ""),
                       textAreaInput("shipping_address", "Shipping Address", value = ""),
                       actionButton("confirm_order", "Confirm Order", class = "btn btn-primary")
                ),
                column(6,
                       h4("Order Summary"),
                       verbatimTextOutput("order_summary")
                )
              )
      ),
      tabItem(tabName = "order_tracking",
              fluidRow(
                column(12,
                       div(class = "faded-tile",
                           h2("Order Tracking", style = "text-align: center; margin-bottom: 20px;")
                           # Add order tracking options here
                       )
                )
              )
      ),
      tabItem(tabName = "payment_history",
              fluidRow(
                column(12,
                       div(class = "faded-tile",
                           h2("Payment History", style = "text-align: center; margin-bottom: 20px;"),
                           DT::dataTableOutput("payment_history_table")
                       )
                )
              )
      ),
      tabItem(tabName = "community",
              fluidRow(
                column(12,
                       div(class = "faded-tile",
                           h2("Community", style = "text-align: center; margin-bottom: 20px;"),
                           tabsetPanel(
                             tabPanel("Discussions",
                                      textInput("discussion_title", "Title", placeholder = "Enter discussion title"),
                                      textAreaInput("discussion_content", "Content", placeholder = "Enter discussion content"),
                                      actionButton("post_discussion", "Post Discussion", class = "btn btn-primary"),
                                      DT::dataTableOutput("discussion_table")
                             ),
                             tabPanel("Events",
                                      DT::dataTableOutput("events_table"),
                                      actionButton("add_event", "Add Event", class = "btn btn-primary")
                             ),
                             tabPanel("Members",
                                      DT::dataTableOutput("members_table")
                             )
                           )
                       )
                )
              )
      ),
      tabItem(tabName = "contact",
              fluidRow(
                column(12,
                       div(class = "faded-tile",
                           h2("Contact Us", style = "text-align: center; margin-bottom: 20px;"),
                           div(class = "form-group",
                               tags$label("Email:", `for` = "email"),
                               tags$input(id = "email", type = "email", class = "form-control", placeholder = "Enter your email address", style = "margin-bottom: 10px;")
                           ),
                           div(class = "form-group",
                               tags$label("Message:", `for` = "message"),
                               tags$textarea(id = "message", class = "form-control", placeholder = "Enter your message", style = "margin-bottom: 10px;")
                           ),
                           actionButton("send_message", "Send Message", class = "btn btn-primary", style = "width: 100%;"),
                           useShinyjs()
                       )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store payment history
  payment_history <- reactiveVal(initial_payment_history)
  
  # Reactive value to store charging data
  charging_data <- reactiveVal(initial_charging_data)
  
  # Reactive value to store real-time charging data
  charging_status <- reactiveValues(
    is_charging = FALSE,
    percentage = 0,
    total_time = 0,
    remaining_time = 0,
    time_last = 0
  )
  
  # Reactive values to store user location
  user_location <- reactiveValues(lat = NULL, lon = NULL)
  
  # Reactive value to store the current custom image path
  custom_image_path <- reactiveVal("www/Screenshot 2024-07-29 010052.png")
  
  # Reactive values for community
  discussions <- reactiveVal(data.frame(
    Title = character(),
    Content = character(),
    Date = as.Date(character()),
    stringsAsFactors = FALSE
  ))
  
  events <- reactiveVal(data.frame(
    Title = character(),
    Start = as.Date(character()),
    End = as.Date(character()),
    stringsAsFactors = FALSE
  ))
  
  members <- reactiveVal(data.frame(
    Name = character(),
    Email = character(),
    Joined = as.Date(character()),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$geolocation, {
    if (!is.null(input$geolocation$error)) {
      output$location_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addPopups(lng = 0, lat = 0, popup = input$geolocation$error)
      })
      return()
    }
    
    user_lat <- input$geolocation$lat
    user_lon <- input$geolocation$lon
    
    stations_response <- fetch_charging_stations(user_lat, user_lon, Sys.getenv("OPENCHARGEMAP_API_KEY"))
    
    if (stations_response$success) {
      stations_data <- stations_response$data
      leaflet_map <- leaflet() %>%
        addTiles() %>%
        setView(lng = user_lon, lat = user_lat, zoom = 12)
      
      for (station in stations_data) {
        leaflet_map <- leaflet_map %>%
          addMarkers(
            lng = station$longitude,
            lat = station$latitude,
            popup = paste0(
              "Name: ", station$title,
              "<br>Address: ", station$address,
              "<br>Distance: ", round(station$distance, 2), " km"
            )
          )
      }
      
      output$location_map <- renderLeaflet(leaflet_map)
      
      stations_df <- do.call(rbind, lapply(stations_data, as.data.frame))
      stations_df <- stations_df[order(stations_df$distance), ]
      stations_df$distance <- round(stations_df$distance, 2) # Round distance to 2 decimal places
      
      output$nearest_stations_table <- renderDT({
        datatable(stations_df, options = list(pageLength = 5))
      })
      
    } else {
      output$location_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng = user_lon, lat = user_lat, zoom = 12) %>%
          addPopups(lng = user_lon, lat = user_lat, popup = stations_response$message)
      })
    }
  })
  
  # Existing outputs and other reactive elements...
  
  output$location_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, autoCenter = TRUE, setView = TRUE)) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }")
      ))
  })
  
  # Render payment history table
  output$payment_history_table <- DT::renderDataTable({
    data <- payment_history()
    if (is.null(data) || nrow(data) == 0) {
      datatable(data.frame(Message = "No data available"))
    } else {
      datatable(data)
    }
  })
  
  # Render last charging information table
  output$last_charging_info <- DT::renderDataTable({
    data <- charging_data()
    if (is.null(data) || nrow(data) == 0) {
      datatable(data.frame(Message = "No data available"))
    } else {
      datatable(data)
    }
  })
  
  # Add new charging data and update table
  observeEvent(input$add_data, {
    new_data <- data.frame(
      date = Sys.Date(),
      kWh = input$new_kwh,
      cost = input$new_cost,
      time = input$new_time,
      stringsAsFactors = FALSE
    )
    updated_data <- bind_rows(charging_data(), new_data)
    charging_data(updated_data)
  })
  
  # Render usage trend plot
  output$trend_plot <- renderPlotly({
    charging_data_filtered <- charging_data()
    if (nrow(charging_data_filtered) > 1) {
      p <- ggplot(charging_data_filtered, aes(x = date, y = kWh, group = 1)) +
        geom_line(color = "blue", linewidth = 1) +
        geom_point(color = "red", size = 3) +
        labs(x = "Date", y = "kWh", title = "Usage Trend") +
        theme_minimal()
      ggplotly(p, width = 600, height = 300)
    } else {
      plot_ly(type = 'scatter', mode = 'markers', width = 600, height = 300) %>%
        add_annotations(text = "Not enough data to plot.", x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 15)) %>%
        layout(xaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE),
               yaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE))
    }
  })
  
  # Render monthly energy consumption plot
  output$monthly_energy_plot <- renderPlotly({
    monthly_data <- charging_data() %>%
      mutate(month = format(date, "%Y-%m")) %>%
      group_by(month) %>%
      summarise(total_kWh = sum(kWh))
    
    if (nrow(monthly_data) > 1) {
      p <- ggplot(monthly_data, aes(x = month, y = total_kWh, group = 1)) +
        geom_line(color = "blue", linewidth = 1) +
        geom_point(color = "red", size = 3) +
        labs(x = "Month", y = "Total kWh", title = "Monthly Energy Consumption") +
        theme_minimal()
      ggplotly(p, width = 600, height = 300)
    } else {
      plot_ly(type = 'scatter', mode = 'markers', width = 600, height = 300) %>%
        add_annotations(text = "Not enough data to plot.", x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 15)) %>%
        layout(xaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE),
               yaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE))
    }
  })
  
  # Render payment trends plot
  output$payment_trends_plot <- renderPlotly({
    payment_history_filtered <- payment_history()
    if (nrow(payment_history_filtered) > 1) {
      p <- ggplot(payment_history_filtered, aes(x = Date, y = Amount, color = Method, group = Method)) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        labs(x = "Date", y = "Amount", title = "Payment Trends") +
        theme_minimal()
      ggplotly(p, width = 600, height = 300)
    } else {
      plot_ly(type = 'scatter', mode = 'markers', width = 600, height = 300) %>%
        add_annotations(text = "Not enough data to plot.", x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 15)) %>%
        layout(xaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE),
               yaxis = list(showline = FALSE, showticklabels = FALSE, zeroline = FALSE))
    }
  })
  
  # Render statistics
  output$total_energy_used <- renderText({
    sum(charging_data()$kWh, na.rm = TRUE)
  })
  
  output$average_cost <- renderText({
    round(mean(charging_data()$cost, na.rm = TRUE), 2)
  })
  
  output$co2_savings <- renderText({
    round(sum(charging_data()$kWh, na.rm = TRUE) * 0.37, 2)  # Assuming 0.37 kg CO2 saved per kWh
  })
  
  output$peak_usage_time <- renderText({
    peak_time <- charging_data() %>%
      group_by(time) %>%
      summarise(total_kWh = sum(kWh)) %>%
      arrange(desc(total_kWh)) %>%
      slice(1) %>%
      pull(time)
    peak_time
  })
  
  output$total_payments <- renderText({
    sum(payment_history()$Amount, na.rm = TRUE)
  })
  
  output$average_payment <- renderText({
    round(mean(payment_history()$Amount, na.rm = TRUE), 2)
  })
  
  output$most_used_method <- renderText({
    payment_history() %>%
      count(Method) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(Method)
  })
  
  output$pending_payments <- renderText({
    nrow(payment_history() %>% filter(Status == "Pending"))
  })
  
  # Show modal for adding payment method
  observeEvent(input$show_payment_modal, {
    showModal(modalDialog(
      title = "Add Payment Method",
      selectInput("payment_method_type", "Select Payment Method:", 
                  choices = c("Credit/Debit Card", "Bank Transfer", "PayPal", "Other")),
      conditionalPanel(
        condition = "input.payment_method_type == 'Credit/Debit Card'",
        textInput("card_number", "Card Number:", ""),
        textInput("expiry_date", "Expiry Date (MM/YYYY):", ""),
        textInput("cvv", "CVV:", "")
      ),
      conditionalPanel(
        condition = "input.payment_method_type == 'Bank Transfer'",
        textInput("account_number", "Account Number:", ""),
        textInput("routing_number", "Routing Number:", "")
      ),
      conditionalPanel(
        condition = "input.payment_method_type == 'PayPal'",
        textInput("paypal_email", "PayPal Email:", "")
      ),
      numericInput("payment_amount", "Payment Amount:", value = 0, min = 0),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_payment_method", "Save")
      )
    ))
  })
  
  # Save new payment method and update history
  observeEvent(input$save_payment_method, {
    payment_type <- input$payment_method_type
    payment_amount <- input$payment_amount
    
    new_payment <- data.frame(
      Date = Sys.Date(),  # Use current date as payment date
      Amount = payment_amount,
      Method = payment_type,
      Status = "Pending",
      stringsAsFactors = FALSE
    )
    
    # Update payment history with new payment
    current_history <- payment_history()
    updated_history <- bind_rows(current_history, new_payment)
    payment_history(updated_history)
    
    # Close modal after saving
    removeModal()
  })
  
  # Handle contact form submission
  observeEvent(input$send_message, {
    email <- input$email
    message <- input$message
    
    if (email != "" && message != "") {
      showNotification("Message sent successfully!", type = "message", duration = 5)
      updateTextInput(session, "email", value = "")
      updateTextAreaInput(session, "message", value = "")
    } else {
      showNotification("Please fill in both fields.", type = "error", duration = 5)
    }
  })
  
  # Handle login form submission
  observeEvent(input$login_button, {
    email <- input$login_email
    password <- input$login_password
    
    if (email != "" && password != "") {
      showNotification("Successfully logged in!", type = "message", duration = 5)
      updateTextInput(session, "login_email", value = "")
      updateTextInput(session, "login_password", value = "")
    } else {
      showNotification("Please fill in both fields.", type = "error", duration = 5)
    }
  })
  
  # Handle real-time charging data
  observeEvent(input$start_charging, {
    charging_status$is_charging <- TRUE
    charging_status$percentage <- 0
    charging_status$total_time <- 0
    charging_status$remaining_time <- 100
    charging_status$time_last <- 0
  })
  
  observeEvent(input$stop_charging, {
    charging_status$is_charging <- FALSE
  })
  
  observe({
    if (charging_status$is_charging) {
      invalidateLater(1000, session)
      isolate({
        charging_status$percentage <- min(100, charging_status$percentage + 1)
        charging_status$total_time <- charging_status$total_time + 1
        charging_status$remaining_time <- max(0, 100 - charging_status$percentage)
        charging_status$time_last <- charging_status$total_time * 2
      })
    }
  })
  
  output$charging_percentage_graph <- renderPlotly({
    plot_ly(
      type = 'indicator',
      mode = 'gauge+number',
      value = charging_status$percentage,
      title = list(text = "Current Charging Percentage"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = 'green'),
        steps = list(
          list(range = c(0, 50), color = 'lightgray'),
          list(range = c(50, 100), color = 'gray')
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90
        )
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 30, t = 50, b = 20),
        height = 300,
        showlegend = FALSE,
        modebar = list(orientation = 'h', bgcolor = 'transparent')
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$charging_details_graph <- renderPlotly({
    plot_ly(
      type = 'indicator',
      mode = 'gauge+number',
      value = charging_status$remaining_time,
      title = list(text = "Expected Charging Time (minutes)"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = 'blue'),
        steps = list(
          list(range = c(0, 50), color = 'lightgray'),
          list(range = c(50, 100), color = 'gray')
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 50
        )
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 30, t = 50, b = 20),
        height = 300,
        showlegend = FALSE,
        modebar = list(orientation = 'h', bgcolor = 'transparent')
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$total_time_graph <- renderPlotly({
    plot_ly(
      type = 'indicator',
      mode = 'gauge+number',
      value = charging_status$total_time,
      title = list(text = "Total Time Taken to Charge (minutes)"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = 'orange'),
        steps = list(
          list(range = c(0, 50), color = 'lightgray'),
          list(range = c(50, 100), color = 'gray')
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 50
        )
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 30, t = 50, b = 20),
        height = 300,
        showlegend = FALSE,
        modebar = list(orientation = 'h', bgcolor = 'transparent')
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$time_last_graph <- renderPlotly({
    plot_ly(
      type = 'indicator',
      mode = 'gauge+number',
      value = charging_status$time_last,
      title = list(text = "Time the Charge Will Last (minutes)"),
      gauge = list(
        axis = list(range = list(0, 200)),
        bar = list(color = 'purple'),
        steps = list(
          list(range = c(0, 100), color = 'lightgray'),
          list(range = c(100, 200), color = 'gray')
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 100
        )
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 30, t = 50, b = 20),
        height = 300,
        showlegend = FALSE,
        modebar = list(orientation = 'h', bgcolor = 'transparent')
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Render the image UI for customization
  output$custom_pod_image <- renderImage({
    list(src = custom_image_path(),
         contentType = 'image/png',
         width = "100%")
  }, deleteFile = FALSE)
  
  # Handle customization saving and image preview
  observeEvent(input$save_customization, {
    new_custom_image_path <- "www/Screenshot 2024-07-29 010052.png"
    custom_image_path(new_custom_image_path)
  })
  
  # Handle order confirmation and generate order/tracking IDs
  observeEvent(input$confirm_order, {
    order_id <- paste0("ORD", sample(100000, 1))
    tracking_id <- paste0("TRK", sample(100000, 1))
    
    # Assuming we save order details here
    # You would typically save this to a database or a reactive value
    
    output$order_summary <- renderText({
      paste("Order ID:", order_id, "\nTracking ID:", tracking_id, "\nEmail:", input$customer_email, "\nAddress:", input$shipping_address)
    })
    
    showNotification(paste("Order confirmed. Your order ID is", order_id), type = "success")
  })
  
  # Handle posting a discussion
  observeEvent(input$post_discussion, {
    new_discussion <- data.frame(
      Title = input$discussion_title,
      Content = input$discussion_content,
      Date = Sys.Date(),
      stringsAsFactors = FALSE
    )
    updated_discussions <- bind_rows(discussions(), new_discussion)
    discussions(updated_discussions)
  })
  
  # Render discussion table
  output$discussion_table <- DT::renderDataTable({
    datatable(discussions())
  })
  
  # Handle adding an event
  observeEvent(input$add_event, {
    showModal(modalDialog(
      title = "Add Event",
      textInput("event_title", "Event Title", value = ""),
      dateInput("event_start", "Start Date", value = Sys.Date()),
      dateInput("event_end", "End Date", value = Sys.Date()),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_event", "Save Event")
      )
    ))
  })
  
  observeEvent(input$save_event, {
    new_event <- data.frame(
      Title = input$event_title,
      Start = as.Date(input$event_start),
      End = as.Date(input$event_end),
      stringsAsFactors = FALSE
    )
    updated_events <- bind_rows(events(), new_event)
    events(updated_events)
    removeModal()
  })
  
  # Render events table
  output$events_table <- DT::renderDataTable({
    datatable(events())
  })
  
  # Render members table
  output$members_table <- DT::renderDataTable({
    datatable(members())
  })
}

# Function to launch the app in Google Chrome
launch_in_chrome <- function(app_url) {
  chrome_path <- "C:/Program Files/Google/Chrome/Application/chrome.exe" # Change this path if Chrome is installed elsewhere
  system2(chrome_path, args = c("--new-window", app_url))
}

# Run the application
app_port <- 6949  # Choose an appropriate port
app_url <- paste0("http://127.0.0.1:", app_port)
launch_in_chrome(app_url)
shinyApp(ui = ui, server = server, options = list(port = app_port))

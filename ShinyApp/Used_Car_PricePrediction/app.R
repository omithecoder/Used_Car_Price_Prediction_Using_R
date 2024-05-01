library(shiny)
library(shinydashboard)
library(DT)
library(randomForest)  # Assuming you trained your model using randomForest

# Load your trained model
price <- "price"

car_data <- data.frame(
  car_name = c("Corolla", "Civic", "F-150", "Elantra", "Camaro")
)

# Welcome Page 
welcome_ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    tags$style(HTML("
      .content-wrapper {
        background-image: url('www/welcome_background.jpg');
        background-size: cover;
      }
      #welcome-page {
        height: 100vh;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }
      #welcome-page h1 {
        color: white;
        font-size: 36px;
        font-weight: bold;
        text-align: center;
        padding-top: 200px;
      }
      #welcome-page .btn-primary {
        margin-top: 30px;
      }
    ")),
    tags$div(
      id = "welcome-page",
      h1("Welcome to Car Price Prediction"),
      actionButton("enter_button", "Enter", class = "btn btn-primary")
    )
  )
)

# Define UI for application
main_ui <- dashboardPage(
  dashboardHeader(
    title = div(
      "Used Car Price Prediction",
      tags$style(HTML("font-size: 24px;"))
    )
  ),
  dashboardSidebar(
    # Input fields
    width = "30%",
    tags$head(
      tags$style(
        HTML(".sidebar .form-group.shiny-input-container {
          width: 90%;
          align-item:center;
          margin-left:2rem;
        }
        .img
        {
        margin-bottom:3rem;
        }
        "
        )
      )
    ),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      numericInput("year" ,"Year", min = 1998, max = 2024, value = 2015),
      selectInput("brand", "Brand of Car",
                  choices <- c("Maruti", "Hyundai", "Honda", "Audi", 
                               "Nissan", "Toyota", "Volkswagen", "Tata", "Land", 
                               "Mitsubishi", "Renault", "Mercedes-Benz", "BMW", 
                               "Mahindra", "Ford", "Porsche", "Datsun", "Jaguar", 
                               "Volvo", "Chevrolet", "Skoda", "Mini", "Fiat", "Jeep", 
                               "Smart", "Ambassador", "Isuzu", "Force", "Bentley", 
                               "Lamborghini")),
      selectInput("location", "Location of Car",
                  choices <- c("Mumbai", "Pune", "Chennai", "Coimbatore", "Hyderabad", "Jaipur", "Kochi", "Kolkata", "Delhi", "Bangalore", "Ahmedabad")),
      numericInput("kilometer", "Kilometer Driven", value = 0, min = 0, max = 100000),
      selectInput("fuel", "Fuel Type", choices = c("Petrol", "Diesel", "CNG", "LPG")),
      selectInput("transmission", "Transmission", choices = c("Manual", "Automatic")),
      selectInput("owner", "Owner Type",
                  choices = c("First", "Second", "Third", "Fourth", "Test Drive Car")),
      numericInput("mileage", "Mileage (kmpl)", value = 0, min = 5, max = 30),
      numericInput("power", "Power (bhp)", value = 0, min = 200, max = 2000),
      numericInput("engine", "Engine (CC)", value = 0, min = 600, max = 8000),
      numericInput("seats", "Number of Seats", value = 0, min = 2, max = 10),
      selectInput("car_name", "Name of Car", choices = car_data$car_name)
      
    )
  ),
  dashboardBody(
    # Main panel for displaying results and the car image
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Prediction",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  height = "50%",
                  DTOutput("prediction")
                ),
                box(
                  title = "Car Image",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  height = "50%",
                  div(
                    style = "display: flex; justify-content: center; align-items: flex-start;margin-bottom:6rem;margin-left:10rem;",
                    imageOutput("carImage")
                  )
                ),
                box(
                  title = "Price Prediction",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  height = "50%",
                  textOutput("formatted_price"),
                  actionButton("predict_button", "Predict Price", class = "btn btn-primary", style ="color:black;")
                )
              )
      )
    )
  )
)

# Combine welcome page UI and main app UI


# Define server logic
server <- function(input, output, session) {
  
  car_data <- data.frame(
    car_name = c("Corolla", "Civic", "F-150", "Elantra", "Camaro"),
    image_file = c("corolla.jpg", "civic.jpg", "f150.jpg", "elantra.jpg", "camaro.jpg")
  )
  
  observeEvent(input$enter_button, {
    # Redirect to the main app page
    updateTabItems(session, "home")
  })
  
  # Server logic for prediction
  output$prediction <- renderDT({
    # Creating a data frame with parameters and values
    data <- data.frame(
      "Parameters" = c("Name of Car", "Year", "Brand", "Kilometer Driven", "Fuel Type", "Transmission", "Owner Type", "Mileage", "Power", "Engine", "Seats", "Price"),
      "Values" = c(input$car_name, input$year, input$brand, input$kilometer, input$fuel, input$transmission, input$owner, input$mileage, input$power, input$engine, input$seats, "A"),
      stringsAsFactors = FALSE
    )
    
    # Highlighting the row with "Price" equal to "A"
    data$Parameters <- ifelse(data$Parameters == "Price", price, data$Parameters)
    
    # Returning the data frame as a datatable
    datatable(data, rownames = FALSE, options = list(
      columnDefs = list(
        list(targets = "_all", className = "valueColumn")
      )
    ))
  })
  
  # Dynamically render car image based on the selected car name
  output$carImage <- renderImage({
    # Get the selected car name
    selected_car <- input$car_name
    
    # Find the corresponding image file name based on the selected car name
    image_file <- car_data$image_file[car_data$car_name == selected_car]
    
    # If image file name is found, render the image
    if (!is.na(image_file) && file.exists(paste0("www/", image_file))) {
      list(src = paste0("www/", image_file), width = "70%" )
    } else {
      # If image file is not found, display a placeholder image
      list(src = "www/placeholder.jpg", width = "65%")
    }
  }, deleteFile = FALSE)
  
  # Predict price using the trained model
  observeEvent(input$predict_button, {
    # Check if all inputs are valid
    if (validateInputs()) {
      # Prepare input data for prediction
      # Create new_data dataframe with proper data types and levels
      new_data <- data.frame(
        Year = as.integer(input$year),
        Age = as.integer(2024 - input$year),  # Corrected the calculation of Age
        Location = factor(input$location, levels = levels(train_data$Location)),
        Kilometers_Driven = as.integer(input$kilometer),
        Fuel_Type = factor(input$fuel, levels = levels(train_data$Fuel_Type)),
        Transmission = factor(input$transmission, levels = levels(train_data$Transmission)),
        Owner_Type = factor(input$owner, levels = levels(train_data$Owner_Type)),
        Mileage = as.numeric(input$mileage),
        Engine = as.integer(input$engine),
        Power = as.numeric(input$power),
        Seats = as.integer(input$seats),
        Brand = factor(input$brand, levels = levels(train_data$Brand))
      )
      
      # Make prediction using the loaded model
      predicted_price <- predict(mymodel, new_data)
      
      predicted_price <- round(predicted_price,2)
      # Format the predicted price with a range of +/- 2 Lacs
      formatted_price <- paste(predicted_price, "Lacs")
      
      
      # Return the formatted predicted price
      output$formatted_price <- renderText(formatted_price)
    }
  })
  
  # Function to validate input values
  validateInputs <- function() {
    if (input$year < 1998 || input$year > 2024) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Year must be between 1998 and 2024.",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    if (input$kilometer < 0 || input$kilometer > 100000) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Kilometer Driven must be between 0 and 100,000.",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    if (input$mileage < 5 || input$mileage > 30) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Mileage must be between 5 and 30.",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    if (input$engine < 600 || input$engine > 8000) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Engine must be between 600 and 8000 CC.",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    if (input$power < 50 || input$power > 2000) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Power must be between 200 and 2000 bhp.",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    if (input$seats < 2 || input$seats > 10) {
      showModal(modalDialog(
        title = "Invalid Input",
        "Number of Seats must be between 2 and 10.",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    return(TRUE)
  }
}

# Run the application 
shinyApp(ui = main_ui, server = server)

library(shiny)

# Welcome Page UI
welcome_ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        background-color: royalblue;
      }
      .container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        animation: fadeIn 1s ease-in-out;
      }
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      .welcome-box {
        background-color: #fff;
        padding: 40px;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        text-align: center;
      }
      h1 {
        color: royalblue;
        font-size: 36px;
        font-weight: bold;
        margin-bottom: 20px;
      }
      .btn-enter {
        background-color: royalblue;
        color: #fff;
        border: none;
        border-radius: 4px;
        padding: 10px 20px;
        font-size: 18px;
        cursor: pointer;
        transition: background-color 0.3s;
      }
      .btn-enter:hover {
        background-color: #4169e1; /* Darker shade of royalblue */
      }
    "))
  ),
  div(class = "container",
      div(class = "welcome-box",
          h1("Welcome to Car Price Prediction"),
          actionButton("enter_button", "Enter", class = "btn-enter")
      )
  )
)

# Define UI for application
ui <- function(request) {
  welcome_ui
}

# Run the application 
shinyApp(ui = ui, server = function(input, output, session) {})

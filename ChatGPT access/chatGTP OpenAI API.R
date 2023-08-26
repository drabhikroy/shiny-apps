library(shiny)
library(shinyBS)
library(shinyalert)
library(tidyverse)
library(openai)
library(bslib)
library(waiter)

waiting_screen <- tagList(
  spin_wobblebar(),
  h4("Reticulating splines...")
)

ui <- fluidPage(
  tags$style('.sweet-alert {background-color: #212121 !important;}'),
  tags$style('.sweet-alert p {color: #f2f2f2 !important; font-family: Roboto Condensed;}'),
  tags$style('.sweet-alert.alert-size-s {border-radius: 25px;}'),
  tags$style('h2 {color: #f2f2f2 !important;}'),
  tags$style('.sweet-alert button {background-color: #212121 !important; font-family: Roboto Condensed; font-weight: 900; box-shadow: #212121;}'),
 tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),

  useWaiter(),
  
  titlePanel("ChatGPT"),
  
  theme = bs_theme(bg = "#212121", 
                   fg = "#B8BCC2", 
                   primary = "#EA80FC",
                   base_font = font_google("Roboto Condensed"),
                   heading_font = font_google("Roboto Condensed")
                   ),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("temperature",
                  p("Sampling temperature", 
                    actionButton("info_temperature", 
                                 "",
                                 icon = tags$i(
                                   class = "fa-solid fa-circle-info", 
                                   style = "color: #80fcea;"
                                 ))),
                    min = 0.0, max = 1.0, value = 0.5, step = 0.1
                  ),
      
      sliderInput("num_tokens",
                  p("Maximum number of tokens", 
                    actionButton("info_num_tokens", 
                                 "", 
                                 icon = tags$i(
                                   class = "fa-solid fa-circle-info", 
                                   style = "color: #80fcea;"
                                 ))),
                    min = 10, max = 2000, value = 100, step = 10
                  ),
      
      selectInput("model", 
                  p("Select model", 
                    actionButton("info_model", 
                                 "", 
                                 icon = tags$i(
                                   class = "fa-solid fa-circle-info", 
                                   style = "color: #80fcea;"
                                 ))),
                  choices = c("text-curie-001",
                              "text-davinci-003",
                              "text-davinci-003",
                              "code-davinci-002",
                              "code-search-ada-text-001"),
                  selected = "text-curie-001"),
      
      passwordInput("api_key", 
                    p("Enter OpenAI API Key", 
                      actionButton("info_api", 
                                   "", 
                                   icon = tags$i(
                                     class = "fa-solid fa-circle-info", 
                                     style = "color: #80fcea;"
                                   )))
                    ),
      
    ),
    
    mainPanel(
      textAreaInput(
        inputId = "prompt", 
        label = "Enter your prompt below:",
        value = '',
        placeholder = "eg: Write a 100 word introductory paragraph on the reasons that Nickelback is the worst band ever to have existed.",
        height = "200px",
        resize = "both"),
      actionButton("submit", "Submit"),
      tags$br(),
      tags$br(),
      div(style = "padding: 10px; border-radius: 5px; max-width: 50%; background-color: #212121 !important; color: #f2f2f2",
          textOutput("api_response")
      ),
      tags$br(),
      downloadButton("downloadOut", label = "Download")
    )
  )
)


server <- function(input, output, session) {
 # bs_themer()
  observeEvent(input$info_temperature, {
    # Show a modal when the button is pressed
    shinyalert("Sampling temperature", "The model will look at all possible words and sample from them given their probability distribution to predict the next word. Higher values will make the output more random, while lower values will make it more focused and deterministic.", type = "info")
  })
  
  observeEvent(input$info_num_tokens, {
    # Show a modal when the button is pressed
    shinyalert("Maximum number of tokens", 
               paste0("Tokens are numeric representations of words, or more often parts of words which are used rather than letters because they can be processed more efficiently. There is a hard limit OpenAI provides for its basic accounts. Play around with the values and read more about tokens on OpenAI's ", 
      a("FAQ", 
        href = "https://openai.com/api/pricing/#faq", 
        target="_blank")
    ), 
    html = TRUE,
    size = "s",
    type = "info")
  })
  
  observeEvent(input$info_model, {
    # Show a modal when the button is pressed
    shinyalert("Model", 
               paste0("The OpenAI API consists of a family of models with different capabilities and pricing. The best wat to test them is to play around, but if you want to know more about each please click on the following to find out more about the ", 
                      a("text models", 
                        href = "https://platform.openai.com/docs/models", 
                        target="_blank"),
                      " and ",
                      a("coding models", 
                        href = "https://platform.openai.com/docs/models/codex", 
                        target="_blank")
               ), 
               html = TRUE,
               size = "s",
               type = "info")
  })
  
  observeEvent(input$info_api, {
    # Show a modal when the button is pressed
    shinyalert("OpenAI API Key",
               paste0("Get your own key by first ", 
                      a("signing up for free", 
                        href = "https://beta.openai.com/signup", 
                        target="_blank")
                      , " and then either ",
                      a("visiting your OpenAI key page", 
                        href = "https://beta.openai.com/account/api-keys", 
                        target="_blank"), 
                      " or click the menu item" ,
                      br(),
                      HTML("<i>View API keys</i>"),
                      br(),
                      br(),
                      "You can then view your existing keys or click on",
                      br(),
                      HTML("<i>Create new secret key</i>")), 
               html = TRUE,
               size = "s",
               type = "info")
  })
  
  observeEvent(input$submit, {
    if (input$api_key == "") {
      # If API key is not entered, display error message
      showModal(modalDialog(title = "Error", "Please enter your API key then submit."))
    } else {
      waiter_show(html = waiting_screen, color = "#212121")
      # Set up the API request
      rs <- openai::create_completion(model = input$model,
                                      prompt = input$prompt,
                                      temperature = input$temperature,
                                      max_tokens = input$num_tokens,
                                      openai_api_key = input$api_key)
      
      
      # Display the response to the user
      output$api_response <- renderText({
        rs$choices$text
      })
    
      waiter_hide()
    
    }
    
    # Allow the user to download text
    output$downloadOut <- downloadHandler(
      filename = function(){
        paste("data-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        writeLines(paste(output$api_response, collapse = ", "), file)
      }
    )

  }
    
  )
  
}

shinyApp(ui = ui, server = server)

# Adapted by Abhik Roy from https://github.com/machinatoonist/prompt-code/blob/main/Shiny-app/shiny-app-to-access-openai-api.R

# Under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License

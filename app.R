library(shiny)
library(gander)
library(tidyverse)
library(gtsummary)
library(gt)
library(plotly)
library(rtables) 
library(tern)
library(random.cdisc.data)
library(survival)
library(survminer)
library(bslib)


#setwd('C:/Users/user/Desktop/R')

addResourcePath("assets", "www")

ui <- fluidPage(theme = bs_theme(version = 5, bootswatch = "minty"),
                titlePanel("R-GPT: The LLM-Powered Shiny Sandbox"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    radioButtons('llm','Select LLM',choices=c('gemini-3.1-flash-lite','gemini-3.5-flash-lite','gemini-3.8-flash'),
                                 selected='gemini-3.5-flash-lite'),
                    br(),
                    
                    h5("Simulate random datasets:"),
                    numericInput('no','Number of subjects',10),
                    checkboxInput('adsl','ADSL dataset'),
                    checkboxInput('adae','ADAE dataset*'),
                    checkboxInput('adtte','ADTTE dataset*'),
                    tags$small(class = "text-muted", "*ADSL is required to be checked too"),
                    br(),br(),
                    h5("Or upload your own data:"),
                    fileInput("data_file", "Upload Data  .rds/.csv/.xpt/.sas7bdat", accept = c(".rds", ".csv", ".xpt", ".sas7bdat")),
                
                    textAreaInput('txt', 'Specify required analysis', 
                                  value = "enter request", 
                                  width = "100%", rows = 4),
                    actionButton('act', 'Generate & Render', class = "btn-primary"),
                    actionButton('undo', 'Undo', class = "btn-primary"),
                    
                    br(),br(),br(),br(),
                    tags$img(src = "assets/shiny2.jpg", style = "width: 80%; margin-bottom: 15px; opacity: 0.23"),
                  ),
                  
                  mainPanel(
                    h4("Generated R Code:"),
                    verbatimTextOutput('generated_code'),
                    hr(),
                    h4("Resulting Output:"),
                    uiOutput('dynamic_output')
                  )
                )
)




server <- function(input, output, session) {
  
  user_data_ <- reactive({
    if (is.null(input$data_file)) return(NULL)
    
    ext <- tolower(tools::file_ext(input$data_file$name))
    path <- input$data_file$datapath
    
    switch(ext,
           rds      = readRDS(path),
           csv      = read.csv(path),
           sas7bdat = haven::read_sas(path),
           xpt      = haven::read_xpt(path),
           NULL
    )
  })
  
  
  
  # Initialize a persistent reactive value to hold our live chat session
  v <- reactiveValues(chat_session = NULL)
  
  
  ask_llm <- function(msg) {
    if (is.null(v$chat_session)) {
      v$chat_session <- ellmer::chat_google_gemini(
        model = input$llm,
        system_prompt = "You are an R code generator. Return ONLY valid, executable R code that generates either a plot, a plotly chart, a table (gt, gtsummary, or rtables), or a standard console printout. DO NOT include markdown code blocks (```r), explanations, or comments. Just return the raw code expression. Always modify or build on top of your previous code responses if the user requests revisions.Don't use any external or simulated data or data provided in any r package. Assume that dataset required already exist. If no dataset is mentioned then assume that the dataset name is: user_data"
      )
    }
    raw <- v$chat_session$chat(msg)
    v$code <- trimws(gsub("```r|```", "", raw))
  }
  
  observeEvent(input$act, {
    req(input$txt)
    ask_llm(input$txt)
    updateTextAreaInput(session, 'txt', value = '')
  })
  
  observeEvent(input$undo, {
    req(v$chat_session)
    ask_llm("Undo your previous change and return the complete R code as it was before your last revision.")
  })
  
  ai_code <- reactive(v$code)
  
  
  
  output$generated_code <- renderText({
    ai_code()
  })
  
  
  adsl_=reactive({ 
    if(input$adsl) radsl(input$no,seed=155) 
    else NULL
  })
  
  adae_=reactive({ 
    if(isTRUE(input$adsl) && isTRUE(input$adae)) radae(adsl_()) 
    else NULL
  })
  
  adtte_=reactive({ 
    if(isTRUE(input$adsl) && isTRUE(input$adtte)) radtte(adsl_()) 
    else NULL
  })
  
  # 2. Evaluate the code once
  evaluated_result <- reactive({
    req(ai_code())
    
    adsl<<-adsl_()  #global environment
    adae<<-adae_()
    adtte<<-adtte_()
    
    user_data<<-user_data_()
    
    tryCatch({
      console_out <- capture.output({
        res_obj <- eval(parse(text = ai_code()))
      })
      list(obj = res_obj, text = console_out)
    }, error = function(e) {
      e 
    })
  })
  
  # 3. Dynamically build the correct UI component and render it
  output$dynamic_output <- renderUI({
    res <- evaluated_result()
    
    if (inherits(res, "error")) {
      return(verbatimTextOutput("error_display"))
    }
    
    if (inherits(res$obj, "gtsummary") || inherits(res$obj, "gt_tbl")) {
      gt::gt_output("rendered_table")
    } else if (inherits(res$obj, "plotly") || inherits(res$obj, "htmlwidget")) {
      plotly::plotlyOutput("rendered_plotly") 
    } else if (is.atomic(res$obj) || length(res$text) > 0 || is.data.frame(res$obj)) {
      # FIXED: Added is.atomic(res$obj) so raw numbers like 14 from nrow() route here!
      verbatimTextOutput("rendered_text")
    } else {
      plotOutput("rendered_plot") 
    }
  })
  
  # 4. Render the specific outputs based on what was generated
  output$rendered_table <- gt::render_gt({
    res <- evaluated_result()
    if (inherits(res$obj, "gtsummary")) {
      as_gt(res$obj) 
    } else {
      res$obj
    }
  })
  
  plot_object <- reactive({
    res <- evaluated_result()
    res$obj
  })
  
  output$rendered_plot <- renderPlot({
    res <- evaluated_result()
    if (inherits(res, "error")) {
      plot(1, type = "n", main = "Plotting Error")
    } else if (inherits(res$obj, c("ggplot", "ggsurvplot", "gg", "grob"))) {
      print(res$obj)
    }
  })
  
  output$rendered_plotly <- plotly::renderPlotly({
    res <- evaluated_result()
    res$obj 
  })
  
  # FIXED: Switched to renderPrint so R formats the rtables object layout beautifully
  output$rendered_text <- renderPrint({
    res <- evaluated_result()
    if (!is.null(res$obj) && (is.vector(res$obj) || is.data.frame(res$obj))) {
      print(res$obj)
    } else {
      cat(paste(res$text, collapse = "\n"))
    }
  })
  
  output$error_display <- renderText({
    paste("Error executing code:", evaluated_result()$message)
  })
}

shinyApp(ui, server)


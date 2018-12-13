server <- 
    function(input, output, session) {
        
        outVar <- reactive({
            vars <- all.vars(parse(text=input$inBody))
            vars <- as.list(vars)
            return(vars)
        })
        
        output$inBody <- renderUI({
            textInput(inputId = "inBody", label = h4("Enter a function:"), value = "a+b+c")
        })
        
        output$inVar <- renderUI({  ## works but the choices are non-reactive
            selectInput(inputId = "inVar", label = h4("Select variables:"), choices =  list("a","b"))
        })
        
        observe({  ## doesn't work
            choices <- outVar()
            updateSelectInput(session = session, inputId = "inVar", choices = choices)
        })
        
    }

ui <- (
    basicPage(
        uiOutput("inBody"),
        uiOutput("inVar")
    )
)


shinyApp(ui,server)

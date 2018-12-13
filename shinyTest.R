library(shiny)
library(data.table)
library(DT)


ui <- fluidPage(
    
    tags$head(tags$style(HTML("
            .selectize-input, 
            .selectize-dropdown {font-size: 75%;}
                              ")
    )),
    
    titlePanel("Portfolio View"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        
        column(2,
               selectInput("dt",
                           "Date:",
                           c(rev(as.character(unique(prices$Date)))))
        ),
        
        column(2,
               selectInput("by",
                           "SelectBy:",
                           c("All", "Cur", "Country", "GicsSector", "GicsGroup", "GicsSubGroup"))
        ),
        
        column(3,
               selectInput("select",
                           "Selection:",
                           c("All")
                           )
        )
        
    ),
    
    # Create a new row for the table.
    fluidRow(
        
        div(dataTableOutput("table"), style= "font-size:65%")
        
        ),
    
    fluidRow(
        
        div(dataTableOutput("total"), style= "font-size:65%")
        
    )
    
)


#choices= unlist(ifelse(x== "All", "All", unique(positions[, x, with= FALSE]))),

server <- function(input, output, session) {
    

    sel = reactive({
        
        if (input$by == "All") sel <- "All"
        else sel <- positions[, get(input$by)]
        
        #sel <- positions[, get(input$by)]
        unique(c("All", sel))

    })
    
    observe({
        
        updateSelectInput(session, "select",
                          label= input$by,
                          choices= sel()
        )
        
        
    })

    myData = reactive({
        
        if (input$select == "All") myData <- positions
        else myData <- positions[get(input$by) == input$select, ]
        
        myData <- myData[, .(Date, Ticker, Country,
                             GicsSector, GicsGroup, GicsSubGroup,
                             Position, Cur, Last, Bid, Cost,
                             Value, EurValue, Wght)]
        
    })
    
    myTot = reactive({
        
        myTot <- myData()
        myTot[Date == input$dt, .(round(sum(EurValue), 2),
                                  round(sum(Wght), 2))]
        
        
        
    })

    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        
        data <- myData()
        
        data <- data[Date == input$dt, -1, with= FALSE]
        
    },
    
    options= list(pageLength= 100,
                  
                  columnDefs= list(list(className= 'dt-right',
                                        targets= 6:12)
                                   )
                  )
    ) %>%
        formatCurrency(6,  currency= '', digits= 0) %>%
        formatCurrency(8,  currency= '', digits= 4) %>%
        formatCurrency(c(7,9:13), currency= '', digits= 2))
    
    output$total <- DT::renderDataTable(DT::datatable({
        
        data <- myTot()
        
        # data <- data[, -1, with= FALSE]
        
    }))

    
# },
# 
# options= list(pageLength= 1,
#               
#               columnDefs= list(list(className= 'dt-right',
#                                     targets= 6:12)
#               )
# )
# ) %>%
#     formatCurrency(6,  currency= '', digits= 0) %>%
#     formatCurrency(8,  currency= '', digits= 4) %>%
#     formatCurrency(c(7,9:12), currency= '', digits= 2))

}



shinyApp(ui,server)

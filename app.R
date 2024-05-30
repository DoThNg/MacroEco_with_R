#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Australian Macroeconomic Overview"),

    # Sidebar with a slider input for date of data 
    sidebarLayout(
        sidebarPanel(
          div(dateRangeInput(inputId = "dateRange",
                        label = "Select the dates:",
                        start = "1969-07",
                        end = format(Sys.Date(), format = "%Y-%m"),
                        startview = "year"
                        )
                      ), br(), br(),
          div("Note on data presented in this Dashboard:"), br(),
          div("- Monthly Bond Yield: Not Seasonally Adjusted, Source: Organization for Economic Co-operation and Development."),br(),
          div("- Monthly Interest Rate: Not Seasonally Adjusted, Source: Organization for Economic Co-operation and Development."), br(),
          div("- Annual Gov Gross Debt: Not Seasonally Adjusted, Source: International Monetary Fund."), br(),
          div("- Annual Stock Market Cap to GDP: Not Seasonally Adjusted, Source: World Bank."), br(),
          div("- Monthly Financial Market - Share Prices: Not Seasonally Adjusted, Source: Organization for Economic Co-operation and Development."), br(),
          div("- Quarterly Real Residential Property Prices: Not Seasonally Adjusted, Source: Bank for International Settlements."), br(),
          div("- Annual Inflation: Not Seasonally Adjusted, Source: World Bank."), br(),
          div("- Quarterly Consumer Price Indices: Growth rate same period previous year, Not Seasonally Adjusted, Source: Organization for Economic Co-operation and Development."), br(),
          div("- Monthly Unemployment Rates: Seasonally Adjusted, Source: Organization for Economic Co-operation and Development."), br(),
          div("- Quarterly Real Gross Domestic Product: Seasonally Adjusted, Source: International Monetary Fund."), br(),
          ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Bond Yield", plotOutput("distPlot1")),
            tabPanel("Interbank Rates", plotOutput("distPlot2")),
            tabPanel("Gov Gross Debt", plotOutput("distPlot3")),
            tabPanel("Stock Market Capitalization to GDP", plotOutput("distPlot4")),
            tabPanel("Share Prices", plotOutput("distPlot5")),
            tabPanel("Real Residential Property Prices", plotOutput("distPlot6"))
          ),
          tabsetPanel(
            tabPanel("Inflation", plotOutput("distPlot7")),
            tabPanel("Consumer Price Indices", plotOutput("distPlot8"))
          ),
          tabsetPanel(
            tabPanel("Unmployment Rate", plotOutput("distPlot9")),
            tabPanel("GDP", plotOutput("distPlot10"))
          ),
           
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

      # Interest Rates 
      symb1 = "IRLTLT01AUM156N"

      #  Inter-bank Rates (3-Month or 90 days rates) 
      symb2 = "IR3TIB01AUM156N"

      #  Gov Debt 
      symb3 = "GGGDTAAUA188N"

      #  Mkt Cap to GDP 
      symb4 = "DDDM01AUA156NWDB"

      #  Share Prices 
      symb5 = "SPASTT01AUM661N"
      
      #  Real Residential Property Prices 
      symb6 = "QAUR628BIS"
      
      #  Inflation 
      symb7 = "FPCPITOTLZGAUS"
      
      #  Consumer Price Indices (CPI)
      symb8 = "CPALTT01AUQ659N"

      # Unemployment Rate
      symb9 = "LRUNTTTTAUM156S"
      
      # Real Gross GDP
      symb10 = "NGDPRSAXDCAUQ"
      
      
      ## Obtain historical data for Interest Rate
      dataInput1 <- reactive({
        
        window(getSymbols(symb1, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })

      ## Obtain historical data for Inter-bank Rates
      dataInput2 <- reactive({
        
        window(getSymbols(symb2, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })
      
      ## Obtain historical data for Gov Debts
      dataInput3 <- reactive({
        
        window(getSymbols(symb3, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })

      ## Obtain historical data for Stock market Cap to GDP
      dataInput4 <- reactive({
        
        window(getSymbols(symb4, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })

      ## Obtain historical data for financial market: share prices
      dataInput5 <- reactive({
        
        window(getSymbols(symb5, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })

      ## Obtain historical data for Residential Property Prices
      dataInput6 <- reactive({
        
        window(getSymbols(symb6, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })
      
      ## Obtain historical data for Inflation
      dataInput7 <- reactive({
        
        window(getSymbols(symb7, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })      

      ## Obtain historical data for CPI
      dataInput8 <- reactive({
        
        window(getSymbols(symb8, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })    
      
      ## Obtain historical data for Unemployment Rates
      dataInput9 <- reactive({
        
        window(getSymbols(symb9, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })   

      ## Obtain historical data for Unemployment Rates
      dataInput10 <- reactive({
        
        window(getSymbols(symb10, src = "FRED", auto.assign = FALSE), start=input$dateRange[1], end=input$dateRange[2])
        
      })   
      
      
      ### Render Plots      
      output$distPlot1 <- renderPlot({
        
        chartSeries(dataInput1(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Monthly Interest Rates (%): Long-Term Government Bond Yields: 10-Year",
                    type = "line", 
                    TA = NULL)
      })

      output$distPlot2 <- renderPlot({
        
        chartSeries(dataInput2(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Monthly Interbank Rates (%): 3-Month or 90-Day Rates and Yields",
                    type = "line", 
                    TA = NULL)
      })

      output$distPlot3 <- renderPlot({
        
        chartSeries(dataInput3(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Annual General Gov Gross Debt (% of GDP)",
                    type = "line", 
                    TA = NULL)
      })

      output$distPlot4 <- renderPlot({
        
        chartSeries(dataInput4(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Annual Stock Market Capitalization to GDP (% of GDP)",
                    type = "line", 
                    TA = NULL)
      })
      
      output$distPlot5 <- renderPlot({      

      chartSeries(dataInput5(), 
                  theme = chartTheme("white", up.col='blue'),
                  name = "Financial Market: Monthly Share Prices (Index 2015=100)",
                  type = "line", 
                  TA = NULL)
      })
      
      output$distPlot6 <- renderPlot({      
        
        chartSeries(dataInput6(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Quarterly Real Residential Property Prices (Index 2010=100)",
                    type = "line", 
                    TA = NULL)
      })
      
      output$distPlot7 <- renderPlot({
        
        chartSeries(dataInput7(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Annual Inflation Rates (%)",
                    type = "line", 
                    TA = NULL)
      })

      output$distPlot8 <- renderPlot({
        
        chartSeries(dataInput8(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Quarterly CPI",
                    type = "line", 
                    TA = NULL)
      })        

      output$distPlot9 <- renderPlot({
        
        chartSeries(dataInput9(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Monthly Unemployment Rate (%)",
                    type = "line", 
                    TA = NULL)
      })

      output$distPlot10 <- renderPlot({
        
        chartSeries(dataInput10(), 
                    theme = chartTheme("white", up.col='blue'),
                    name = "Quarterly Real Gross GDP (Domesic Currency)",
                    type = "line", 
                    TA = NULL)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

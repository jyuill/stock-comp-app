#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(quantmod)
library(shinythemes)
library(bslib)
library(dygraphs)
library(PerformanceAnalytics)
library(here)

# Function to fetch price data for selected symbols
function(input, output, session) {
  ## get price data based on inputs for use elsewhere
  symData_all <- reactive({
    syms <- input$txtSym ## space-separated items
    sym_list <- str_split_1(syms, " ") ## splits the space-sep items into list
    ## empty data frame to hold results of loop
    symData_all <- NULL
    ## loop through to get data for each symbol
    for(symbs in sym_list){
      ## show list of all symbols and current symbol for reference
      cat(paste0("syms: ", syms, " symbs: ",symbs, "\n"))
      ## build data as loop cycles
      symData_all <- cbind(symData_all,
                           getSymbols(Symbols=symbs,
                                      from=input$dtRng[1], to=input$dtRng[2], auto.assign=FALSE, src='yahoo'))
    }
    ## return combined results of each loop (all symbols)
    symData_all
  })
  ## price chart - show data collected above to compare symbols
    output$priceChart <- renderDygraph({
      symData <- symData_all()
      if(input$mmnorm==FALSE){
        ## actual or normalized - depending on checkbox
        dygraph(Ad(symData)) %>% dyRangeSelector()
      } else {
        ## calc min-max normalized for better comp
        symData <- na.omit(symData)
        symData_mmn <- xts(apply(symData, 2, function(x) (x-min(x))/(max(x)-min(x))
                                 ),
                           order.by=index(symData))
        ## show dygraph with normalized data
        dygraph(Ad(symData_mmn)) %>% dyRangeSelector()
      }
    })
    
    ## correlation - adjusted prices
    output$pa_corr <- renderPlot({
      symData <- symData_all()
     # charts.PerformanceSummary(Cl(symData), main="Perf Summ",
      #                          geometric = FALSE, wealth.index=TRUE)
      chart.Correlation(Ad(symData))
    })
    
    output
}

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

## start server ####
function(input, output, session) {
  ## get price data ####
  ## get price data based on inputs for use elsewhere
  symData_all <- reactive({
    ## get symbols and dates from inputs
    syms <- input$txtSym ## space-separated items
    dt_start <- input$dtRng[1]
    dt_end <- input$dtRng[2] 
    ## for testing - set symbols and dates
    #syms <- "META AMZN AAPL NFLX GOOG"
    #dt_start <- '2020-01-01'
    #dt_end <- '2023-05-12'
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
                                      from=dt_start, to=dt_end, auto.assign=FALSE, src='yahoo'))
    }
    ## return combined results of each loop (all symbols)
    symData_all
  })
  ## price chart ####
  ## price chart - show data collected above to compare symbols
    output$priceChart <- renderDygraph({
      symData <- symData_all()
      ## > raw or normalized ####
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
    ## price correlations ####
    ## correlation - adjusted prices
    output$pa_corr <- renderPlot({
      symData <- symData_all()
     # charts.PerformanceSummary(Cl(symData), main="Perf Summ",
      #                          geometric = FALSE, wealth.index=TRUE)
      chart.Correlation(Ad(symData))
    })
    
    ## returns ####
    ## > calc returns ####
    ## monthly returns
    ## potentially simpler way to calc monthly returns
    ## calc only on adjusted prices - every 6th col
    symData_mth_ret <- reactive({
      data_all <- symData_all()
      ## get symbols and dates from inputs
      syms <- input$txtSym ## space-separated items
      sym_list <- str_split_1(syms, " ") ## split symbols into list
      symData_mth_ret <- NULL
      for(i in 1:length(sym_list)){
        cadj <- i*6
        sym_mr <- monthlyReturn(data_all[,cadj])
        colnames(sym_mr) <- sym_list[i]
        symData_mth_ret <- cbind(symData_mth_ret, sym_mr)
      }
      symData_mth_ret
    })
 
    ## > chart returns ####
    output$retChart <- renderDygraph({
      symData <- symData_mth_ret()
        dygraph(symData) %>% dyRangeSelector()
    })
    ## > correl returns ####
    output$mr_corr <- renderPlot({
      symData <- symData_mth_ret()
      chart.Correlation(symData)
    })
    
} ## end server ####

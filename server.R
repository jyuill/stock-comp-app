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

## functions - non-reactive
## individual dist
## - provide symbol to select (by number in list), list of symbols (from start), dataset of returns as data frame
fn_ind_rtn_dist <- function(sel, symbs, data){
  chart.Histogram(data[,sel],
                  methods=c("add.normal","add.risk"),
                  colorset = c('steelblue','','navyblue'),
                  main=symbs[sel])
}

## start server ####
function(input, output, session) {
  ## PRICES ####
  ## get price data ####
  ## get price data based on inputs for use elsewhere
  ## reactive for accessing symbol info
  sym_list <- reactive({
    str_split_1(input$txtSym, " ")
  })
  
  symData_all <- reactive({
    ## get symbols and dates from inputs
    sym_list <- sym_list()
    print(sym_list)
    dt_start <- input$dtRng[1]
    dt_end <- input$dtRng[2] 
    ## for testing - set symbols and dates
    #sym_list <- str_split_1("META AMZN GOOG", " ")
    #dt_start <- '2022-01-01'
    #dt_end <- '2023-05-12'
    ## empty data frame to hold results of loop
    symData_all <- NULL
    ## loop through to get data for each symbol
    for(symbs in sym_list){
      ## show list of all symbols and current symbol for reference
      cat(paste0("symbs: ", symbs, "\n"))
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
    
  
  
    ## RETURNS ####
    ## > calc returns ####
    ## monthly returns
    ## potentially simpler way to calc monthly returns
    ## calc only on adjusted prices - every 6th col
    symData_mth_ret <- reactive({
      data_all <- symData_all()
      ## for testing: uncomment, skip syms stmt, run (assuming symData_all avail.)
      #data_all <- symData_all
      ## get symbols and dates from inputs
      sym_list <- sym_list()
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
    
  ## > hist of returns ####
  output$mr_dist_hist <- renderPlot({
    symData_mth_ret <- symData_mth_ret()
    df_mth_ret <- as.data.frame(symData_mth_ret)
    df_mth_ret <- df_mth_ret %>% rownames_to_column(var='date')
    df_mth_ret_long <- df_mth_ret %>% pivot_longer(!date, names_to="asset", values_to="returns")
    summary_data <- df_mth_ret_long %>% group_by(asset) %>% summarize(
      ret_mean=mean(returns))
    df_mth_ret_long <- df_mth_ret_long %>% group_by(asset) %>% mutate(
      ret_mean=mean(returns),
      ret_med=median(returns),
      ret_pc25=quantile(returns, 0.25),
      ret_pc05=quantile(returns, 0.05)
    )
    ## thinking of breaking the set up for side-by-side charts
    ## abandoned just because of priorities
    assets <- unique(df_mth_ret_long$asset) ## get asset names
    n_assets <- length(assets) ## get # of assets and next line count first group
    n_assets_1 <- round(length(assets)/2)
    ## first dataset split
    df_mth_ret_long_a1 <- df_mth_ret_long %>% 
      filter(asset %in% assets[1:n_assets_1])
    ## remaining dataset split
    df_mth_ret_long_a2 <- df_mth_ret_long %>% 
      filter(asset %in% assets[n_assets-n_assets_1:n_assets])
    ## end experiment in splitting dataset 
    
    df_mth_ret_long %>% ggplot(aes(x=returns))+
      ## experimenting with calc for # of bins
      geom_histogram(fill='lightblue', bins = max(8,round(nrow(df_mth_ret)/4)))+
      facet_grid(asset~.)+
      geom_vline(aes(xintercept = ret_mean, group=asset), linetype='dotted', color='red', linewidth=0.8)+
      geom_vline(aes(xintercept = ret_med, group=asset), linetype='dotted', color='green', linewidth=0.8)+
      geom_vline(aes(xintercept = ret_pc05, group=asset), linetype='dotted', color='blue', linewidth=0.8)+
      geom_vline(aes(xintercept = 0), linetype='solid', color='black', linewidth=1)+
      theme_bw()
  }, width=400)
  # had 'height=200*(ncol(df_mth_ret)-1)' but apparently
  #  renderPlot can't access the data frame :(
  
   ## trying to use the function with chart.Histogram from performance analytics
    ## single histogram - works but need to dynamically generate multiple
    # output$mr_dist_hist <- renderPlot({
    #   ## get symbols and dates from inputs
    #   sym_list <- sym_list()
    #   ## get mth return data
    #   symData_mth_ret <- symData_mth_ret()
    #   ## use function to produce chart
    #   fn_ind_rtn_dist(1, sym_list, symData_mth_ret)
    # })
  
  # output$mr_dist_plots <- renderUI({
  #   sym_list <- sym_list()
  #   symData_mth_ret <- symData_mth_ret()
  #   ## this works with individual examples -> trick is to dynamically combine
  #   # output$mr_dist_hist1 <- renderPlot({
  #   #   ## use function to produce chart
  #   #   fn_ind_rtn_dist(1, sym_list, symData_mth_ret)
  #   # })
  #   # output$mr_dist_hist2 <- renderPlot({
  #   #   ## use function to produce chart
  #   #   fn_ind_rtn_dist(2, sym_list, symData_mth_ret)
  #   # })
  #   #   p1 <- plotOutput('mr_dist_hist1')
  #   #   print(p1)
  #   #   p2 <- plotOutput('mr_dist_hist2')
  #   #tagList(p1, p2) ## works to here - but not scalable
  #   
  ## tried to create loop with help from chatGPT - no luck so abandoned
  # })
  
  ## drawdowns ####
  # Shows how resilient investment is during negative return situations.
  ## - downward price movement relative to a high.
  ## plot.engine options: ggplot2, plotly, dygraph, googlevis, default
  output$drawdown <- renderDygraph({
    data <- symData_mth_ret()
    chart.Drawdown(data, geometric=TRUE, legend.loc='bottomleft', plot.engine="dygraph")
  })
  
  # Add a JavaScript snippet to adjust the height of the div containing the dygraph
  # - from chatGPT - doesn't appear to be doing anything
  output$adjust_dygraph_height <- renderUI({
    tags$head(tags$script(HTML(
      'var plot_height = $("#plot-container").height();
     $("#drawdown-container").css("margin-top", plot_height + 20);'
    )))
  })
  
  ## Upside/Downside Capture
  output$updown <- renderPlot({
    data <- symData_mth_ret()
    chart.CaptureRatios(Ra=data[,2:ncol(data)], Rb=data[,1], colorset="dodgerblue4",
                        main="Capture Ratio")
  })
  
  
  
  
  
} ## end server ####


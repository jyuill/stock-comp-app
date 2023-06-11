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
library(gt)
library(plotly)
library(htmlwidgets)

theme_set(theme_bw())
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
  
  ## Symbols ####
  ## get symbols for use as inputs
  ## reactive for accessing symbol info
  sym_list <- reactive({
    req(input$txtSym)
    str_split_1(input$txtSym, " ")
  })
  ## PRICES ####
  ## get price data ####
  symData_all <- reactive({
    #req(input$txtSym)
    ## get symbols and dates from inputs
    sym_list <- sym_list()
    print(sym_list)
    dt_start <- input$dtRng[1]
    dt_end <- input$dtRng[2] 
    ## for testing - set symbols and dates
    # sym_list <- str_split_1("META AMZN AAPL GOOG", " ")
    # dt_start <- '2022-01-01'
    # dt_end <- '2023-05-12'
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
    ## show total returns for ref
    cat("total return of first item:", (last(symData_all[,6])[[1]] - first(symData_all[,6])[[1]])/first(symData_all[,6])[[1]],"\n")
    
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
    ## price summary ####
    ## summarize: Hi, Low, Current (latest), current as % of range
    ## - probably need table because of range in data
    ## symData <- symData_all()
    #max(symData_all$META.Adjusted)
    #min(symData_all[,6])
    ## price correlations ####
    ## correlation - adjusted prices
    output$pa_corr <- renderPlot({
      symData <- symData_all()
     # charts.PerformanceSummary(Cl(symData), main="Perf Summ",
      #                          geometric = FALSE, wealth.index=TRUE)
      chart.Correlation(Ad(symData))
    })
    
    ## RETURNS ####
    ## Mthly returns ####
    ## > calc returns ####
    ## monthly returns - loop through each symbol
    ## calc only on adjusted prices - every 6th col
    symData_mth_ret <- reactive({
      data_all <- symData_all()
      ## get symbols and dates from inputs
      sym_list <- sym_list()
      ## for testing: uncomment, skip syms stmt, run (assuming symData_all avail.)
      #data_all <- symData_all
      #sym_list <- sym_list
      ## end test vars
      symData_mth_ret <- NULL
      for(i in 1:length(sym_list)){
        cadj <- i*6
        sym_mr <- monthlyReturn(data_all[,cadj])
        colnames(sym_mr) <- sym_list[i]
        symData_mth_ret <- cbind(symData_mth_ret, sym_mr)
      }
      cat("means:", colMeans(symData_mth_ret),"\n")
      cat("medians",apply(symData_mth_ret, MARGIN=2, FUN=median),"\n")
      symData_mth_ret
    })
 
  ## > calc summary of overall returns ####
  mth_ret_smry <- reactive({
    ## create table with stats in cols and assets in rows
    ## - cumulative returns, avg mthly, median mthly, 0.05 quantile
    ## get return data
    symData_mth_ret <- symData_mth_ret()
    ## for testing: start here
    symData_mth_ret <- symData_mth_ret
    
    ## cumulative return for period
    df_mth_ret_smry <- data.frame()
    ## calculates all at once - don't need loop
    df_ret_cum <- data.frame(Return.cumulative(symData_mth_ret))
    df_ret_cum_col <- df_ret_cum %>% pivot_longer(everything(), names_to='asset', values_to='cumulative_rtn')
    df_mth_ret_smry <- bind_rows(df_mth_ret_smry, df_ret_cum_col)
    
    ## get ave mthly ret
    ## need to loop through symData_mth_ret to calc for each col
    ## add cols for new metric to existing df 
    df_ret_stats <- data.frame(colMeans(symData_mth_ret)) %>% rownames_to_column('asset')
    colnames(df_ret_stats)[2] <- 'mean_rtn' 
    df_mth_ret_smry <- left_join(df_mth_ret_smry, df_ret_stats, by="asset")
    
    ## median - same as above and join
    df_ret_stats <- data.frame(apply(symData_mth_ret, MARGIN=2, FUN=median)) %>% rownames_to_column('asset')
    colnames(df_ret_stats)[2] <- 'median_rtn' 
    df_mth_ret_smry <- left_join(df_mth_ret_smry, df_ret_stats, by="asset")
    
    ## .05 quantile - same as above and join
    # Calculate the 5th percentile for each stock
    df_ret_stats <- data.frame(apply(symData_mth_ret, 2, function(x) quantile(x, probs = 0.05))) %>% rownames_to_column(('asset'))
    colnames(df_ret_stats)[2] <- 'percentile_5'
    df_mth_ret_smry <- left_join(df_mth_ret_smry, df_ret_stats, by="asset")
    
    return(df_mth_ret_smry)
    
  })
  ## summary table - using gt
  output$mth_smry_tbl <- render_gt({
    mth_ret_smry <- mth_ret_smry()
    tbl <- gt(mth_ret_smry)
    tbl |> fmt_percent(
      columns=everything(),
      decimals=1,
      use_seps = FALSE
    ) |>
      ## edit col names for readability
      cols_label(
        cumulative_rtn = "cumulative",
        mean_rtn = "mthly ave",
        median_rtn = "mthly median",
        percentile_5 = "mthly at risk*"
      )
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
    
  ## > mth rtn long with smry ####
  df_mth_ret_long <- reactive({
    cat("get mthly returns to convert to long df \n")
    symData_mth_ret <- symData_mth_ret()
    cat("convert to data frame \n")
    df_mth_ret <- data.frame(symData_mth_ret)
    df_mth_ret <- df_mth_ret %>% rownames_to_column("date")
    df_mth_ret_long <- df_mth_ret %>% pivot_longer(!date, names_to="asset", values_to="returns")
    ## uses mutate instead of summarize: data is left ungrouped and summary stats repeated
    ## maintains single df for histogram - use group=asset to display individual summary stats
    df_mth_ret_long <- df_mth_ret_long %>% group_by(asset) %>% mutate(
      ret_mean=mean(returns, na.rm=TRUE),
      ret_med=median(returns, na.rm=TRUE),
      ## what % are you ok with losing in a mth?
      ret_pc25=quantile(returns, 0.25, na.rm=TRUE), ## 25% of time returns this low
      ret_pc20=quantile(returns, 0.1, na.rm=TRUE), ## 10% of time returns this low
      ret_pc05=quantile(returns, 0.05, na.rm=TRUE) ## 5% of time returns this low
    )
    cat(str(df_mth_ret_long),"\n")
    df_mth_ret_long
  })
  
  ## > hist of returns ####
  output$mr_dist_hist <- renderPlotly({
    cat("get histogram \n")
    ## get return data in long format for hist from smry function
    df_mth_ret_long <- df_mth_ret_long()
    
    ## thinking of breaking the set up for side-by-side charts
    ## abandoned just because of priorities
    #assets <- unique(df_mth_ret_long$asset) ## get asset names
    #n_assets <- length(assets) ## get # of assets and next line count first group
    #n_assets_1 <- round(length(assets)/2)
    ## first dataset split
    #df_mth_ret_long_a1 <- df_mth_ret_long %>% 
    #  filter(asset %in% assets[1:n_assets_1])
    ## remaining dataset split
    #df_mth_ret_long_a2 <- df_mth_ret_long %>% 
    #  filter(asset %in% assets[n_assets-n_assets_1:n_assets])
    ## end experiment in splitting dataset 
    
    ## calc for number of bins
    assets <- unique(df_mth_ret_long$asset)
    avg_rows <- nrow(df_mth_ret_long)/length(assets)
    
    mth_hist <- df_mth_ret_long %>% ggplot(aes(x=returns))+
      ## experimenting with calc for # of bins
      geom_histogram(fill='lightblue', bins = max(8,round(avg_rows/4)))+
      scale_x_continuous(labels=percent_format())+
      facet_grid(asset~.)+
      geom_vline(aes(xintercept = ret_mean, group=asset), linetype='dotted', color='red', linewidth=1)+
      geom_vline(aes(xintercept = ret_med, group=asset), linetype='dotted', color='green', linewidth=1)+
      geom_vline(aes(xintercept = ret_pc05, group=asset), linetype='dotted', color='blue', linewidth=1)+
      geom_vline(aes(xintercept = 0), linetype='solid', color='black', linewidth=1)+
      theme_bw()
    
    ggplotly(mth_hist, tooltip='text', 
             scales=list(y=list(formatter="percent", accuracy=0.1)), width=600) #%>% layout(width=600)
  }) # width=400 taken out for plotly
  
  
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
  
  ## > drawdowns ####
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
  
## Annual returns ####  
  
  symData_yr_ret <- reactive({
    data_all <- symData_all()
    ## get symbols and dates from inputs
    sym_list <- sym_list()
    ## for testing: uncomment, skip syms stmt, run (assuming symData_all avail.)
    #data_all <- symData_all
    symData_yr_ret <- NULL
    for(i in 1:length(sym_list)){
      cadj <- i*6
      ## annual return calculation with built-in function
      sym_yr <- annualReturn(data_all[,cadj])
      colnames(sym_yr) <- sym_list[i]
      symData_yr_ret <- cbind(symData_yr_ret, sym_yr)
    }
    symData_yr_ret
  })
  
  ## > calc summary of overall returns ####
  yr_ret_smry <- reactive({
    ## create table with stats in cols and assets in rows
    ## - cumulative returns, avg mthly, median mthly, 0.05 quantile
    ## get return data
    symData_ret <- symData_yr_ret()
    ## for testing
    #symData_ret <- symData_yr_ret
    
    ## cumulative return for period
    df_ret_smry <- data.frame()
    ## calculates all at once - don't need loop
    df_ret_cum <- data.frame(Return.cumulative(symData_ret))
    df_ret_cum_col <- df_ret_cum %>% pivot_longer(everything(), names_to='asset', values_to='cumulative_rtn')
    df_ret_smry <- bind_rows(df_ret_smry, df_ret_cum_col)
    
    ## get ave ret
    ## need to loop through symData_ret to calc for each col
    ## add cols for new metric to existing df 
    df_ret_stats <- data.frame(colMeans(symData_ret)) %>% rownames_to_column('asset')
    colnames(df_ret_stats)[2] <- 'mean_rtn' 
    df_ret_smry <- left_join(df_ret_smry, df_ret_stats, by="asset")
    
    ## median - same as above and join
    df_ret_stats <- data.frame(apply(symData_ret, MARGIN=2, FUN=median)) %>% rownames_to_column('asset')
    colnames(df_ret_stats)[2] <- 'median_rtn' 
    df_ret_smry <- left_join(df_ret_smry, df_ret_stats, by="asset")
    
    ## .05 quantile - same as above and join
    # Calculate the 5th percentile for each stock
    df_ret_stats <- data.frame(apply(symData_ret, 2, function(x) quantile(x, probs = 0.05))) %>% rownames_to_column(('asset'))
    colnames(df_ret_stats)[2] <- 'percentile_5'
    df_ret_smry <- left_join(df_ret_smry, df_ret_stats, by="asset")
    
    ## set to period return to avoid confusion
    df_yr_ret_smry <- df_ret_smry
    return(df_yr_ret_smry)
    
  })
  ## summary table - using gt
  output$yr_smry_tbl <- render_gt({
    ret_smry <- yr_ret_smry()
    tbl <- gt(ret_smry)
    tbl |> fmt_percent(
      columns=everything(),
      decimals=1,
      use_seps = FALSE
    ) |>
      ## edit col names for readability
      cols_label(
        cumulative_rtn = "cumulative",
        mean_rtn = "annual ave",
        median_rtn = "annual median",
        percentile_5 = "annual at risk*"
      )
  })
  
  ## > chart returns ####
  ## used dygraph at first but confusing with lines and couldn't get
  ##  to only show end of yr on x-axis (now realize it is because of current yr data
  ##  point as of latest date)
  #output$retChart_yr <- renderDygraph({
    #symData <- symData_yr_ret()
    #dygraph(symData_a) %>% 
    #  dyAxis("x", axisLabelFormatter = JS("function(d, gran) { return d.getFullYear(); }"))
  #})
  
  output$retChart_yr <- renderPlotly({
    symData <- symData_yr_ret()
    df_symData <- data.frame(symData)
    df_symData <- df_symData %>% rownames_to_column("date")
    df_symData <- df_symData %>% pivot_longer(!date, names_to="asset", values_to="returns")
    #cat("pivoted: ", head(df_symData))
    #ar_plot <- df_symData %>% ggplot(aes(x=date, y=returns, fill=asset))+geom_col(position=position_dodge2())+
    #  scale_y_continuous(labels=percent_format())+
    #  labs(x="", y="annual return")
    # ar_plot <- df_symData %>% ggplot(aes(x=date, y=returns))+geom_col()+
    #   facet_wrap(.~asset)+
    #   scale_y_continuous(labels=percent_format())+
    #   labs(x="", y="annual return")
    ar_plot <- df_symData %>% ggplot(aes(x=date, y=returns))+geom_col()+
      facet_grid(.~asset)+
      scale_y_continuous(labels=percent_format())+
      geom_hline(yintercept=0)+
      labs(x="", y="")+
      theme_light()+
      theme(axis.text.x = element_text(angle=90),
            panel.grid = element_blank(),
            axis.ticks.x = element_blank())
    
    ggplotly(ar_plot)
  })
  
  ## > annual smry ####
  
  ## > correl returns ####
  output$yr_corr <- renderPlot({
    symData <- symData_yr_ret()
    chart.Correlation(symData)
  })
  
  
} ## end server ####


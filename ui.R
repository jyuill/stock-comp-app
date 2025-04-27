

library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(here)
library(quantmod)
library(shinythemes)
library(bslib)
library(dygraphs)
library(PerformanceAnalytics)
library(gt)
library(plotly)


# Define UI for application
fluidPage(
  ## head ####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    # Add a CSS rule for the dygraph container
    # - suggested by chatGPT - doesn't do anything
    tags$style(
      ".drawdown-container {
      border: 10px solid #ccc;
      padding: 50px;
      }"
  )
 ), ## end head ####
    
    # title ####
    h2("Stock Price Comparisons:",
       tags$span("Enter Symbols for Price, Return, and Related Comparisons",
                 style="font-size: 0.8em; color: darkgrey; font-weight: normal"),
               ) %>% tagAppendAttributes(class="title-panel-class"),
    # layout ####
    sidebarLayout(
      ## sidebar panel ####
        sidebarPanel(
            textInput(inputId='txtSym', label="Enter Symbol (sep with single space, no commas)", value="^GSPC AAPL GOOG KO") %>% tagAppendAttributes(class="symbol-box"),
            tags$div(
              tags$a(href="https://finance.yahoo.com/lookup/", "Stock symbol lookup", target="_blank"),
              style="font-size: 0.9em; padding-left: 4px;"),
            dateRangeInput(inputId='dtRng', label='Date Range', start='2020-01-01', end='2025-03-31' ) %>% tagAppendAttributes(class='date-box'),
            checkboxInput(inputId='mmnorm', label="Normalized price comparison?", value=FALSE),
            width=3
        ), ## end sidebar panel ####
        ## main panel ####
        mainPanel(
          ## tabs ####
          tabsetPanel(
            ## Prices ####
            tabPanel(title='Prices',
              tags$h3("Prices & Correlation"),
              tags$p("Prices for general context before looking at returns.",tags$em("(click 'Normalized?' at left for scaled comparison)")),
              tags$h4("Prices"),
              dygraphOutput("priceChart"),
              tags$h4("Price Correlations"),
              plotOutput("pa_corr")
            ), ## end Prices panel ####
            ## Mth Returns ####
            tabPanel(title='Mthly Returns',
              tags$h3("Monthly Returns"),
              tags$p("Prices may be interesting, but RETURNS are the game. All data based on ADJUSTED returns."),
              tags$h4("Monthly Return Comparison"),
              tags$p("Returns based on price at end of mth vs beginning."),
              dygraphOutput("retChart"),
              tags$h5("Cumulative Returns"),
              tags$p("Cumulative Return on $1 invested at start of period:"),
              dygraphOutput("cumChart"),
              tags$h4("Summary"),
              tags$p("Summary of mthly return data for the date range selected."),
              gt_output(outputId='mth_smry_tbl'),
              tags$p("* expected 'worst case' scenario for monthly drop, based on historical 5% chance of losing this much.") %>%
                tagAppendAttributes(class='small-note'),
              tags$h4("Return Correlations"),
              tags$p("Correlation of monthly returns over the period."),
              plotOutput("mr_corr"),
              #tags$h3("mr_dist_plots: where?"),
              #uiOutput(outputId="mr_dist_plots"),
              tags$h4("Distribution of monthly returns"),
              #plotOutput(outputId="mr_dist_hist"),
              plotlyOutput(outputId="mr_dist_hist"),
              tags$p("Red = mean, Green = median, Blue = 5th percentile"),
              tags$h4("Drawdowns"),
              dygraphOutput(outputId='drawdown'),
              tags$h4("Upside/downside capture"),
              tags$p("Shows the relative percentage of upside and downside captured 
              by an asset relative to a benchmark."),
              plotOutput(outputId='updown'),
              tags$p("Notes:",
                tags$ul(
                  tags$li("the benchmark (first item in list) will be situated where the x and y axes cross at 1. This is because it captures 100% of the upside and downside of itself."),
                  tags$li("other assets are then evaluated by how far and what direction they sit on both x and y axis from the benchmark."),
                  tags$li("downside that is lower than 1 means the asset tends to not to react with as much losses on the downside as the benchmark."),
                  tags$li("upside greater than 1 means the asset tends to see greater gains on the upside than the benchmark.")
                  )
                )
          ), ## > end Mthly Returns panel ####
          ## Yr returns ####
          tabPanel(title='Annual Returns',
                   tags$h3("Annual Returns"),
                   tags$p("Comparing returns over longer cycle. Based on calendar year."),
                   tags$h4("Annual Returns Comparison"),
                   #dygraphOutput("retChart_yr"),
                   plotlyOutput("retChart_yr"),
                   tags$h5("Cumulative Returns"),
                   tags$p("Cumulative Return on $1 invested at start of period:"),
                   ## consider REPLACE - x-axis shows mid-yr mths, confusing
                   dygraphOutput("cumChart_yr"),
                   ## NOT much better (if at all)
                   #plotlyOutput("cumChart_yr"),
                   tags$h4("Summary"),
                   tags$p("Summary of return data for the date range selected."),
                   gt_output(outputId='yr_smry_tbl'),
                   tags$p("* expected 'worst case' scenario for monthly drop, based on historical 5% chance of losing this much.") %>%
                     tagAppendAttributes(class='small-note'),
                   tags$h4("Return Correlations"),
                   tags$p("Correlation of annual returns over the period."),
                   plotOutput("yr_corr"),
                   ), ## > end 1 yr returns ####
          ## Rolling Ret ####
          tabPanel(title='Rolling Returns',
                   tags$h3("Rolling Returns Analysis"),
                   tags$h4("12 mth Rolling Returns"),
                   dygraphOutput("rolling_12"),
                   plotlyOutput("rolling_hist_12"),
                   tags$h4("24 mth Rolling Returns"),
                   dygraphOutput("rolling_24"),
                   plotlyOutput("rolling_hist_24"),
                   tags$h4("36 mth Rolling Returns"),
                   dygraphOutput("rolling_36"),
                   plotlyOutput("rolling_hist_36"),
                   tags$h4("60 mth Rolling Returns"),
                   dygraphOutput("rolling_60"),
                   plotlyOutput("rolling_hist_60"),
                   ), ## > end Rolling Ret ####
          tabPanel(title="About",
                   tags$h3("About this app"),
                   tags$p("This app is designed to allow users to compare stock prices and returns over a specified date range. 
                          It is provided for information purposes only, with no recommendations or guarantees of data accuracy."),
                   tags$p("The app uses the quantmod package to retrieve stock data from Yahoo Finance, and the PerformanceAnalytics package to calculate returns and drawdowns."),
                   tags$p("The app is built using the Shiny framework, which allows for interactive web applications in R."),
                   tags$p("This app is a work in progress, and additional features will be added in the future."),
                   tags$h3("Who did this?"),
                   tags$p(HTML("Created by <strong>John Yuill</strong>, 
                        a data analyst and data product developer based in Vancouver, BC, Canada."), 
                          tags$br("Reach me on", 
                                  tags$a(href="https://www.linkedin.com/in/johnyuill/", "LinkedIn.", class="non-tab"),
                                  " for more info, questions, comments."))
                  ) ## > end About panel ####
        ) ## end tabset panel ####
    ) ## end main panel ####
) ## end sidebar layout
) ## END fluidPage
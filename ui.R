

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


# Define UI for application that draws a histogram
fluidPage(
  ## head ####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Add a CSS rule for the dygraph container
    # - suggested by chatGPT - doesn't do anything
    tags$style(
      "#drawdown-container {
      border: 10px solid #ccc;
      padding: 50px;
      }"
  )
 ), ## end head ####
    
    # title ####
    titlePanel("Stock Price Comparisons"),

    # layout ####
    sidebarLayout(
      ## sidebar panel ####
        sidebarPanel(
            textInput(inputId='txtSym', label="Enter Symbol (sep with single space, no commas)", value="^GSPC AAPL GOOG KO"),
            tags$a(href="https://finance.yahoo.com/lookup/", "Stock symbol lookup"),
            dateRangeInput(inputId='dtRng', label='Date Range', start='2020-01-01', end='2023-05-01' ),
            checkboxInput(inputId='mmnorm', label="Normalized price comparison?", value=FALSE)
        ), ## end sidebar panel ####
        ## main panel ####
        mainPanel(
          ## tabs ####
          tabsetPanel(
            ## Prices ####
            tabPanel(title='Prices',
              tags$h2("Prices & Correlation"),
              tags$p(tags$em("(click 'Normalized?' at left for scaled comparison)")),
              tags$h3("Prices"),
              dygraphOutput("priceChart"),
              tags$h3("Price Correlations"),
              plotOutput("pa_corr")
            ), ## end Prices panel ####
            ## Mth Returns ####
            tabPanel(title='Mthly Returns',
              tags$p("Prices may be interesting, but RETURNS are the game. All data based on ADJUSTED returns."),
              tags$h3("Monthly Returns"),
              tags$p("Returns based on price at end of mth vs beginning."),
              dygraphOutput("retChart"),
              tags$h3("Summary"),
              gt_output(outputId='mth_smry_tbl'),
              tags$h3("Return Correlations"),
              tags$p("Correlation of monthly returns over the period."),
              plotOutput("mr_corr"),
              #tags$h3("mr_dist_plots: where?"),
              #uiOutput(outputId="mr_dist_plots"),
              tags$h3("Distribution of monthly returns"),
              #plotOutput(outputId="mr_dist_hist"),
              plotlyOutput(outputId="mr_dist_hist"),
              tags$p("Red = mean, Green = median, Blue = 5th percentile"),
              tags$h3("Drawdowns"),
              dygraphOutput(outputId='drawdown'),
              tags$h3("Upside/downside capture"),
              tags$p("Shows the relative percentage of upside and downside captured 
              by an asset relative to a benchmark."),
              plotOutput(outputId='updown'),
              tags$p("Couple things to note:",
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
                   tags$h2("Annual Returns"),
                   tags$h3("Annual Returns"),
                   dygraphOutput("retChart_yr"),
                   tags$h3("Return Correlations"),
                   tags$p("Correlation of monthly returns over the period."),
                   plotOutput("yr_corr"),
                   ), ## > end 1 yr returns ####
          ## Rolling Ret ####
          tabPanel(title='Rolling Returns',
                   tags$h2("Rolling Returns Analysis"),
                   tags$p("Coming soon!")
                   ) ## > end Rolling Ret ####
        ) ## end tabset panel ####
    ) ## end main panel ####
) ## end sidebar layout
) ## END fluidPage


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


# Define UI for application that draws a histogram
fluidPage(

    # title ####
    titlePanel("Stock Price Comparisons"),

    # layout ####
    sidebarLayout(
      ## sidebar panel ####
        sidebarPanel(
            textInput(inputId='txtSym', label="Enter Symbol (sep with single space, no commas)", value="^GSPC META AMZN AAPL NFLX GOOG"),
            tags$a(href="https://finance.yahoo.com/lookup/", "Stock symbol lookup"),
            dateRangeInput(inputId='dtRng', label='Date Range', start='2022-01-01', end='2022-12-31' ),
            checkboxInput(inputId='mmnorm', label="Normalized?", value=FALSE)
        ),
        ## main panel ####
        mainPanel(
          ## prices tab ####
          tabsetPanel(
            tabPanel(title='Prices',
              tags$h2("Prices & Correlation"),
              tags$p(tags$em("(click 'Normalized?' at left for scaled comparison)")),
              tags$h3("Prices"),
              dygraphOutput("priceChart"),
              tags$h3("Price Correlations"),
              plotOutput("pa_corr")
            ),
            tabPanel(title='Returns',
              tags$h2("Returns: The Main Thing"),
              tags$p("Prices may be interesting, but RETURNS are the game."),
              tags$h3("Monthly Returns"),
              dygraphOutput("retChart"),
              tags$h3("Return Correlations"),
              tags$p("Correlation of monthly returns over the period."),
              plotOutput("mr_corr")
            )
          )
        )
    )
)
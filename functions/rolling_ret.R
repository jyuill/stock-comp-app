## functions for shiny app

## rolling returns 
rolling_returns_mth <- function(data_d, n_roll = 12) {
  if(n_roll == 1) {
    return(data_d)
  } else {
    end_mth <- endpoints(data_d, on='months')
    data_m <- data_d[end_mth,]
    # get adjusted prices for each symbol -> every 6th col
    adj <- seq(6, ncol(data_m), 6)
    data_m <- data_m[,adj]
    
    return_price_method <- ROC(data_m, n = n_roll, type = "discrete")
    
    data_roll <- return_price_method*100
    data_roll <- na.omit(data_roll)
    data_roll
    
    return(data_roll)
  }
  
} # end rolling_returns_mth

rolling_plot <- function(data_roll, n_roll = 12, title = "", 
                         ylab = "Rolling Returns (%)", 
                         xlab = "Date") {
  # check data:
  #print(head(data_roll))
  # Create a dygraph plot
  dygraph(data_roll, main = title) %>%
    dyRangeSelector() %>%
    dyAxis("y", axisLabelFormatter = JS("function(d) { return (d).toFixed(0) + '%'; }")) %>%
    dyAxis("x", label = xlab) %>%
    #dyOptions(colors = RColorBrewer::brewer.pal(ncol(data_roll), "Set1")) %>%
    dyHighlight(highlightCircleSize = 3, highlightSeriesBackgroundAlpha = 0.3)
} # end rolling_plot)

rolling_hist <- function(data_roll, title = "Summary of Returns: Mean & Distribution", 
                         ylab = "# of periods", 
                         xlab = "Distribution of Returns") {
    df_data <- data.frame(data_roll)
    df_data <- df_data %>% rownames_to_column("date")
    df_data <- df_data %>% pivot_longer(!date, names_to="asset", values_to="returns")
    #print("pivoted: ", head(df_data))
    # calc summary stats for each asset
    df_data_smry <- df_data %>% group_by(asset) %>% 
      summarise(mean = mean(returns, na.rm=TRUE), 
                sd = sd(returns, na.rm=TRUE),
                median = median(returns, na.rm=TRUE),
                p5 = quantile(returns, 0.05, na.rm=TRUE),
                p95 = quantile(returns, 0.95, na.rm=TRUE))
    #print("summary: ", head(df_data_smry))
    # plot - ggplot
    roll_hist <- df_data %>% ggplot(aes(x=returns/100))+geom_histogram()+
      facet_wrap(.~asset)+
      geom_vline(xintercept=0, linetype='solid', color='black', linewidth=0.5)+
      geom_vline(data=df_data_smry, aes(xintercept=mean/100), 
                 linetype='solid', 
                 color='red', linewidth=0.5)+
      scale_x_continuous(labels=percent_format())+
      labs(title=title, x=xlab, y=ylab)+
      theme(panel.grid.minor = element_blank())
    # plotly - with formatting
      ggplotly(roll_hist) %>%
        layout(title = list(text = title,
                            font = list(size = 16),
                            x = 0.5, # 0 = left, 0.5 = center, 1 = right
                            xanchor = "center",
                            y = 0.96, # adjust y position of title
                            yanchor = ""),
               # ovoiding this because centers under left facet only
               # xaxis = list(
               #   title = list(text =  xlab,
               #                standoff = 10,
               #                font = list(size = 14)
               #                ),
               #   tickfont = list(size = 10),
               #   automargin = TRUE
               #   #tickangle = -45,
               #   #showgrid = TRUE,
               #   #zeroline = TRUE,
               #   #zerolinecolor = "black",
               #   #zerolinewidth = 1
               # ),
               margin = list(l=60, r=50, t=60, b=50)) %>%
        config(displayModeBar = TRUE)
} # end rolling_hist

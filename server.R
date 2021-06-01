setwd("/Users/rogercusco/Documents/ShinyMobileGaming")

library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)

source("BusinessModel.R")
                    
server <- function(input, output) {

  x <- seq(-10, 10)
  y <- rnorm(length(x))
  
  output$p <- renderPlotly({
    
    # https://community.rstudio.com/t/sliding-a-point-on-a-plot-rather-than-sliderinput-to-update-plot-in-shiny/16405/2
    
    d <- event_data("plotly_relayout", source = "trajectory")
    
    selected_point <- if (!is.null(d[["shapes[0].x0"]])) {
      xint <- d[["shapes[0].x0"]]
      xpt <- x[which.min(abs(x - xint))]
      list(x = xpt, y = y[which(x == xpt)])
    } else {
      list(x = 1, y = y[which(x == 1)])
    }
    
    plot_ly(color = I("red"), source = "trajectory") %>%
      add_lines(x = x, y = y) %>%
      add_markers(x = selected_point$x, y = selected_point$y) %>%
      layout(
        shapes = list(
          type = "line", 
          line = list(color = "gray", dash = "dot"),
          x0 = selected_point$x, 
          x1 = selected_point$x,
          y0 = 0,
          y1 = 1,
          yref = "paper"
        )
      ) %>%
      config(editable = TRUE) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE)) 
  })
  output$event <- renderPrint({
    event_data("plotly_relayout", source = "trajectory")
  })
  
  df<- data.frame(c1=c(50,20,10,5,3,1))
  colnames(df) <- c("% Retention")
  rownames(df) <- c("D1", "D7", "D30", "D180", "D365", "D730")
  values <- reactiveValues(data = df)
  
  # observe({
  #   if(!is.null(input$hot)){
  #     values$data <- as.data.frame(hot_to_r(input$hot))
  #     isolate(values$data[,'diff'] <- ifelse(values$data[,'select'], values$data[,'c1']-values$data[,'c2'] ,0))
  #     print(values$data)
  #     output$hot <- renderRHandsontable({
  #       rhandsontable(values$data)
  #     })
  #   }
  # })    
  
  output$hot <- renderRHandsontable({
    rhandsontable(values$data) %>%
      hot_cols(colWidths=70)
  })
  
  observeEvent(
    input$hot$changes$changes,{
    values$data <- hot_to_r(input$hot)
  })

  output$retPlot <- renderPlot({
    par(mar=c(4,3,0,0))
    if(is.null(input$hot)) return(NULL)
    if(sum(unlist(lapply(unlist(input$hot$params$data), is.character)))>0) return(NULL)
    ret <- get_retention_curve(hot_to_r(input$hot)[,1], retention_weights)
    #plot(values$data)
    plot(ret, type="l", xlab="Days since install", ylab="Retention %")
  })
  
  df_ltv<- data.frame(c1=c(0.2,0.3,0.4,0.5,0.6,0.7))
  colnames(df_ltv) <- c("LTV ($)")
  rownames(df_ltv) <- c("D1", "D7", "D30", "D180", "D365", "D730")
  values_ltv <- reactiveValues(data = df_ltv)
  
  output$hot_ltv <- renderRHandsontable({
    rhandsontable(values_ltv$data) %>%
      hot_cols(colWidths=70)
  })
  
  observeEvent(
    input$hot_ltv$changes$changes,{
      values$data <- hot_to_r(input$hot_ltv)
    })
  
  output$ltvPlot <- renderPlot({

    if(is.null(input$hot_ltv)) return(NULL)
    if(sum(unlist(lapply(unlist(input$hot_ltv$params$data), is.character)))>0) return(NULL)
    ltv <- get_ltv_curve(hot_to_r(input$hot_ltv)[,1], ltv_weights)
    par(mar=c(4,3,0,0))
    plot(ltv, type="l", xlab="Days since install", ylab="Dollars")
   
  })
  
  ##### OUTPUT BASIC MODEL #####
  
  vals <- reactiveValues()
  observe({
    if(is.na(input$cpi_inpu)) return(NULL)
    if(is.null(input$hot_ltv)) return(NULL)
    if(is.null(input$hot)) return(NULL)
    if(sum(unlist(lapply(unlist(input$hot_ltv$params$data), is.character)))>0) return(NULL)
    if(sum(unlist(lapply(unlist(input$hot$params$data), is.character)))>0) return(NULL)
    if(is.na(input$mkg_inv)) return(NULL)
    if(is.na(input$opex_m)) return(NULL)
    if(is.na(input$init_capital)) return(NULL)
    vals$yearly_mkg <- from_quarterly_to_yearly(time_converter(input$mkg_inv)$quarterly)
    vals$yearly_opex <- from_quarterly_to_yearly(time_converter(input$opex_m)$quarterly)
    vals$daily_mkg <- input$mkg_inv / 30
    vals$installs <- apply_kfactor(input$kfactor, acquire_ngu(730, vals$daily_mkg, 14, 14/max(input$cpi_inpu,0.01)))
    vals$ltv <- get_ltv_curve(hot_to_r(input$hot_ltv)[,1], ltv_weights)
    vals$ret_curve <- get_retention_curve(hot_to_r(input$hot)[,1], retention_weights)
    vals$daus <- get_dau_r(vals$installs, vals$ret_curve)
    vals$daily_rev <- colSums(get_revenue(vals$installs,vals$ltv))
    vals$yearly_rev <- from_quarterly_to_yearly( from_daily_to_quarterly(vals$daily_rev) )
    vals$ebitda <- paste("$", format(vals$yearly_rev - vals$yearly_mkg - vals$yearly_opex, big.mark=",", scientific=FALSE))
    vals$revenue <- paste("$", format(vals$yearly_rev,  big.mark=",", scientific=FALSE))
    vals$marketing <- paste("$", format(vals$yearly_mkg,  big.mark=",", scientific=FALSE))
    vals$opex <- paste("$", format(vals$yearly_opex,  big.mark=",", scientific=FALSE))
    
    vals$mkg_monthly <- time_converter(input$mkg_inv)$monthly
    vals$opex_monthly <- time_converter(input$opex_m)$monthly
    vals$rev_monthly <- from_daily_to_monthly(vals$daily_rev)
    vals$ebitda_monthly <- vals$rev_monthly - vals$mkg_monthly - vals$opex_monthly
    vals$cash_flow <- get_monthly_cashflow( input$init_capital, 
                                               vals$rev_monthly, 
                                               vals$mkg_monthly,
                                               vals$opex_monthly)
    
    vals$capital <- get_cash_info(vals$cash_flow)$capital
    vals$months <- get_cash_info(vals$cash_flow)$months
  })
  
  output$PLTable <- renderPlotly({

    m <- list(
      l = 50,
      r = 50,
      b = 10,
      t = 10,
      pad = 4
    )
    
    plot_ly(
      type = 'table',
      columnwidth = c(20, 20, 20),
      columnorder = c(0, 1, 2),
      header = list(
        values = c("","Year 1", "Year 2"),
        align = c("left", "center", "center"),
        line = list(width = 1, color = 'black'),
        fill = list(color = c("gray", "gray", "gray")),
        font = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        values = t(cbind(c("Revenue", "Marketing", "OPEX", "EBITDA"), 
                         rbind(vals$revenue, vals$marketing, vals$opex, vals$ebitda))),
        align = c("left", "center", "center"),
        height=30,
        line = list(color = "black", width = 1),
        font = list(family = "Source Sans Pro", size = 14, color = c("black")))) %>%
       layout(margin= m)
  })
  
  output$capital <- renderText({ 
    
    # cash_balance <- get_monthly_cashflow(0, from_daily_to_monthly(colSums(r))*7, time_converter(20000)$monthly,
    #                                      time_converter(3000)$monthly)
    if(vals$capital > 0 ) {
    paste("You will need to raise at least:", paste("$", format(vals$capital, big.mark=",", scientific=FALSE)), "to sustain the business for the next 2 years.")
    } else {
      print("You don't need to raise any money.")
    }
  })
  
  output$months <- renderText({ 
    if(is.na(vals$months)) {
      NULL
    } else {
      paste("You will ran out of money in", vals$months, "months.")
    }
  })
  
  output$cashPlot <- renderPlotly({
    
    x <- 1:24
    y1 <- vals$cash_flow
    y2 <- vals$ebitda_monthly
    data <- data.frame(x, y1, y2)
    
    plot_ly(data, x=~x, y=~y1, name = "Cash balance", type = "bar") %>% 
      add_trace(y = ~y2, name="Cash-flow") %>%     
      layout(title = "",
             xaxis = list(title = "Months since game launch"),
             yaxis = list(title = "USD"),
             barmode = 'group',
             bargap = 0.3)  %>%
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE)) 
  })
  
  output$dausPlot <- renderPlotly({
    x <- 1:730
    y2 <- colSums(vals$daus)
    data <- data.frame(x, y2)
    
    plot_ly(data, x = ~x, y = ~y2, name="Daily Active Users", type = 'scatter', mode = 'lines') %>%
      layout(title="Daily Active Users", xaxis = list(title = "Days since launch"), font=list(size=11),
             yaxis = list(title = ""))
  })
  
  output$nguPlot <- renderPlotly({
    x <- 1:730
    y1 <- vals$installs
    data <- data.frame(x, y1)
    
    fig3 <- plot_ly(data, x = ~x, y = ~y1, name="Downloads", type = 'scatter', mode = 'lines') %>%
      layout(title="Downloads", xaxis = list(title = "Days since launch"), font=list(size=11), 
             yaxis = list(range=c(0, round(max(y1)*1.5)), title = "")) 
    
  })
  # x <- 1:730
  # y1 <- inst
  # y2 <- colSums(get_dau_r(inst, prov))
  # data <- data.frame(x, y1, y2)
  # 
  # fig3 <- plot_ly(data, x = ~x, y = ~y2, name="Daily Active Users", type = 'scatter', mode = 'lines') %>%
  #   layout(title="Daily Active Users", xaxis = list(title = "Days since game launch"), yaxis = list(title = ""))
  # 
  # fig3
  
  
  # x <- 1:730
  # y1 <- inst
  # y2 <- colSums(get_dau_r(inst, prov))
  # data <- data.frame(x, y1, y2)
  # 
  # fig3 <- plot_ly(data, x = ~x, y = ~y1, name="Downloads", type = 'scatter', mode = 'lines') %>%
  #   layout(title="Downloads", xaxis = list(title = "Days since game launch"), yaxis = list(range=c(0, round(max(y1)*1.5)), title = ""))
  # fig3  
  # 
}

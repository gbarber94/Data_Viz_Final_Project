library(shiny)
library(shinyWidgets)
library(tidyverse)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(rlang)
library(DT)

shinyServer(function(input, output, session) {
  
  edu <- read.csv("states.csv")
  
  edu$code <- state.abb[match(tolower(edu$STATE),tolower(state.name))]
  
  edu2 <- edu
  
  edu3 <- edu2
  
  edu3 <- edu3 %>% select(Math.Score.Grade.4,Math.Score.Grade.8,Reading.Score.Grade.4,Reading.Score.Grade.8,YEAR,STATE, code)
  
  
  edu2 <- edu2 %>% select(Total.Expenditure,	Instruction.Expenditure,	Support.Services.Expenditure,Other.Expenditure,Capital.Outlay.Expenditure, GRADES_ALL_G,YEAR,STATE,code)
  
  #Tab 1 input
  updateSelectInput(session,"score",choices=colnames(edu3[1:4]))
  
  updateSelectInput(session,"funding",choices=colnames(edu2[1:5]))
  
  #Tab 1
  output$map <- renderPlotly({
    
    year <- req(input$year)
    score <- req(input$score)
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE,
      lakecolor = toRGB('white')
    )
    
    edu_plot <- filter(edu3, YEAR == year)
    
    edu_plot$value <- edu_plot[,score]
    
    #edu_plot$h1 <- as.data.vector(select(edu_plot,score))
    
    #print(head(edu_plot))
    
   edu_plot$hover <- with(edu_plot, paste("State:",str_to_title(tolower(STATE)), '<br>', "Average Test Score:", round(value)))
    
    p <- plot_geo(edu_plot, locationmode = 'USA-states') %>%
      add_trace(z = ~get(score), locations = ~code, text = ~hover, hoverinfo = 'text',
                color = ~get(score), colors = 'Spectral' , alpha=0.7, reversescale = TRUE
      ) %>%
      colorbar(title = "Test Score", y = .75, x = .8) %>%
      layout(title = paste0(edu_plot$YEAR, " Average ", score),
             geo = g
      )%>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    return(p)
  })
  
  output$map_fund <- renderPlotly({
    
    year <- req(input$year)
    funding <- req(input$funding)
    percap <- input$per_cap4
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE,
      lakecolor = toRGB('white')
    )
    
    if (percap == FALSE){
      edu4 <- edu2
    }
    
    else {
      edu4 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    edu_plot <- filter(edu4, YEAR == year)
    
    edu_plot$value <- edu_plot[,funding]
    
    #edu_plot$h1 <- as.data.vector(select(edu_plot,score))
    
    #print(head(edu_plot))
    
    edu_plot$hover <- with(edu_plot, paste("State:",str_to_title(tolower(STATE)), '<br>', "Expenditure: $", prettyNum(round(value,2),big.mark=",",scientific=FALSE)))
    
    p <- plot_geo(edu_plot, locationmode = 'USA-states') %>%
      add_trace(z = ~get(funding), locations = ~code, text = ~hover, hoverinfo = 'text',
                color = ~get(funding), colors = 'Spectral' , alpha=0.7, reversescale = TRUE
      ) %>%
      colorbar(title = "Expenditure (dollars)", y = .75, x = .8) %>%
      layout(title = paste0(edu_plot$YEAR, " ", funding),
             geo = g
      )%>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    return(p)
  })
  
  
  #Tab 2 input
  updateSelectInput(session,"states",choices=unique(edu2$STATE), selected = "FLORIDA")
  
  updateSelectInput(session,"score1",choices=colnames(edu3[1:4]))
  
  updateSelectInput(session,"funding1",choices=colnames(edu2[1:5]))
  
  output$exp_line <- renderPlotly({
    
    states <- req(input$states)
    expend <- req(input$funding1)
    percap <- input$per_cap
    
    print(percap)
    
    if (percap == FALSE){
      edu4 <- edu2
    }
    
    else {
      edu4 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    edu4 <- edu4 %>% filter(STATE %in% states)

    edu4$value <- edu4[,expend]
    
    p <- plot_ly(edu4,x=~YEAR, y=~get(expend), type="scatter",color=~STATE, mode="lines+markers",
                 hovertext = paste("<b>State:<b>",edu4$STATE,"<br>Year:",edu4$YEAR,"<br>Expenditure $",prettyNum(round(edu4$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"), yaxis = list(title = paste0(expend," (dollars)"))) %>%
      layout(title = paste0(expend)) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })

  output$score_line <- renderPlotly({
    
    score <- req(input$score1)
    states <- req(input$states)
    expend <- req(input$funding1)
    
    
    edu5 <- edu3
    edu5 <- edu5 %>% filter(STATE %in% states)
    edu5$value <- edu5[,score]
    edu5 <- edu5 %>% filter(!is.na(edu5[,score]))
    
    p <- plot_ly(edu5,x=~YEAR, y=~get(score), type="scatter",color=~STATE,mode="lines+markers",
                 hovertext = paste("<b>State:<b>",edu5$STATE,"<br>Year:",edu5$YEAR,"<br>Avereage Score",round(edu5$value)), hoverinfo = 'text') %>%
      layout(xaxis = list(title = "Year"), yaxis = list(title = score)) %>%
      layout(title = paste0(score)) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
  
#Tab 3 Top Ten
  
  
  updateSelectInput(session,"score2",choices=colnames(edu3[1:4]))
  
  updateSelectInput(session,"funding2",choices=colnames(edu2[1:5]))
  
  output$expen_ten <- renderPlotly({
    
    
    year <- req(input$year2)
    expend <- req(input$funding2)
    percap <- input$per_cap2
    
    if (percap == FALSE){
      edu6 <- edu2
    }
    
    else {
      edu6 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    
    #edu6 <- edu6[order(expend,STATE)] %>% drop_na()
    
    edu6 <- edu6 %>% filter(YEAR == year)
    
    edu6 <- select(edu6,c("STATE",expend)) %>% drop_na() %>%
      arrange(desc(!! rlang::sym(expend)))
     
    
    top_ten <- head(edu6,10) %>% droplevels()
    
    top_ten$value <- top_ten[,expend]
    
    col_names     <- paste0(top_ten$STATE)
    
    p <- plot_ly(top_ten,x=factor(col_names, levels=col_names), y=~get(expend), type="bar",color=~STATE, hovertext = paste("<b>State:<b>",top_ten$STATE,"<br>Expenditure: $",prettyNum(round(top_ten$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(xaxis = list(title = "State"), yaxis = list(title = expend)) %>%
      layout(title = paste0(expend)) %>%
      layout(showlegend = FALSE) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
  output$expen_ten_low <- renderPlotly({
    
    
    year <- req(input$year2)
    expend <- req(input$funding2)
    percap <- input$per_cap2
    
    if (percap == FALSE){
      edu6 <- edu2
    }
    
    else {
      edu6 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    
    #edu6 <- edu6[order(expend,STATE)] %>% drop_na()
    
    edu6 <- edu6 %>% filter(YEAR == year)
    
    edu6 <- select(edu6,c("STATE",expend)) %>% drop_na() %>%
      arrange(desc(!! rlang::sym(expend)))
    
    
    top_ten <- tail(edu6,10) %>% droplevels()
    
    top_ten$value <- top_ten[,expend]
    
    col_names     <- paste0(top_ten$STATE)
    
    p <- plot_ly(top_ten,x=factor(col_names, levels=col_names), y=~get(expend), type="bar",color=~STATE, hovertext = paste("<b>State:<b>",top_ten$STATE,"<br>Expenditure: $",prettyNum(round(top_ten$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(xaxis = list(title = "State"), yaxis = list(title = expend)) %>%
      layout(title = paste0(expend)) %>%
      layout(showlegend = FALSE) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })

  output$score_ten <- renderPlotly({
    
    
    year <- req(input$year2)
    score <- req(input$score2)
    
    edu6 <- edu3
    
    edu6 <- edu6 %>% filter(YEAR == year)
    
    edu6 <- select(edu6,c("STATE",score)) %>% drop_na() %>%
      arrange(desc(!! rlang::sym(score)))
    
    
    top_ten <- head(edu6,10) %>% droplevels()
    
    top_ten$value <- top_ten[,score]
    
    col_names     <- paste0(top_ten$STATE)
    
    p <- plot_ly(top_ten,x=factor(col_names, levels=col_names), y=~get(score), type="bar",color=~STATE, hovertext = paste("<b>State:<b>",top_ten$STATE,"<br>Expenditure: $",prettyNum(round(top_ten$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(xaxis = list(title = "State"), yaxis = list(title = score)) %>%
      layout(title = paste0(score)) %>%
      layout(showlegend = FALSE) %>%
      layout(yaxis = list(range = c(min(top_ten$value)-5, max(top_ten$value)+5))) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
  output$score_ten_low <- renderPlotly({
    
    
    year <- req(input$year2)
    score <- req(input$score2)
    
    edu6 <- edu3
    
    edu6 <- edu6 %>% filter(YEAR == year)
    
    edu6 <- select(edu6,c("STATE",score)) %>% drop_na() %>%
      arrange(desc(!! rlang::sym(score)))
    
    
    top_ten <- tail(edu6,10) %>% droplevels()
    
    top_ten$value <- top_ten[,score]
    
    col_names     <- paste0(top_ten$STATE)
    
    p <- plot_ly(top_ten,x=factor(col_names, levels=col_names), y=~get(score), type="bar",color=~STATE, hovertext = paste("<b>State:<b>",top_ten$STATE,"<br>Expenditure: $",prettyNum(round(top_ten$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(xaxis = list(title = "State"), yaxis = list(title = score)) %>%
      layout(title = paste0(score)) %>%
      layout(showlegend = FALSE) %>%
      layout(yaxis = list(range = c(min(top_ten$value)-5, max(top_ten$value)+5))) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
# Tab 4 Ploting Score vs Expenditure
  
  updatePickerInput(session,"states3",choices= as.character(unique(edu2$STATE)), selected = as.character(unique(edu2$STATE)))
  
  updateSelectInput(session,"funding3",choices=colnames(edu2[1:5]))
  
  output$math_4th <- renderPlotly({
    
    states <- req(input$states3)
    expend <- req(input$funding3)
    score <- "Math.Score.Grade.4"
    percap <- input$per_cap3
    
    if (percap == FALSE){
      edu4 <- edu2
    }
    
    else {
      edu4 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    edu4 <- edu4 %>% filter(STATE %in% states)
    
    edu5 <- edu3 %>% filter(STATE %in% states)
    
    edu4$value <- edu4[,expend]
    edu4$scor <- edu5[,score]
    
    edu4 <- edu4 %>% filter(!is.na(edu4$scor)) %>% arrange(value)
    
    
    p <- plot_ly(edu4,x=~value, y=~scor, type="scatter",color=~YEAR, mode="markers",
                 hovertext = paste("<b>State:<b>",edu4$STATE, "<br>Year:<b>",edu4$YEAR,"<br>Score:",round(edu4$scor),"<br>Expenditure $",prettyNum(round(edu4$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(yaxis = list(title = score), xaxis = list(title = paste0(expend," (dollars)"))) %>%
      layout(title = paste0(expend," v. ",score)) %>%
      hide_colorbar() %>% 
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
  output$math_8th <- renderPlotly({
    
    states <- req(input$states3)
    expend <- req(input$funding3)
    score <- "Math.Score.Grade.8"
    percap <- input$per_cap3
    
    if (percap == FALSE){
      edu4 <- edu2
    }
    
    else {
      edu4 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    edu4 <- edu4 %>% filter(STATE %in% states)
    
    edu5 <- edu3 %>% filter(STATE %in% states)
    
    edu4$value <- edu4[,expend]
    edu4$scor <- edu5[,score]
    
    edu4 <- edu4 %>% filter(!is.na(edu4$scor)) %>% arrange(value)
    
    
    p <- plot_ly(edu4,x=~value, y=~scor, type="scatter",color=~YEAR, mode="markers",
                 hovertext = paste("<b>State:<b>",edu4$STATE, "<br>Year:<b>",edu4$YEAR,"<br>Score:",round(edu4$scor),"<br>Expenditure $",prettyNum(round(edu4$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(yaxis = list(title = score), xaxis = list(title = paste0(expend," (dollars)"))) %>%
      layout(title = paste0(expend," v. ",score)) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
  output$reading_4th <- renderPlotly({
    
    states <- req(input$states3)
    expend <- req(input$funding3)
    score <- "Reading.Score.Grade.4"
    percap <- input$per_cap3
    
    if (percap == FALSE){
      edu4 <- edu2
    }
    
    else {
      edu4 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    edu4 <- edu4 %>% filter(STATE %in% states)
    
    edu5 <- edu3 %>% filter(STATE %in% states)
    
    edu4$value <- edu4[,expend]
    edu4$scor <- edu5[,score]
    
    edu4 <- edu4 %>% filter(!is.na(edu4$scor)) %>% arrange(value)
    
    
    p <- plot_ly(edu4,x=~value, y=~scor, type="scatter",color=~YEAR, mode="markers",
                 hovertext = paste("<b>State:<b>",edu4$STATE, "<br>Year:<b>",edu4$YEAR,"<br>Score:",round(edu4$scor),"<br>Expenditure $",prettyNum(round(edu4$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(yaxis = list(title = score), xaxis = list(title = paste0(expend," (dollars)"))) %>%
      layout(title = paste0(expend," v. ",score)) %>%
      hide_colorbar() %>% 
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
  output$reading_8th <- renderPlotly({
    
    states <- req(input$states3)
    expend <- req(input$funding3)
    score <- "Reading.Score.Grade.8"
    percap <- input$per_cap3
    
    if (percap == FALSE){
      edu4 <- edu2
    }
    
    else {
      edu4 <- cbind(edu2[,1:5]/edu2[,6], edu2[,7:9])
    }
    
    edu4 <- edu4 %>% filter(STATE %in% states)
    
    edu5 <- edu3 %>% filter(STATE %in% states)
    
    edu4$value <- edu4[,expend]
    edu4$scor <- edu5[,score]
    
    edu4 <- edu4 %>% filter(!is.na(edu4$scor)) %>% arrange(value)
    
    
    p <- plot_ly(edu4,x=~value, y=~scor, type="scatter",color=~YEAR, mode="markers",
                 hovertext = paste("<b>State:<b>",edu4$STATE, "<br>Year:<b>",edu4$YEAR,"<br>Score:",round(edu4$scor),"<br>Expenditure $",prettyNum(round(edu4$value,2),big.mark=",",scientific=FALSE)), hoverinfo = 'text') %>%
      layout(yaxis = list(title = score), xaxis = list(title = paste0(expend," (dollars)"))) %>%
      layout(title = paste0(expend," v. ",score)) %>%
      config(collaborate = FALSE, 
             displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                "toggleHover", "resetViews", "toggleSpikelines"))
    
    p
    
    return(p)
  })
  
  
 
  
#Tab 5 data table
  
  output$table <- renderDataTable(
                                  edu, 
                                  extensions = 'Buttons',
                                  options = list(dom = 'Bfrtip',scrollX = TRUE, buttons = c('copy', 'csv', 'excel')),
                                  class = "display")
  
  
})

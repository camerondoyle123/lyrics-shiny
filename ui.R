# UI

library(shiny)
library(bslib)
library(DT)

shinyUI(fluidPage(
  
  # persist click data
  # resizing the window causes state reset, losing temp data
  #tags$style(type="text/css", "body { overflow-y: scroll; }"),
  #tags$style(type="text/css", "body { overflow-x: scroll; }"),
  # don't make newlines for HTML
  tags$style(type="text/css", "container p { display: inline }"),
  
  # Application title
  titlePanel("Music Sentiment - Number 1 Pop Charts"),
  
  # Main Graph
page_fillable(  
  card(
    plotOutput("distPlot", click = "click")
  ),

  # Now, two columns for the Lyrics and Word Cloud
  layout_columns(
  
    card(
      card_header(textOutput('songinfoText')),
      card_body(htmlOutput('lyricsText')),
      max_height = 400
    ),
    
    navset_card_underline(height = 400,
      title = "Summaries",
      # Panel with plot ----
      nav_panel(
        "NRC Sentiment Wordcloud", 
        plotOutput("wordcloudPlot"),
        max_height = 400
        ),
      
      # Panel with summary ----
      nav_panel(
        "Lexicon Summary", 
        DT::dataTableOutput("summartablePlot"),
        max_height = 400
        ),
      
      
    )
  
)
  
)))


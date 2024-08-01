# server
library(tidyverse)
library(tidytext)
library(plotly)
library(shiny)
library(glue)
library(DT)
library(wordcloud)
library(textdata)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  song_info <- read_csv(file = 'song_info_w_lyrics.csv')
  
  song_info <- song_info %>% mutate(word_count = str_count(lyrics,  '\\w+'))
  
  song_sentiment <- song_info %>% 
    group_by(artist_name, title, Year, word_count) %>% 
    unnest_tokens(word, lyrics) %>% 
    mutate(linenumber = row_number() )
  
  general_sentiment <- song_sentiment %>%
    inner_join(get_sentiments("bing")) %>% 
    mutate(sentiment_word = if_else(sentiment == 'negative', 
                                    paste0('<span style=color:#b52f22 !important;>', word, '</span>'),  
                                    paste0('<span style=color:#20e690 !important;>', word, '</span>')
      )
    )
  
  general_sentiment_nrc <- read_csv('nrc.csv')
  
  aww_yeah <- song_sentiment %>%
    inner_join(get_sentiments("bing")) %>%
    count(Year, index = linenumber %/% word_count, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative) %>% 
    arrange(Year) %>% 
    mutate(negative = negative*-1)
  
  
  
  for(i in 1:nrow(song_info)){
    
    if(is.na(song_info$html_lyrics[i]) | song_info$html_lyrics[i]==""){
      next
    }
    
    artist <- song_info$artist_name[i]
    song <- song_info$title[i]
    
    df <- song_info %>% filter(title == song & artist_name == artist_name)
    sent_df <- general_sentiment %>% filter(title == song & artist_name == artist_name)
    
    if(nrow(sent_df)==0){
      next
    }
    
    
    for(j in 1:nrow(sent_df)){
      
      replace_word <- sent_df$word[j]
      here <- sent_df$sentiment_word[j]
      
      df$html_lyrics <- gsub(pattern = replace_word, replacement = here, x = df$html_lyrics)
      song_info$html_lyrics[i] <- df$html_lyrics
      
    }
    
    
  }
  
  
  
  
  
  
  # Graphing and Outputs
  output$distPlot <- renderPlot({
    
    ggplot(data = aww_yeah) +
      geom_bar(aes(x = Year, y = positive), stat='identity', fill = '#20e690') +
      geom_bar(aes(x = Year, y = negative), stat='identity', fill = '#b52f22') +
      labs(title = 'General Song Sentiment over Time: 1946 - 2023',
           subtitle = 'Number One Pop Song for Given Year*',
           y = 'Sentiment Score**',
           x = 'Song Year Release',
           caption = '*Lyrics provided by Genius, songs that have no lyrics are listed as NA.\n**Green scores indicate positive sentiment, whereas Red scores indicate negative sentiment.')
    
    
    
  })
  
  output$lyricsText <- renderUI({
    
    if(is.null(input$click)) return('Click on the Bar Graph to produce lyrics!')
    
    req(input$click)
    
    year_click <- round(input$click$x)
    
    lyrics_txt <- song_info %>% filter(Year == year_click) %>% pull(html_lyrics)
    
    HTML(lyrics_txt)

    
  })
  
  output$songinfoText <- renderText({
    
    if(is.null(input$click)) return('Artist - Song Title (Year)')
    
    req(input$click)
    
    year_click <- round(input$click$x)
    
    artist <- song_info %>% filter(Year == year_click) %>% pull(artist_name)
    song_title <- song_info %>% filter(Year == year_click) %>% pull(title)
    
    paste(artist, ' - ', song_title, '(', year_click, ')')
    
    
  })
  
  output$wordcloudPlot <- renderPlot({
    
    req(input$click)
    
    year_click <- round(input$click$x)
    
    wordcloud_df <- general_sentiment_nrc %>% filter(Year == year_click)
    
    wordcloud_df %>% 
      anti_join(stop_words) %>%
      count(sentiment) %>%
      with(wordcloud(sentiment, n, max.words = 100, fixed.asp = FALSE, rot.per = 0))
    
  })
  
  output$summartablePlot <- DT::renderDataTable({
    
    req(input$click)
    
    year_click <- round(input$click$x)
    
    # nice looking table
    lex_summary_tbl <- aww_yeah %>% rename('Artist' = artist_name, 
                                           'Song Title' = title, 
                                           'Number of Words in Song' = word_count,
                                           'Number of Positive Words' = positive,
                                           'Number of Negative Words' = negative,
                                           'Bing Sentiment Score' = sentiment) %>% 
      select(-index) %>% 
      filter(Year == year_click) %>% 
      t()
    
    lex_summary_tbl <- as.data.frame(as.table(lex_summary_tbl)) %>% select(-Var2)
    colnames(lex_summary_tbl) <- c('Information', 'Value')
    lex_summary_tbl$Information <- as.character(lex_summary_tbl$Information)
    lex_tbl <- lex_summary_tbl %>% as_tibble()
    
    lex_tbl
    
    
    
  })
  
  
}



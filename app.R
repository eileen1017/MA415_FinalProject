#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(wordcloud)
library(SnowballC)
library(tm)
options(warn = -1)
library(twitteR)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(stringr)
library(reshape2)
library(scales)
library(janeaustenr)
library(htmlwidgets)
data(stop_words)

#Add stop words to filter out
stop_words <- add_row(stop_words, word = "t.co", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "https", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "http", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rt", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "RT", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "an", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "a", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "the", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "game", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "and", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "android", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "embiid", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "retweet", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "each", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "false", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "tonight", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "had", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "nofollow", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "is", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "nba", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "client", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "to", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "this", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "followers", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "was", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "true", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "web", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "download", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "iphone", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2018", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "href", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rel", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "twitter", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "twittercom", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "tweetdeck", lexicon = "SMART")


team_celtics <- read.csv("TeamCeltics.csv")
team_ssixers <- read.csv("TeamSsixers.csv")

getalltext <- function(file) {
  file_text <- readLines(file)
  file_text <- data_frame(line = 1:length(file_text), text = file_text)
  file_text <- file_text %>% unnest_tokens(word, text) %>%  anti_join(stop_words, by = "word")
  file_text_corpus <- Corpus(VectorSource(file_text))
  file_text_corpus <- tm_map(file_text_corpus, removePunctuation)
  file_text_corpus <- tm_map(file_text_corpus, content_transformer(tolower))
  file_text_corpus <- tm_map(file_text_corpus, removeWords,c("twittercom","false")) 
  file_text_corpus
  
}

getMostFreq <- function(x){
  file_2 <- TermDocumentMatrix(x)
  file_2 <- as.matrix(file_2)
  file_2 <- sort(rowSums(file_2),decreasing=TRUE)
  file_2 <- data.frame(word = names(file_2),freq=file_2)
  file_2
}

celticsbefore_text <- getalltext("celticsbefore.txt")
celticsbeforewords <- getMostFreq(celticsbefore_text)

celticsafter_text <- getalltext("celticsafter.txt")
celticsafterwords <- getMostFreq(celticsafter_text)

ssixersbefore_text <- getalltext("ssixersbefore.txt")
ssixersbeforewords <- getMostFreq(ssixersbefore_text)

ssixersafter_text <- getalltext("ssixersafter.txt")
ssixersafterwords <- getMostFreq(ssixersafter_text)

set.seed(1234)

celticsbefore <- read.delim("celticsbefore.txt", header=FALSE)
ssixersbefore <- read.delim("ssixersbefore.txt", header = FALSE)
celticsafter <- read.delim("celticsafter.txt", header=FALSE)
ssixersafter <- read.delim("ssixersafter.txt", header = FALSE)

# Read text
tidy_celticsbefore<- data_frame(paragraph = 149, text = as.character(celticsbefore$V1)) %>% 
  unnest_tokens(word, text) %>%  anti_join(stop_words, by = "word")
tidy_ssixersbefore <- data_frame(paragraph = 112, text= as.character(ssixersbefore$V1)) %>% 
  unnest_tokens(word, text) %>%  anti_join(stop_words, by = "word")
tidy_celticsafter<- data_frame(paragraph = 149, text = as.character(celticsafter$V1)) %>% 
  unnest_tokens(word, text) %>%  anti_join(stop_words, by = "word")
tidy_ssixersafter <- data_frame(paragraph = 112, text= as.character(ssixersafter$V1)) %>% 
  unnest_tokens(word, text) %>%  anti_join(stop_words, by = "word")

# celtics tweets before game
celticsbefore_sentiment <- tidy_celticsbefore %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, index = paragraph %/% 2) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)

# celtics tweets after game
celticsafter_sentiment <- tidy_celticsafter %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, index = paragraph %/% 2) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)

# 76ers tweets before game
ssixersbefore_sentiment <- tidy_ssixersbefore %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, index = paragraph %/% 3) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)

# 76ers tweets after game
ssixersafter_sentiment <- tidy_ssixersafter %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, index = paragraph %/% 3) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)




# Define UI for application that draws a histogram
ui <- 
   dashboardPage(
     dashboardHeader(title = "Twitter NBA Sentiment Analysis"),
   dashboardSidebar(
     sidebarMenu(
       menuItem("Players Info 2018", tabName = "match_up"),
       menuItem("Most Frequent Words in Twitter", tabName = "frequent_word"),
       menuItem("Most Frequent WordCloud in Twitter", tabName = "word_cloud"),
       menuItem("Sentiment analysis in Twitter", tabName = "sentiment"),
       menuItem("Sentiment analysis WordCloud in Twitter", tabName = "sentiment_cloud")
     )
   ),
   
   dashboardBody(
     tabItems(
       tabItem(tabName = "match_up",
               fluidRow(
                 box(selectInput("mode1",
                                 "Players Performance in Game Celtics Vs. 76ers in 2018 ",
                                 choices = list("Team Celtics Player Scores", "Team 76ers Player Scores")),    
                     plotOutput("plot1"), width = 12)
               )
       ),
       
       tabItem(tabName = "frequent_word",
               fluidRow(
                 box(selectInput("mode2",
                                 "Most frequent word in tweets mentioning:",
                                 choices = list("Celtics before game", "76ers before game", "Celtics after game", "76ers after game")), 
                     plotOutput("plot2"), width = 12)
               )
       ),
       
       tabItem(tabName = "word_cloud",
               fluidRow(
                 box(selectInput("mode3",
                                 "World cloud:",
                                 choices = list("Celtics before game", "76ers before game", "Celtics after game", "76ers after game")), 
                     plotOutput("plot3"), width = 12)
               )
       ),
       
       tabItem(tabName = "sentiment",
               fluidRow(
                 box(selectInput("mode4",
                                 "Sentiment:",
                                 choices = list("Positive/negative tweets about Celtics before game", "Positive/negative tweets about Celtics after game", "Positive/negative tweets about 76ers before game", "Positive/negative tweets about 76ers after game")), 
                     plotOutput("plot4"), width = 12)
               )
       ),
       
       tabItem(tabName = "sentiment_cloud",
               fluidRow(
                 box(selectInput("mode5",
                                 "Sentiment WordCloud:",
                                 choices = list("Positive/negative tweets about Celtics before game", "Positive/negative tweets about Celtics after game", "Positive/negative tweets about 76ers before game", "Positive/negative tweets about 76ers after game")), 
                     plotOutput("plot5"), width = 12)
               )
       )
       
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$plot1 <- renderPlot({
    
    input1 <- input$mode1
    
    if (input1 =="Team Celtics Player Scores"){
      graph <- ggplot(data = team_celtics, aes(x = Player)) + 
        geom_point(aes(y = Fifth_PTS), colour="blue", size = 2) + 
        geom_point(aes(y = Third_PTS), colour="red", size = 2) +
        geom_point(aes(y = April_PTS), colour="green", size = 2) +
        labs(subtitle="Celtics Vs 76ers 2018", 
             y="Points Scored", 
             x="Player Name", 
             title="Team Celtics Major Player Scores",
             caption = "Source: NBA Stats")
      print(graph)
    }
    
    if(input1 =="Team 76ers Player Scores"){
      graph <- ggplot(data = team_ssixers, aes(x = Player)) + 
        geom_point(aes(y = Fifth_PTS), colour="blue", size = 2) + 
        geom_point(aes(y = Third_PTS), colour="red", size = 2) +
        geom_point(aes(y = April_PTS), colour="green", size = 2) +
        labs(subtitle="Celtics Vs 76ers 2018", 
             y="Points Scored", 
             x="Player Name", 
             title="Team 76ers Major Player Scores",
             caption = "Source: NBA Stats")
      print(graph)
    }
    
  })
  
  output$plot2 <- renderPlot({
    
    input2 <- input$mode2
    
    if(input2 =="Celtics before game"){
      print(barplot(celticsbeforewords[1:15,]$freq, las = 2, names.arg = celticsbeforewords[1:15,]$word, col ="yellow", main ="Most frequent words",ylab = "Word frequencies"))
      
    }
    
    if(input2 =="Celtics after game"){
      print(barplot(celticsafterwords[1:15,]$freq, las = 2, names.arg = celticsafterwords[1:15,]$word, col ="yellow", main ="Most frequent words",ylab = "Word frequencies"))
    }
    
    if(input2 =="76ers before game"){
      print(barplot(ssixersbeforewords[1:15,]$freq, las = 2, names.arg = ssixersbeforewords[1:15,]$word, col ="yellow", main ="Most frequent words",ylab = "Word frequencies"))
    }
    
    if(input2 =="76ers after game"){
      print(barplot(ssixersafterwords[1:15,]$freq, las = 2, names.arg = ssixersafterwords[1:15,]$word, col ="yellow", main ="Most frequent words",ylab = "Word frequencies"))
    }
    
  })
  
  
  output$plot3 <- renderPlot({
    
    input3 <- input$mode3
    
    if(input3 =="Celtics before game"){
      print(wordcloud(celticsbefore_text,min.freq=100,max.words=500,scale=c(2.2,1), colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F))
    }
    
    if(input3 =="Celtics after game"){
      print(wordcloud(celticsafter_text,min.freq=100,max.words=500,scale=c(2.2,1), colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F))
    }
    
    if(input3 =="76ers before game"){
      print(wordcloud(ssixersbefore_text,min.freq=100,max.words=500,scale=c(2.2,1), colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F))
    }
    
    if(input3 =="76ers after game"){
      print(wordcloud(ssixersafter_text,min.freq=100,max.words=500,scale=c(2.2,1), colors=brewer.pal(8, "Dark2"),random.color=T, random.order=F))
    }
    
  })
  
  output$plot4 <- renderPlot({
    
    input4 <- input$mode4
    
    if(input4 =="Positive/negative tweets about Celtics before game"){
      celticsbefore_word_counts <- tidy_celticsbefore %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- celticsbefore_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about Celtics before May 5th game") +
        coord_flip()
      print(graph)
    }
    
    if(input4 =="Positive/negative tweets about Celtics after game"){
      celticsafter_word_counts <- tidy_celticsafter %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- celticsafter_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about Celtics After May 5th game") +
        coord_flip()
      print(graph)
    }
    
    if(input4 =="Positive/negative tweets about 76ers before game"){
      ssixersbefore_word_counts <- tidy_ssixersbefore %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- ssixersbefore_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about 76ers before May 5th game") +
        coord_flip()
      print(graph)
    }
    
    if(input4 =="Positive/negative tweets about 76ers after game"){
      ssixersafter_word_counts <- tidy_ssixersafter %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- ssixersafter_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about 76ers After May 5th game") +
        coord_flip()
      print(graph)
    }
  })
  
  output$plot5 <- renderPlot({
    
    input5 <- input$mode5
    
    if(input5 =="Positive/negative tweets about Celtics before game"){
      print(tidy_celticsbefore %>%
              inner_join(get_sentiments("bing")) %>%
              count(word, sentiment, sort = TRUE) %>%
              acast(word ~ sentiment, value.var = "n", fill = 0) %>%
              comparison.cloud(colors = c("gray80", "gray20"),
                               max.words = 100))
    }
    
    if(input5 =="Positive/negative tweets about Celtics after game"){
      print(tidy_celticsafter %>%
              inner_join(get_sentiments("bing")) %>%
              count(word, sentiment, sort = TRUE) %>%
              acast(word ~ sentiment, value.var = "n", fill = 0) %>%
              comparison.cloud(colors = c("gray80", "gray20"),
                               max.words = 100))
    }
    
    if(input5 =="Positive/negative tweets about 76ers before game"){
      print(tidy_ssixersbefore %>%
              inner_join(get_sentiments("bing")) %>%
              count(word, sentiment, sort = TRUE) %>%
              acast(word ~ sentiment, value.var = "n", fill = 0) %>%
              comparison.cloud(colors = c("gray80", "gray20"),
                               max.words = 100))
    }
    
    if(input5 =="Positive/negative tweets about 76ers after game"){
      print(tidy_ssixersafter %>%
              inner_join(get_sentiments("bing")) %>%
              count(word, sentiment, sort = TRUE) %>%
              acast(word ~ sentiment, value.var = "n", fill = 0) %>%
              comparison.cloud(colors = c("gray80", "gray20"),
                               max.words = 100))
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


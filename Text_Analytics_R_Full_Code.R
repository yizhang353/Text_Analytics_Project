
library(shiny)

shinyServer(function(input, output) {
   
    library(textreadr)
    answers <- read_document(file="/Users/yizhang/Desktop/Text_Analytics/Project/Team_1_Raw_Text_Answers.txt")
    
    a <- 43 #how many observations do you have
    b <- 11 #how many variables do you have
    df <- as.data.frame(matrix(nrow=a, ncol=b))
    
    for(z in 1:b){
        for(i in 1:a){
            df[i,z]<- answers[i*b+z-b]
        }#closing z loop
    }#closing i loop
    
    df 
    #View(df)
    
    # Set the names of the variables
    variable_names <- c("Age",
                        "Gender",
                        "Purpose",
                        "Frequency",
                        "Usage",
                        "Genre",
                        "Shopping_Habit",
                        "Free_Time",
                        "Wires",
                        "Feel_About_AirPods",
                        "Buy")
    colnames(df) <- variable_names
    
    #Age
    age <- df["Age"]
    colnames(age) <- c("text")  #need to rename the column to text 
    print(age)
    
    library(tidytext)
    library(dplyr)
    
    data(stop_words)
    frequencies_tokens_nostop_age <- age %>%
        unnest_tokens(word, text) %>%
        count(word, sort=TRUE)
    
    output$age1 <- renderPlot({
        age %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(word, sort=TRUE) %>%
            # filter(n > 2) %>%
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Spread of the age of students") +
            xlab("Age") + ylab("Number of students")+
        coord_flip()
        
    })
    
  
    #Gender
    gender <- df["Gender"]
    colnames(gender) <- c("text")  #need to rename the column to text 
    print(gender)
    
    data(stop_words)
    frequencies_tokens_nostop_gender <- gender %>%
        unnest_tokens(word, text) %>%
        count(word, sort=TRUE)
    
    output$gender <- renderPlot({
        gender %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(word, sort=TRUE) %>%
            # filter(n > 2) %>%
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Gender of students") +
            xlab("Gender") + ylab("Number of students")+ 
            coord_flip() 
    })
    
    #Splitting data into yes/no
    yes_df <- filter(df, Buy == 'Yes')
    no_df <- filter(df, Buy == 'No')
    
    nrow(yes_df) #18
    nrow(no_df) #25
    #25+18 = 43
    
    #Gender
    gender_y <- yes_df["Gender"]
    colnames(gender_y) <- c("text")  #need to rename the column to text 
    print(gender_y)
    
    data(stop_words)
    frequencies_tokens_nostop_gender <- gender_y %>%
        unnest_tokens(word, text) %>%
        count(word, sort=TRUE)
    
    output$gender_yes <- renderPlot({
        gender_y %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(word, sort=TRUE) %>%
            # filter(n > 2) %>%
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Spread of gender for students who want to buy AirPods") +
            xlab("Gender") + ylab("Number of students")+ 
            coord_flip()
    })
    
    gender_n <- no_df["Gender"]
    colnames(gender_n) <- c("text")  #need to rename the column to text 
    print(gender_n)
    
    data(stop_words)
    frequencies_tokens_nostop_gender <- gender_n %>%
        unnest_tokens(word, text) %>%
        count(word, sort=TRUE)
    
    output$gender_no <- renderPlot({
        gender_n %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            count(word, sort=TRUE) %>%
            # filter(n > 2) %>%
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Spread of gender for students who DON'T want to buy AirPods") +
            xlab("Gender") + ylab("Number of students")+ 
            coord_flip()
    })  
    
    
    #Free Time
    free_time_y <- yes_df["Free_Time"]
    colnames(free_time_y) <- c("text")  #need to rename the column to text 
    print(free_time_y)
    
    custom_stop_words <- bind_rows(data_frame(word = c("play", "watch", "watching",
                                                       "free"),
                                              lexicon = c("custom")),
                                   stop_words)
    custom_stop_words
    
    frequencies_tokens_nostop <- free_time_y %>%
        unnest_tokens(word, text) %>%
        #anti_join(stop_words) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort=TRUE)
    frequencies_tokens_nostop
    
    output$free_time_yes <- renderPlot({
        free_time_y %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>%
            count(word, sort=TRUE) %>%
            filter(n > 1) %>%    
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Free Time | Students who want to buy AirPods") +
            xlab("Free Time") + ylab("Number of students")+ 
            coord_flip()
    })
    
    
    free_time_n <- no_df["Free_Time"]
    colnames(free_time_n) <- c("text")  #need to rename the column to text 
    print(free_time_n)
    
    custom_stop_words <- bind_rows(data_frame(word = c("play", "watch", "watching",
                                                       "free", "lot", "time"),
                                              lexicon = c("custom")),
                                   stop_words)
    custom_stop_words
    
    frequencies_tokens_nostop <- free_time_n %>%
        unnest_tokens(word, text) %>%
        #anti_join(stop_words) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort=TRUE)
    frequencies_tokens_nostop
    
    output$free_time_no <- renderPlot({
        free_time_n %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>%
            count(word, sort=TRUE) %>%
            filter(n > 1) %>%    
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Free Time | Students who DON'T want to buy AirPods") +
            xlab("Free Time") + ylab("Number of students")+ 
            coord_flip()
    })
    
    #Genre
    genre_y <- yes_df["Genre"]
    colnames(genre_y) <- c("text")  #need to rename the column to text 
    print(genre_y)
    
    custom_stop_words <- data_frame(word = c("daily", "day", "listen", "releasing", 
                                             "a", "i","the", "to",
                                             "and", "at", "during", "i'm",
                                             "for", "when", "as",
                                             "going", "have", "if", "mostly",
                                             "my", "or", "well", "will", "year",
                                             "music", "like", "all", "of",
                                             "think", "slow", "r", "prefer", "on", "null", 
                                             "kind", "depend", "depends", "b"))
    
    frequencies_tokens_nostop <- genre_y %>%
        unnest_tokens(word, text) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort=TRUE)
    frequencies_tokens_nostop
    
    output$genre_yes <- renderPlot({
        genre_y %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>%
            count(word, sort=TRUE) %>%
            filter(n > 1) %>%    
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Spread of genre for students who want to buy AirPods") +
            xlab("Genre") + ylab("Number of students")+ 
            coord_flip()
    })
    

    genre_n <- no_df["Genre"]
    colnames(genre_n) <- c("text")  #need to rename the column to text 
    print(genre_n)
    
    custom_stop_words <- data_frame(word = c("daily", "day", "listen", "releasing", 
                                             "a", "i","the", "to",
                                             "and", "at", "during", "i'm",
                                             "for", "when", "as",
                                             "going", "have", "if", "mostly",
                                             "my", "or", "well", "will", "year",
                                             "music", "like", "all", "of",
                                             "think", "slow", "r", "prefer", "on", "null", 
                                             "kind", "depend", "depends", "b"))
    
    frequencies_tokens_nostop <- genre_n %>%
        unnest_tokens(word, text) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort=TRUE)
    frequencies_tokens_nostop
    
    output$genre_no <- renderPlot({
        genre_n %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>%
            count(word, sort=TRUE) %>%
            filter(n > 1) %>%    
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Spread of genre for students who DON'T want to buy AirPods") +
            xlab("Genre") + ylab("Number of students")+ 
            coord_flip()
    })
    
   
    #Feel_About_AirPods
    feel_about_airpods_y <- yes_df["Feel_About_AirPods"]
    colnames(feel_about_airpods_y) <- c("text")  #need to rename the column to text 
    print(feel_about_airpods_y)
    #View(feel_about_airpods)
    
    custom_stop_words <- bind_rows(data_frame(word = c("airpods", "feel", "airports",
                                                       "apple", "159", "buy",
                                                       "easy", "headphones", "lot"),
                                              lexicon = c("custom")),
                                   stop_words)
    custom_stop_words
    
    frequencies_tokens_nostop <- feel_about_airpods_y %>%
        unnest_tokens(word, text) %>%
        #anti_join(stop_words) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort=TRUE)
    frequencies_tokens_nostop
    
    output$feel_yes <- renderPlot({
        feel_about_airpods_y %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>%
            count(word, sort=TRUE) %>%
            filter(n > 1) %>%    
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Feel about AirPods | Students who want to buy AirPods") +
            xlab("Feel about AirPods") + ylab("Number of students")+ 
            coord_flip()
    })
    

    feel_about_airpods_n <- no_df["Feel_About_AirPods"]
    colnames(feel_about_airpods_n) <- c("text")  #need to rename the column to text 
    print(feel_about_airpods_n)
    #View(feel_about_airpods_n)
    
    custom_stop_words <- bind_rows(data_frame(word = c("airpods", "feel", "airports",
                                                       "apple", "159", "buy",
                                                       "easy", "headphones", "lot"),
                                              lexicon = c("custom")),
                                   stop_words)
    custom_stop_words
    
    frequencies_tokens_nostop <- feel_about_airpods_n %>%
        unnest_tokens(word, text) %>%
        #anti_join(stop_words) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort=TRUE)
    frequencies_tokens_nostop
    
    output$feel_no <- renderPlot({
        feel_about_airpods_n %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>%
            count(word, sort=TRUE) %>%
            filter(n > 1) %>%    
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Feel about AirPods | Students who DON'T want to buy AirPods") +
            xlab("Feel about AirPods") + ylab("Number of students")+ 
            coord_flip()
    })    
    
    #Twitter
    consumer_key <- '(you would use your own twitter developer account information here)'
    consumer_secret <- '(you would use your own twitter developer account information here)'
    access_token <- '(you would use your own twitter developer account information here)'
    access_secret <- '(you would use your own twitter developer account information here)'
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    airpod_twitter <- twitteR::searchTwitter('#AirPods + #Poor', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
    airpod_twitter_df = twitteR::twListToDF(airpod_twitter)
    
    custom_stop_words <- bind_rows(data_frame(word = c("https", "people", "poor",
                                                       "t.co", "92nmrclg22", "air",
                                                       "flexin", "grouphomeent", "hoesluvgoldie"),
                                              lexicon = c("custom")),
                                   stop_words)
    custom_stop_words
    
    
    frequencies_tokens_nostop <- airpod_twitter_df %>%
        unnest_tokens(word, text) %>%
        #anti_join(stop_words) %>%
        anti_join(custom_stop_words) %>%
        count(word, sort=TRUE)
    frequencies_tokens_nostop
    
    output$twitter <- renderPlot({
        airpod_twitter_df %>%
            unnest_tokens(word, text) %>%
            anti_join(custom_stop_words) %>%
            count(word, sort=TRUE) %>%
            filter(n > 1) %>%    
            ggplot(aes(word, n))+
            geom_col(colour = "darkslategray3", fill = "darkslategray3") +
            ggtitle("Twitter Feed: AirPods | Poor") +
            xlab("Twitter Feed") + ylab("Frequency")+ 
            coord_flip()
    })
    

    
    #Sentiment Analysis
    
    df <- read.csv(file="/Users/yizhang/Desktop/Text_Analytics/Project/QA.csv")
    df$Line<-as.character(df$Line)
    df$Question<-as.character(df$Question)
    df$text<-as.character(df$text)
    
    tidy_q <- df %>%
        unnest_tokens(word,text)
    
    frequency_q <- tidy_q %>%
        count(word, sort=TRUE)    
    library(reshape2)
    
    output$word_cloud_bing <- renderPlot({
        tidy_q %>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort = TRUE) %>%
            acast(word ~ sentiment, value.var = "n", fill = 15) %>% 
            comparison.cloud(colors = c("blue", "red"),
                             max.words = 100)
    })
    
    output$word_cloud_nrc <- renderPlot({
        tidy_q %>%
            inner_join(get_sentiments("nrc")) %>%
            count(word, sentiment, sort = TRUE) %>%
            acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
            comparison.cloud(max.words = 100)
    })
    
    
    bind_rows(afinn2,
              bing_and_nrc) %>%
        ggplot(aes(index, sentiment, fill = method)) + 
        geom_col(show.legend = FALSE) + 
        facet_wrap(~method, ncol = 1, scales = "free_y")
    
    
    
    bing_word_counts <- tidy_q %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE) %>% 
        ungroup()
    
    output$contribution_to_sentiment <- renderPlot({
        
        bing_word_counts %>%
            group_by(sentiment) %>%
            top_n(10) %>%
            ungroup() %>%
            mutate(word = reorder(word, n)) %>% 
            ggplot(aes(word, n, fill = sentiment)) + 
            geom_col(show.legend = FALSE) + 
            facet_wrap(~sentiment, scales = "free_y") + 
            labs(y = "Contribution to sentiment",x=NULL)+ coord_flip()
    })
    
    afinn2 <- tidy_q %>%
        inner_join(get_sentiments("afinn")) %>%
        group_by(index = Question) %>%
        summarise(sentiment = sum(score)) %>%
        mutate(method = "AFINN")
    
    bing_and_nrc <- bind_rows(
        tidy_q %>%
            inner_join(get_sentiments("bing")) %>%
            mutate(method = "Bing"),
        tidy_q %>%
            inner_join(get_sentiments("nrc") %>%
                           filter(sentiment %in% c("positive",
                                                   "negative"))) %>%
            mutate(method = "NRC")) %>%
        count(method, sentiment, index = Question) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
    
    
    output$sent_comb <- renderPlot({
        bind_rows(afinn2,
                  bing_and_nrc) %>%
            ggplot(aes(index, sentiment, fill = method)) + 
            geom_col(show.legend = FALSE) + 
            facet_wrap(~method, ncol = 1, scales = "free_y")
    })
    
    q_words <- df %>% 
        unnest_tokens(word, text) %>% 
        count(Question , word, sort = TRUE) %>% 
        ungroup()
    #total_words
    total_words <- q_words %>%
        group_by(Question) %>%
        summarize(total = sum(n))
    q_words <- left_join(q_words, total_words)
    
    
    output$tf_a <- renderPlot({
        ggplot(q_words, aes(n/total, fill = Question)) + 
            geom_histogram(show.legend = FALSE) + 
            xlim(NA, 0.09) +
            facet_wrap(~Question, ncol = 2, scales = "free_y")
        
    })
    
    #TFIDF Q10
    df_10 <- subset(df, Question==10)
    
    df_10$Line<-as.character(df_10$Line)
    df_10$Question<-as.character(df_10$Question)
    df_10$text<-as.character(df_10$text)
    
    q_words2 <- df_10 %>% 
        unnest_tokens(word, text) %>% 
        count(Question, word, sort = TRUE) %>% 
        ungroup()
    #total_words
    total_words2 <- q_words2 %>%
        group_by(Question) %>%
        summarize(total = sum(n))
    q_words2 <- left_join(q_words2, total_words2)
    
    output$TF_q10 <- renderPlot({
        
        ggplot(q_words2, aes(n/total, fill = Question)) + 
            geom_histogram(show.legend = FALSE) + 
            xlim(NA, 0.09) +
            facet_wrap(~Question, ncol = 2, scales = "free_y")
    })
    
    
    
    
    #Bigram
    library(textreadr)
    df <- read_document(file="/Users/yizhang/Desktop/Text_Analytics/Project/Team_1_Raw_Text_Answers.txt")
    
    a <- 43#?#? #how many observations to you have
    b <- 11#?? #how many variables do you have
    my_df <- as.data.frame(matrix(nrow=a, ncol=b))
    
    for(z in 1:b){
        for(i in 1:a){
            my_df[i,z]<- df[i*b+z-b]
        }#closing z loop
    }#closing i loop
    
    my_df 
    my_txt <- my_df$V10
    my_txt <- substr(my_txt, start=1 , stop = 10000)
    
    library(dplyr)
    my_df_Q10 <- data_frame(line=1:a, text=my_txt)
    print(my_df_Q10)
    
    Q10_bigrams <- my_df_Q10 %>%
        unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE)
    
    Q10_bigram_graph <- Q10_bigrams %>%
        filter(n>1) %>%
        graph_from_data_frame()
    
    #Q10_bigram_graph
    output$bigram <- renderPlot({
    ggraph(Q10_bigram_graph, layout = "fr") +
        geom_edge_link()+
        geom_node_point()+
        geom_node_text(aes(label=name), vjust =1, hjust=1)
    })
    
    
})
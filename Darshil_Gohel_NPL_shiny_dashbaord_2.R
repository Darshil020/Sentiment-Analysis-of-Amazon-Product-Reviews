#Author:
#Darshil Gohel : dXg163330
#Date : 16th October,2017

library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(ggplot2)
#library(igraph)
#library(networkD3)
#install.packages("GGally")
#library(GGally)
#library(network)
#library(sna)
#library(ggplot2)
#library(igraph)
#library(magrittr)
#library(visNetwork)
library(data.table)
library(ff)
library(readr)
library(tm)
library(stringr)
library(wordcloud)
library(dplyr)
#library(shiny)
library(shinydashboard)
#library(ggplot2)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "Darshil_Gohel_NLP Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      #fileInput('file11', 'Input the Reviews CSV file',accept=c('.csv')),
      #fileInput('file12', 'Input the Sentiment Text file',accept=c('txt')),
      h5("Amazon Data File already Imported"),
      h5("Dictionary Data File already Imported"),
      menuItem("Actual Data", tabName = "Data", icon = icon("th")),
      menuItem("Review Analysis", tabName = "Review_Analysis", icon = icon("dashboard")),
      menuItem("Noramlized Review Analysis", tabName = "Noramlized_Review_Analysis", icon = icon("dashboard")),
      menuItem("Comparison", tabName = "Comparison", icon = icon("dashboard")),
      menuItem("Noramlized Review Sentiment", tabName = "Noramlized_Review_Sentiment", icon = icon("th")),
      menuItem("Product Sentiment", tabName = "Product_Sentiment", icon = icon("th")),
      menuItem("Correlation Plot/Conclusion", tabName = "Plot", icon = icon("dashboard"))
      
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Data",
              fluidRow(box(title = "Actual Data with Normalized Reviews",width=12, dataTableOutput("Values11")))
      ),
      tabItem(tabName = "Review_Analysis",
              fluidRow(
                column(width=6,
                       box(title = "Review Analysis - Data",width=12, dataTableOutput("Values12"))
                ),
                column(width=6,
                       box(title = "Review Analysis - WordCloud",width=12, plotOutput("plot11")),
                       box(sliderInput("slider11", "Minimum Frequency:", 1, 5000, 500),width=12),
                       box(sliderInput("slider12", "Maximum Words:", 100, 1000, 200),width=12)
                )
              )          
      ),
      tabItem(tabName = "Noramlized_Review_Analysis",
              fluidRow(
                column(width=6,
                       box(title = "Noramlized Review Analysis - Data",width=12, dataTableOutput("Values13"))
                ),
                column(width=6,
                       box(title = "Noramlized Review Analysis - WordCloud",width=12, plotOutput("plot12")),
                       box(sliderInput("slider13", "Minimum Frequency:", 1, 5000, 500),width=12),
                       box(sliderInput("slider14", "Maximum Words:", 100, 1000, 200),width=12)
                )
              )         
      ),
      tabItem(tabName = "Comparison",
              fluidRow(box(title = "Review Analysis - WordCloud",width=4, plotOutput("plot11_1")),
                       box(title = "Noramlized Review Analysis - WordCloud",width=4, plotOutput("plot12_1")),
                       box(title = "Comparision - WordCloud",width=4, plotOutput("plot13"))),
              fluidRow( box(
                title = "Comparsion of word cloud", width = NULL, background = "light-blue",
                h5("Review cloud has been created using unnormalized reviews and hence stopwords are in highest frequency and most of the cloud is covered by stopwords"),
                h5("Normalized Review cloud has been created using normalized reviews and hence the legitimate meaningful words have been in highest frequency and most of the cloud is covered by those words as stopwords have been removed in Normalization.It is clearly desmostrating the importance of normalization in sentiment analysis"),
                h5("Comaprision word cloud is showing the words from both of word clouds (review and Normalized review")
                
              ))
      ),
      tabItem(tabName = "Noramlized_Review_Sentiment",
              fluidRow(box(title = "Normalized Review Sentiments",width=12, dataTableOutput("Values14")))
              
      ),
      tabItem(tabName = "Product_Sentiment",
              fluidRow(box(title = "Product wise Average Review Sentiments",width=6, dataTableOutput("Values15")),
                       box(title = "Top 6 most reviewd products",width=6, dataTableOutput("Values16")))
              
      ),
      tabItem(tabName = "Plot",
              fluidRow(box(title = "Product wise Average Review Sentiments",width=12, dataTableOutput("Values16_1"))),
              fluidRow(box(title = "Product 1",width=4, plotOutput("plot14")),
                       box(title = "Product 2",width=4, plotOutput("plot15")),
                       box(title = "Product 3",width=4, plotOutput("plot16"))),
              fluidRow(box(title = "Product 4",width=4, plotOutput("plot17")),
                       box(title = "Product 5",width=4, plotOutput("plot18")),
                       box(title = "Product 6",width=4, plotOutput("plot19"))),
              fluidRow( box(
                title = "Correlation between Review Sentiment and User Score", width = NULL, background = "light-blue",
                "As shown in the graph and concluded from Pearson Coefficient for each Product,there is positive but weak linear correlation between Review Sentiment and User Score for each product.  "
              )),
              fluidRow( box(
                title = "Thoughts/Recommendations on the current sentiment analysis", width = NULL, background = "light-blue",
                h5("The Pearson correlation coeffient shows how strong the linear relationship between two variables are. If the correlation is positive, that means both the variables are moving(inceasing/descresing) in same direction."),
                h5("In our case, for top 6 products person coefficient are postive.Hence,it can be infered that as review sentiment is increasing, the user score is also increaseing.
                However, the values of coefficient is ranging between 0.15-0.40 approximately and hence it is very weal linear correlation. The User score are not increasing as exactly as review sentiment."),
                
                h5("if we closely observed the scatter plots, it can be easily concluded that for the score like 1,2 and 3, the review sentiment and user score are matching each other strongly(points are near the central dotted line), However, for the score like 4,5, the sentiment score is always lagging behind user score(poitns are far away from central dotted line)
                There are two reason for this behaviour:"),

                h5("1) Review is short for the products worth of 5 or 4  score. Users are not writting enough words in the reivew to support its 5 or 4 score."),
                h5("2) The strong positive words have been missing to support its 5 or 4 score."),
                h5("3) For the products worth of rating 1,2,3, Users are writing enough words to support their score."),

                h5("From above observation, for the products worth of rating/score 1,2 or 3,  the User score calculation from the review sentiment analysis is highly recommended, but for the products worth of 4/5 score,it is not at all recommended.")
              )),
              fluidRow( box(
                title = "Sentiment Analysis Improvisation Methods", width = NULL, background = "light-blue",
                h4("Sarcasm:"),
                h5("Sarcastic comments used into reviews can not be identified by this method. Someone may use good words to criticize the bad situation in good way, then this analysis will be worng."),
                h5("e.g. Review- 'This is very good product, really!!!...this review is actally negative which has been written in sarcasm. Our current analysis will treat this review as positive comment and gqve rating according which is not true."),
                h5("Hence, some algorithm is required to identify the sarcastic reviews"),
                
                h4("Different form of words "),
                h5("The words are become verb, noun, adjective or adverb , it would not possible to find exact word for it from dictionary "),
                h5("e.g. Suppose , we have 'faithfully' as word in Review, But we have 'faithful' as word in our dictinory file.In this case, both the words can not be compared and derive the score for  it."),
                h5("Hence, dictionary should be as big as possible which include each and every form of word. There should be some algorithm to convert words to most basic form of them")
                
              )),
              fluidRow( box(
                title = "Method Applied for Data Cleansing", width = NULL, background = "light-blue",
                h5("Coversion of Lowercase for each word"),
                h5("Removal of Punctuation marks for each review"),
                h5("Removal of stop words for each review"),
                h5("Removal of extra spaces/white spaces for each review"),
                h5("Removal of 'br ' word for each review"),
                h5("Removal of all words other then pure character words for each review")
      
              ))
              
              
      )
      #tabItem(tabName = "Data",
      #        fluidRow(box(title = "Actual Data with Normalized Reviews",width=12, dataTableOutput("Values11")))
      #)
      
      
      
      
      
      
    )
  )
)
options(shiny.maxRequestSize=140*1024^2)
dotted_line=data.frame(x=c(1,2,3,4,5),y=c(1,2,3,4,5))
server <- function(input, output) {
  dotted_line=data.frame(x=c(1,2,3,4,5),y=c(1,2,3,4,5))
  dataInput<- reactive({
    #df2=read.table('C:/Users/dgohel/Documents/Darshil Data/D/ABA with R/COCAINE_DEALING.csv',sep=',',header=TRUE)
    #inFile <- input$file11  
    
    #validate(need(!is.null(inFile), "Please import the product review file"))
    
    #df=read_csv('D:/ABA with R/Mini Project - Natural Language Processing/Top250KReviews.csv/Top250KReviews.csv')
    df=read_csv('https://raw.githubusercontent.com/Darshil020/Sentiment-Analysis-of-Amazon-Product-Reviews/master/FewProducts.csv')
    
  })
  
  dataInput11<- reactive({
    df=dataInput()
    df1=df[,c('Id','ProductId','UserId','Text')]
    colnames(df1)[4]='Review'
    
    temp = Corpus(VectorSource(df1$Review))
    temp = tm_map(temp, content_transformer(tolower))
    #temp = gsub("<br />"," ",temp)
    
    temp = tm_map(temp, removePunctuation)
    temp = tm_map(temp, removeWords, stopwords("english"))
    temp = tm_map(temp, stripWhitespace)
    
    list1=rep(NULL,dim(df1)[1])
    for(i in seq(1,dim(df1)[1],1)){
      #list1[i]=temp[[i]]$content
      list1[i]=gsub("br "," ",temp[[i]]$content)
    }
    
    df1$Normalized_Review=list1
    
    
    return(df1)
    
    
  })
  
  dataInput12 <- reactive({
    df12=dataInput11()
    #df12=df12$Review
    df_temp=table(unlist(strsplit(df12$Review, " ")))
    df_word_freq=data.frame(df_temp)
    df_word_freq_words =df_word_freq[grepl("^[[:alpha:]]+$", df_word_freq$Var1),]
    df_word_freq_words_sorted=df_word_freq_words[order(df_word_freq_words$Freq,decreasing = TRUE),] 
    return(df_word_freq_words_sorted)
  })  
  
  dataInput13 <- reactive({
    df12=dataInput11()
    #df12=df12$Normalized_Review
    df_temp=table(unlist(strsplit(df12$Normalized_Review, " ")))
    df_word_freq=data.frame(df_temp)
    df_word_freq_words =df_word_freq[grepl("^[[:alpha:]]+$", df_word_freq$Var1),]
    df_word_freq_words_sorted=df_word_freq_words[order(df_word_freq_words$Freq,decreasing = TRUE),] 
    return(df_word_freq_words_sorted)
  })  
  
  dataInput14 <- reactive({
    #----Reading the Sentiment File--------
    
    
    #inFile <- input$file12
    #validate(need(!is.null(inFile), "Please import the word dictionary file"))
    df_word_sentiment_list=read.table('https://raw.githubusercontent.com/Darshil020/Sentiment-Analysis-of-Amazon-Product-Reviews/master/AFINN-111.txt',header = FALSE,col.names=c('word','score'))
    
    #-------Pre-Processing the review file-------
    df_main=  dataInput11()
    s <- strsplit(df_main$Normalized_Review, split = "\\s+")
    #data.frame(V1 = rep(df$V1, sapply(s, length)), V2 = unlist(s))
    df_seperated_word=data.frame(Id = rep(df_main$Id, sapply(s, length)), ProductId=rep(df_main$ProductId, sapply(s, length)),UserId=rep(df_main$UserId, sapply(s, length)), word = unlist(s))
    
    df_seperated_word_modified=df_seperated_word[grepl("^[[:alpha:]]+$", df_seperated_word$word),]
    df_seperated_word_modified=df_seperated_word_modified[df_seperated_word_modified$word!="",]
    
    #-------Merging the above two dataframes------
    
    score_by_word=merge(x = df_seperated_word_modified, y = df_word_sentiment_list, by = "word", all.x= TRUE)
    
    #------Post Processing the Merged files-------
    score_by_word_modified=score_by_word[!is.na(score_by_word$score),]
    grp <- group_by(score_by_word_modified,Id,ProductId,UserId)
    Review_sentiment=summarise(grp,total_score=sum(score))
    Review_sentiment$Sentiment=NA
    Review_sentiment[Review_sentiment$total_score>0,'Sentiment']='Positive'
    Review_sentiment[Review_sentiment$total_score<0,'Sentiment']='Negative'
    Review_sentiment[Review_sentiment$total_score==0,'Sentiment']='Neutral'
    
    colnames(Review_sentiment)[4]='Sentiment_Degree'
    Review_sentiment
  })
  
  dataInput15 <- reactive({
    Review_sentiment=dataInput14()
    grp <- group_by(Review_sentiment,ProductId)
    Product_Review_Distribution=summarise(grp,Total_User=n(),Average_Product_Score=mean(Sentiment_Degree))
    Product_Review_Distribution
  })
  
  dataInput16 <- reactive({
    Product_Review_Distribution=dataInput15()
    Product_Review_Distribution_sorted <- Product_Review_Distribution[order(Product_Review_Distribution$Total_User,decreasing=TRUE),]
    Product_Review_Distribution_sorted_6=Product_Review_Distribution_sorted[c(1:6),]
    Product_Review_Distribution_sorted_6
  })
  
  
  
  
  
  
  output$Values11 <- DT::renderDataTable({
    df1=dataInput11()
    #rownames(df1) <- NULL
    df1
    
  },options = list(pageLength = 20),rownames=FALSE)  
  
  output$Values12 <- DT::renderDataTable({
    df2=dataInput12()
    colnames(df2)=c('Word','Frequency')
    df2
    
  },options = list(pageLength = 20),rownames=FALSE)  
  
  output$Values13 <- DT::renderDataTable({
    df3=dataInput13()
    colnames(df3)=c('Word','Frequency')
    df3
    
  },options = list(pageLength = 20),rownames=FALSE)  
  
  output$Values14 <- DT::renderDataTable({
    df4=dataInput14()
    #df4$Average_Product_Score=round(df4$Average_Product_Score,2)
    df4
    
  },options = list(pageLength = 20),rownames=FALSE)  
  
  output$Values15 <- DT::renderDataTable({
    df5=dataInput15()
    df5$Average_Product_Score=round(df5$Average_Product_Score,2)
    df5
    
  },options = list(pageLength = 20),rownames=FALSE)  
  
  output$Values16 <- DT::renderDataTable({
    df6=dataInput16()
    df6$Average_Product_Score=round(df6$Average_Product_Score,2)
    df6
    
  },options = list(pageLength = 20),rownames=FALSE)  
  
  output$Values16_1 <- DT::renderDataTable({
    df6=dataInput16()
    df6$Average_Product_Score=round(df6$Average_Product_Score,2)
    df6
    
  },options = list(pageLength = 20),rownames=FALSE)  
  
  output$plot11 <- renderPlot({
    df2=dataInput12()
    wordcloud(df2$Var1,df2$Freq, min.freq=input$slider11, max.words = input$slider12, rot.per=.15,colors=brewer.pal(8, "Dark2"),width=1000,height=1000)
  })
  
  output$plot11_1 <- renderPlot({
    df2=dataInput12()
    wordcloud(df2$Var1,df2$Freq, min.freq=input$slider11, max.words = input$slider12, rot.per=.15,colors=brewer.pal(8, "Dark2"),width=1000,height=1000)
  })
  
  output$plot12 <- renderPlot({
    df3=dataInput13()
    wordcloud(df3$Var1,df3$Freq, min.freq=input$slider13, max.words = input$slider14, rot.per=.15,colors=brewer.pal(8, "Dark2"),width=1000,height=1000)
  })
  
  output$plot12_1 <- renderPlot({
    df3=dataInput13()
    wordcloud(df3$Var1,df3$Freq, min.freq=input$slider13, max.words = input$slider14, rot.per=.15,colors=brewer.pal(8, "Dark2"),width=1000,height=1000)
  })
  
  output$plot13 <- renderPlot({
    df12=dataInput12()
    df13=dataInput13()
    colnames(df12)=c('word','Reviews')
    colnames(df13)=c('word','Normaized_Reviews')
    #df12=df12$Normalized_Review
    merge_df=merge(x = df12, y = df13, by = "word", all = TRUE)
    rownames(merge_df) <- merge_df[,1]
    
    merge_df=merge_df[-c(1)]
    merge_df_matrix=as.matrix(merge_df)
    merge_df_matrix[is.na(merge_df_matrix)] <- 0
    comparison.cloud(merge_df_matrix,max.words=mean(input$slider12,input$slider14),random.order=FALSE)
    
  })
  
  output$plot14 <- renderPlot({
    
    Product_Review_Distribution_sorted_6=as.data.frame(dataInput16())
    Review_sentiment=as.data.frame(dataInput14())
    df=as.data.frame(dataInput())
    Top_6_Product=Review_sentiment[Review_sentiment$ProductId==Product_Review_Distribution_sorted_6[1,'ProductId'],]
    plot_df=merge(x = Top_6_Product, y = df, by = c('ProductId','UserId'), all.x= TRUE)
    plot_df$Sentiment_Degree=rescale(plot_df$Sentiment_Degree, to = c(1, 5))
    ggplot(plot_df,aes(x=Sentiment_Degree, y=Score)) + geom_point()+geom_line(data=dotted_line,aes(x=x, y=y),linetype = 2)+labs(x='Review Sentiment(Scaled)',y='User Score',title = paste(' ProductId:',Product_Review_Distribution_sorted_6[1,'ProductId'],'\n','Pearson(correlation) Coefficient:',round(cor(plot_df$Sentiment_Degree,plot_df$Score),2) ))
    
    
  })
  
  output$plot15 <- renderPlot({
    
    Product_Review_Distribution_sorted_6=as.data.frame(dataInput16())
    Review_sentiment=as.data.frame(dataInput14())
    df=as.data.frame(dataInput())
    Top_6_Product=Review_sentiment[Review_sentiment$ProductId==Product_Review_Distribution_sorted_6[2,'ProductId'],]
    plot_df=merge(x = Top_6_Product, y = df, by = c('ProductId','UserId'), all.x= TRUE)
    plot_df$Sentiment_Degree=rescale(plot_df$Sentiment_Degree, to = c(1, 5))
    ggplot(plot_df,aes(x=Sentiment_Degree, y=Score)) + geom_point()+geom_line(data=dotted_line,aes(x=x, y=y),linetype = 2)+labs(x='Review Sentiment(Scaled)',y='User Score',title = paste(' ProductId:',Product_Review_Distribution_sorted_6[2,'ProductId'],'\n','Pearson(correlation) Coefficient:',round(cor(plot_df$Sentiment_Degree,plot_df$Score),2) ))
    
    
  })
  
  output$plot16 <- renderPlot({
    
    Product_Review_Distribution_sorted_6=as.data.frame(dataInput16())
    Review_sentiment=as.data.frame(dataInput14())
    df=as.data.frame(dataInput())
    Top_6_Product=Review_sentiment[Review_sentiment$ProductId==Product_Review_Distribution_sorted_6[3,'ProductId'],]
    plot_df=merge(x = Top_6_Product, y = df, by = c('ProductId','UserId'), all.x= TRUE)
    plot_df$Sentiment_Degree=rescale(plot_df$Sentiment_Degree, to = c(1, 5))
    ggplot(plot_df,aes(x=Sentiment_Degree, y=Score)) + geom_point()+geom_line(data=dotted_line,aes(x=x, y=y),linetype = 2)+labs(x='Review Sentiment(Scaled)',y='User Score',title = paste(' ProductId:',Product_Review_Distribution_sorted_6[3,'ProductId'],'\n','Pearson(correlation) Coefficient:',round(cor(plot_df$Sentiment_Degree,plot_df$Score),2) ))
    
    
  })
  
  output$plot17 <- renderPlot({
    
    Product_Review_Distribution_sorted_6=as.data.frame(dataInput16())
    Review_sentiment=as.data.frame(dataInput14())
    df=as.data.frame(dataInput())
    Top_6_Product=Review_sentiment[Review_sentiment$ProductId==Product_Review_Distribution_sorted_6[4,'ProductId'],]
    plot_df=merge(x = Top_6_Product, y = df, by = c('ProductId','UserId'), all.x= TRUE)
    plot_df$Sentiment_Degree=rescale(plot_df$Sentiment_Degree, to = c(1, 5))
    ggplot(plot_df,aes(x=Sentiment_Degree, y=Score)) + geom_point()+geom_line(data=dotted_line,aes(x=x, y=y),linetype = 2)+labs(x='Review Sentiment(Scaled)',y='User Score',title = paste(' ProductId:',Product_Review_Distribution_sorted_6[4,'ProductId'],'\n','Pearson(correlation) Coefficient:',round(cor(plot_df$Sentiment_Degree,plot_df$Score),2) ))
    
    
  })
  
  output$plot18 <- renderPlot({
    
    Product_Review_Distribution_sorted_6=as.data.frame(dataInput16())
    Review_sentiment=as.data.frame(dataInput14())
    df=as.data.frame(dataInput())
    Top_6_Product=Review_sentiment[Review_sentiment$ProductId==Product_Review_Distribution_sorted_6[5,'ProductId'],]
    plot_df=merge(x = Top_6_Product, y = df, by = c('ProductId','UserId'), all.x= TRUE)
    plot_df$Sentiment_Degree=rescale(plot_df$Sentiment_Degree, to = c(1, 5))
    ggplot(plot_df,aes(x=Sentiment_Degree, y=Score)) + geom_point()+geom_line(data=dotted_line,aes(x=x, y=y),linetype = 2)+labs(x='Review Sentiment(Scaled)',y='User Score',title = paste(' ProductId:',Product_Review_Distribution_sorted_6[5,'ProductId'],'\n','Pearson(correlation) Coefficient:',round(cor(plot_df$Sentiment_Degree,plot_df$Score),2) ))
    
    
  })
  
  output$plot19 <- renderPlot({
    
    Product_Review_Distribution_sorted_6=as.data.frame(dataInput16())
    Review_sentiment=as.data.frame(dataInput14())
    df=as.data.frame(dataInput())
    Top_6_Product=Review_sentiment[Review_sentiment$ProductId==Product_Review_Distribution_sorted_6[6,'ProductId'],]
    plot_df=merge(x = Top_6_Product, y = df, by = c('ProductId','UserId'), all.x= TRUE)
    plot_df$Sentiment_Degree=rescale(plot_df$Sentiment_Degree, to = c(1, 5))
    ggplot(plot_df,aes(x=Sentiment_Degree, y=Score)) + geom_point()+geom_line(data=dotted_line,aes(x=x, y=y),linetype = 2)+labs(x='Review Sentiment(Scaled)',y='User Score',title = paste(' ProductId:',Product_Review_Distribution_sorted_6[6,'ProductId'],'\n','Pearson(correlation) Coefficient:',round(cor(plot_df$Sentiment_Degree,plot_df$Score),2) ))
    
    
  })

}

shinyApp(ui, server)
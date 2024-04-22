
set.seed(123)

library(data.table)

library(dplyr)

library(DT)

library(lubridate)
library(corrplot)

library(plotly)

library(ggplot2)

library(plotrix)

library(corrplot)

library(ggdendro)

library(ggrepel)

library(wordcloud)

library(tidytext)

library(stringr)

library(tm)

library(sentimentr)

library(wordcloud)

library(RSentiment)


# 1. Great Britan
gb <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/GBvideos.csv",encoding = "UTF-8"),20000)

gb[,"Location":="GB"]

# 2. France
fr <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/FRvideos.csv",encoding = "UTF-8"),20000)

fr[,"Location":="FR"]

# 3. Canada
ca <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/CAvideos.csv",encoding = "UTF-8"),20000)

ca[,"Location":="CA"]

# 4. United states
us <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/USvideos.csv",encoding = "UTF-8"),20000)

us[,"Location":="US"]

# 5. Denmark
ge <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/DEvideos.csv",encoding = "UTF-8"),20000)

ge[,"Location":="DE"]

# 6. India
ind <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/INvideos.csv",encoding = "UTF-8"),20000)

ind[,"Location":="IN"]

# 7. Japan
jp <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/JPvideos.csv",encoding = "UTF-8"),20000)

jp[,"Location":="JP"]

# 8. Korea
kr <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/KRvideos.csv",encoding = "UTF-8"),20000)

kr[,"Location":="KR"]

# 9. Mexico
mx <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/MXvideos.csv",encoding = "UTF-8"),20000)

mx[,"Location":="MX"]

# 10. Russia
ru <- tail(fread("/Users/mili/Documents/IIT/CS571/Project/csv/RUvideos.csv",encoding = "UTF-8"),20000)

ru[,"Location":="RU"]


videos <- as.data.table(rbind(gb,fr,ca,us,ge,ind,jp,mx,ru,kr))

print(videos$trending_date)

videos$trending_date <- ydm(videos$trending_date)

print(videos$publish_time)
typeof(videos$publish_time)

videos$publish_time <- ymd(substr(videos$publish_time,start = 1,stop = 10))

print(videos$trending_date)
print(videos$publish_time)
typeof(videos$publish_time)

summary(videos)

videos$dif_days <- videos$trending_date-videos$publish_time

videos$dif_days_factor <- as.factor(videos$dif_days)

print(videos$dif_days)



cleaned_videos <- videos[!duplicated(videos$video_id), ]

summary(cleaned_videos)



max_title_length <- 20
number_of_late_bloomers <- 20
      
late_bloomers <- cleaned_videos[order(cleaned_videos$dif_days, decreasing = TRUE),][1:number_of_late_bloomers,]
print(late_bloomers)


late_bloomers_title <- ifelse(nchar(late_bloomers$title) <= max_title_length, late_bloomers$title, substr(late_bloomers$title, 1, max_title_length) %>% paste0("..."))
late_bloomers_days <- late_bloomers$dif_days
late_bloomers_views <- late_bloomers$views

print(late_bloomers_days)
print(late_bloomers_views)

df <- data.frame(
  title = late_bloomers_title,
  days = late_bloomers_days,
  views = late_bloomers_views
)
print(df)


df_long <- reshape2::melt(df, id.vars = "title")


# -- VIDEOS THAT WAITED THE LONGEST BEFORE THEY BECAME TRENDING
p <- ggplot(df_long, aes(x = title, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_text(aes(label = value), vjust = -0.5, size = 3) +
  labs(title = "Number of days until becoming trending vs Total number of views",
       x = "Title",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)


# line plot - TIME BETWEEN PUBLISHING AND TRENDING 
ggplot(videos, aes(x = dif_days_factor, group = 1)) +
  geom_line(stat = "count", aes(y = ..count.., color = dif_days_factor)) +
  geom_point(stat = "count", aes(y = ..count.., color = dif_days_factor)) +
  labs(title = "Time between published and trending",
       subtitle = "In days", x = 'days ', y = 'views') +
  theme_minimal()

 
missing_data_videos <- sum(is.na(videos))
cat("sum of missing values -",missing_data_videos,"\n")
missing_videosD <- sum(is.na(videos$description))
cat("sum of missing values in description -",missing_videosD,"\n")




# Correlation plot between Views, Likes, Dislikes, Comment_count

# correlation matrix
corr_matrix <- cor(videos[, c("category_id", "views", "likes", "dislikes", "comment_count"), with = FALSE])

corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("white", "red"))(100),
         addCoef.col = "black")

#------Sentiment analysis - description ------
indices <- sample(nrow(videos), size = 200)

sample_list <- list(description = videos$description[indices],
                    views = videos$views[indices])

print(sample_list$description)

corpus = Corpus(VectorSource(sample_list$description))

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removeNumbers) 

corpus = tm_map(corpus, stripWhitespace)

corpus = tm_map(corpus, removeWords, stopwords('english'))

dtm_eap = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

freq_eap <- colSums(as.matrix(dtm_eap))
print(freq_eap)

sentiments_eap = calculate_sentiment(names(freq_eap))

print(sentiments_eap)

sent_video = cbind(sentiments_eap, as.data.frame(freq_eap))

sent_video[contains(match = "uu",vars = sent_video$text),"freq_eap"] <- 0L

sentiments <- as.data.table(sentiments_eap)

sentiments1 <- sentiments[,.N,by=.(sentiment)]

sentiments1[,"Total":=sum(N)]

sentiments1 <- sentiments1[,.("Percentage"=100*N/Total),by=.(sentiment)]


ggplot(sentiments1,aes(x = sentiment,y = Percentage ,fill=sentiment ))+
  
  geom_bar(stat = "identity") +
  
  scale_fill_manual(values =  c("#FF9999", "#66CCCC", "#99CC99")) +  # Use custom colors
  
  
  ggtitle("Sentiment analysis on Description")+xlab("Sentiment")+ylab("% Sentiment")+ 
  
  theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))


#------Sentiment analysis - title ------
indices <- sample(nrow(videos), size = 200)

sample_list <- list(title = videos$title[indices],
                    views = videos$views[indices])

print(sample_list$title)

corpus = Corpus(VectorSource(sample_list$title))

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, removeNumbers) 

corpus = tm_map(corpus, stripWhitespace)

corpus = tm_map(corpus, removeWords, stopwords('english'))

dtm_eap = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

freq_eap <- colSums(as.matrix(dtm_eap))
print(freq_eap)

sentiments_eap = calculate_sentiment(names(freq_eap))

print(sentiments_eap)

sent_video = cbind(sentiments_eap, as.data.frame(freq_eap))

sent_video[contains(match = "uu",vars = sent_video$text),"freq_eap"] <- 0L

sentiments <- as.data.table(sentiments_eap)

sentiments1 <- sentiments[,.N,by=.(sentiment)]

sentiments1[,"Total":=sum(N)]

sentiments1 <- sentiments1[,.("Percentage"=100*N/Total),by=.(sentiment)]


ggplot(sentiments1,aes(x = sentiment,y = Percentage ,fill=sentiment ))+
  
  geom_bar(stat = "identity") +
  
  scale_fill_manual(values =  c("#FF9999", "#66CCCC", "#99CC99")) +  
  
  
  ggtitle("Sentiment analysis on Title")+xlab("Sentiment")+ylab("% Sentiment")+ 
  
  theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))







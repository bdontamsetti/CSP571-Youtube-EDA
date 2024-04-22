library('tidyverse') 
library('tidyverse') 
library('readr')
library('lubridate')
library('ggplot2')
library('tidyr')
library('dplyr')
library('scales')
library('grid')
library('moments')
library('gridExtra')
library('tm')


CA <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/CAvideos.csv")
DE <- read_csv("//Users/mili/Documents/IIT/CS571/Project/csv/DEvideos.csv")
FR <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/FRvideos.csv")
GB <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/GBvideos.csv")
IN <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/INvideos.csv")
JP <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/JPvideos.csv")
KR <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/KRvideos.csv")
MX <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/MXvideos.csv")
RU <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/RUvideos.csv")
US <- read_csv("/Users/mili/Documents/IIT/CS571/Project/csv/USvideos.csv")

US$trending_date  <- as.Date(US$trending_date,format="%y.%d.%m")
CA$trending_date  <- as.Date(CA$trending_date,format="%y.%d.%m")
DE$trending_date  <- as.Date(DE$trending_date,format="%y.%d.%m")
FR$trending_date  <- as.Date(FR$trending_date,format="%y.%d.%m")
GB$trending_date  <- as.Date(GB$trending_date,format="%y.%d.%m")
IN$trending_date  <- as.Date(IN$trending_date,format="%y.%d.%m")
JP$trending_date  <- as.Date(JP$trending_date,format="%y.%d.%m")
KR$trending_date  <- as.Date(KR$trending_date,format="%y.%d.%m")
MX$trending_date  <- as.Date(MX$trending_date,format="%y.%d.%m")
RU$trending_date  <- as.Date(RU$trending_date,format="%y.%d.%m")

US$country <- c("USA")
GB$country <- c("Great Britain")
DE$country <- c("Germany")
CA$country <- c("Canada")
FR$country <- c("France")
RU$country <- c("Russia")
MX$country <- c("Mexico")
KR$country <- c("South Korea")
JP$country <- c("Japan")
IN$country <- c("India")


category_data <- c('Film & Animation', 'Autos & Vehicle', 'Music', 'Pets & Animals',
                   'Sports', 'Travel & Events', 'Gaming', 'People & Blogs',
                   'Comedy', 'Entertainment', 'News & Politics', 'Howto & Style',
                   'Education', 'Science & Technology', 'Movies', 'Shows',
                   'Nonprofits & Activism')
names(category_data) <- c(1, 2, 10, 15, 17, 19, 20, 22, 23, 24, 25, 26, 27, 28, 30, 43, 29)

CA$category_name <- category_data[as.character(CA$category_id)]
US$category_name <- category_data[as.character(US$category_id)]
FR$category_name <- category_data[as.character(FR$category_id)]
GB$category_name <- category_data[as.character(GB$category_id)]
MX$category_name <- category_data[as.character(MX$category_id)]
IN$category_name <- category_data[as.character(IN$category_id)]
RU$category_name <- category_data[as.character(RU$category_id)]
JP$category_name <- category_data[as.character(JP$category_id)]
KR$category_name <- category_data[as.character(KR$category_id)]
DE$category_name <- category_data[as.character(DE$category_id)]


combined_data <- bind_rows(US,CA,GB,MX,FR,IN,KR,DE,RU,JP)

head(combined_data)


#-------------MOST POPULAR CATEGORY ----------------


Overall_most_category<- combined_data %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("OVERALL")
head(Overall_most_category,3)

CA_most_category <- CA %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("CANADA")
head(CA_most_category,3)


US_most_category <- US %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("USA")
head(US_most_category,3)

IN_most_category <- IN %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("INDIA")
head(IN_most_category,3)


FR_most_category <- FR %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("FRANCE")
head(FR_most_category,3)


GB_most_category<- GB %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("UK")
head(GB_most_category,3)


RU_most_category <- RU %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("RUSSIA")
head(RU_most_category,3)


MX_most_category<- MX %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("Mexico")
head(MX_most_category,3)


DE_most_category <- DE %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("GERMANY")
head(MX_most_category,3)


KR_most_category<- KR %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("SOUTH KOREA")
head(KR_most_category,3)


JP_most_category<- JP %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("JAPAN")
head(JP_most_category,3)


plot1 <- ggplot(data = head(JP_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "Japan"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(data = head(KR_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "South Korea"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(data = head(RU_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "Russia"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(data = head(US_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "USA"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5 <- ggplot(data = head(GB_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "UK"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot6 <- ggplot(data = head(IN_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "India"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot7 <- ggplot(data = head(DE_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "Germany"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot8 <- ggplot(data = head(MX_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "Mexico"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot9 <- ggplot(data = head(FR_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "France"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot10 <- ggplot(data = head(CA_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "Canada"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Overall_most_category<- combined_data %>% group_by(category_name) %>%
  summarise(average_views=mean(views),least_views=min(views))%>%
  arrange(desc(average_views))
print("OVERALL")
head(Overall_most_category,3)


plot11 <- ggplot(data = head(Overall_most_category, 3), aes(x = category_name, y = average_views, fill = category_name)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top 3 Categories in", "Overall"),
       x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(plot1)
plot(plot2)
plot(plot3)
plot(plot4)
plot(plot5)
plot(plot6)
plot(plot7)
plot(plot8)
plot(plot9)
plot(plot10)
plot(plot11)



#-------------WORDS IN TITLE ----------------

# PRE-PROCESSING

corpus <- Corpus(VectorSource(FR$title))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(corpus, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("fr"))
docs <- tm_map(docs, removeWords, c("video", "audio"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

as.character(corpus[[100]])
as.character(docs[[100]])

title<- TermDocumentMatrix(docs)
m.matrix <- as.matrix(title)
title_sort <- sort(rowSums(m.matrix),decreasing=TRUE)

df_title <- data.frame(word = names(title_sort),freq=title_sort)
print(df_title[1:10,])
options(repr.plot.width = 10, repr.plot.height = 10)

ggplot(df_title[1:10,], aes(y = reorder(word, freq), x = freq)) +
  geom_col() +
  ylab('category') +
  labs(title = "Most used words in title - Japan") +
  theme(axis.text.y = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"))


#------------------------ Hour of the day when trending videos are uploaded

IN$publish_time_hour <- format(IN$publish_time, format = "%H")
IN %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in India') +
  theme_minimal()

library(knitr)
library(dplyr)
# Preprocess the data
IN$publish_time_hour <- format(IN$publish_time, format = "%H")

# Create a table of counts by hour
table_data <- IN %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour) %>%
  arrange(publish_time_hour)

kable(table_data, caption = "Number of videos Published by Hour in India")


US$publish_time_hour <- format(US$publish_time, format = "%H")
US %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in USA') +
  theme_minimal()

MX$publish_time_hour <- format(MX$publish_time, format = "%H")
MX %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in Mexico') +
  theme_minimal()


JP$publish_time_hour <- format(JP$publish_time, format = "%H")
JP %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in Japan') +
  theme_minimal()


RU$publish_time_hour <- format(RU$publish_time, format = "%H")
RU %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in Russia') +
  theme_minimal()

FR$publish_time_hour <- format(FR$publish_time, format = "%H")
MX %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in France') +
  theme_minimal()

GB$publish_time_hour <- format(GB$publish_time, format = "%H")
GB %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in UK') +
  theme_minimal()


KR$publish_time_hour <- format(KR$publish_time, format = "%H")
KR %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in South Korea') +
  theme_minimal()


DE$publish_time_hour <- format(DE$publish_time, format = "%H")
DE %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in Germany') +
  theme_minimal()


CA$publish_time_hour <- format(CA$publish_time, format = "%H")
CA %>%
  group_by(publish_time_hour) %>%
  count(publish_time_hour)%>%
  ggplot()+ geom_col(aes(x= publish_time_hour, y=n), fill='blue')+
  labs(title = 'Number of videos Published by Hour in Canada') +
  theme_minimal()




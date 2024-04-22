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
library(plotly)

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

# Assuming 'df' is your DataFrame with a column 'title'
US$title_length <- nchar(US$title)

missing_CA <- sum(is.na(CA))
missing_DE <- sum(is.na(DE))
missing_FR <- sum(is.na(FR))
missing_GB <- sum(is.na(GB))
missing_IN <- sum(is.na(IN))
missing_JP <- sum(is.na(JP))
missing_KR <- sum(is.na(KR))
missing_MX <- sum(is.na(MX))
missing_RU <- sum(is.na(RU))
missing_US <- sum(is.na(US))

cat("sum of missing values -",missing_CA,"\n")
cat("sum of missing values -",missing_DE,"\n")
cat("sum of missing values -",missing_FR,"\n")
cat("sum of missing values -",missing_GB,"\n")
cat("sum of missing values -",missing_IN,"\n")
cat("sum of missing values -",missing_JP,"\n")
cat("sum of missing values -",missing_KR,"\n")
cat("sum of missing values -",missing_MX,"\n")
cat("sum of missing values -",missing_RU,"\n")
cat("sum of missing values -",missing_US,"\n")


combined_data <- bind_rows(US,CA,GB,MX,FR,IN,KR,DE,RU,JP)

missing_sum <- colSums(is.na(combined_data))

print(missing_sum)

missing_data <- sum(is.na(combined_data))
cat("sum of missing values -",missing_data,"\n")

count_duplicates <- function(x) {
  sum(duplicated(x))
}

duplicates_matrix <- sapply(combined_data, count_duplicates)

print(duplicates_matrix)


duplicates_in_video_id <- combined_data[duplicated(combined_data$video_id) | duplicated(combined_data$video_id, fromLast = TRUE), "video_id"]

# To get the count of duplicates
count_duplicates_in_video_id <- sum(duplicated(combined_data$video_id) | duplicated(combined_data$video_id, fromLast = TRUE))

print(duplicates_in_video_id)
print(count_duplicates_in_video_id)


missing_data_desc <- sum(is.na(combined_data$description))
cat("sum of missing values in description-",missing_data_desc,"\n")

records <- subset(combined_data, video_id == "2kyS6SvSYSE")
print(records)

head(combined_data)


# Create a data frame with the logarithm of dislikes and likes
hist_data <- data.frame(dislikes_log = log(US$dislikes),
                        likes_log = log(US$likes))

print(hist_data)
# Create a histogram for dislikes_log and likes_log
p <- ggplot(data = hist_data) +
  geom_histogram(aes(x = dislikes_log), fill = '#FF9999', bins = 30, alpha = 0.7) +
  geom_histogram(aes(x = likes_log), fill = '#66CCCC', bins = 30, alpha = 0.7) +
  labs(title = 'Likes vs Dislikes Distribution',
       x = 'Log Value',
       y = 'Frequency') +
  theme_minimal()


ggplotly(p)


corpus <- Corpus(VectorSource(US$title))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(corpus, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
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
  labs(title = "US") +
  theme(axis.text.y = element_text(color = "grey20", size = 20, hjust = .5, vjust = .5, face = "plain"))



# Plot distribution of title lengths
# library(ggplot2)
# ggplot(data, aes(x = title_length)) +
# geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
# labs(title = "Distribution of Title Lengths", x = "Title Length (Words)", y = "Frequency")
# # 








library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)
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


data_total <- bind_rows(US,CA,GB,MX,FR,IN,KR,DE,RU,JP)

glimpse(US)
data <- data_total

# Load necessary libraries
library(stats)
library(ggplot2)

# Define the data frame
df <- data.frame(data_total$likes, data_total$dislikes, data_total$views, data_total$comment_count)

# Perform clustering
wcss <- vector()
for (i in 1:10) {
  kmeans_result <- kmeans(df, centers=i, nstart=10)
  wcss[i] <- kmeans_result$tot.withinss
}



print(wcss)

# Plot the Elbow Method
ggplot() + geom_line(aes(x=1:10, y=wcss)) +
  labs(title="The Elbow Method", x="Number of clusters", y="WCSS")


library(plotly)

# Run KMeans clustering
k <- 3
set.seed(0)
km <- kmeans(df, centers = k)

# Add cluster labels to the dataset
df$cluster <- as.factor(km$cluster)


#klust <- kmeans(scale(df, center = TRUE, scale = TRUE), centers = 3)
km

# Create 3D scatter plot
plot_ly(data = df, x = ~data_total$likes, y = ~data_total$comment_count, z = ~data_total$views, color = ~cluster, colors = "Blues", type = "scatter3d", mode = "markers", marker = list(size = 5, opacity = 0.8)) %>%
  layout(scene = list(xaxis = list(title = "Likes"), yaxis = list(title = "Comment Count"), zaxis = list(title = "Views")))


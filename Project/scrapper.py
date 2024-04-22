from googleapiclient.discovery import build
import pandas as pd
from datetime import datetime, timedelta


# Set your API key
api_key = "AIzaSyCwkiuuUj4NboGxvjaFBZv2not2TrA6JlM"

#Create a YouTube API client
youtube = build('youtube', 'v3', developerKey=api_key)


# target_date = datetime(2022, 4, 17)  # Change this to your desired date

# Define the start and end of the target date
# published_after = target_date.isoformat() + 'T00:00:00Z'
# published_before = (target_date + timedelta(days=1)).isoformat() + 'T00:00:00Z'

# Define the parameters for the search
search_response = youtube.videos().list(
    part='snippet,statistics',
    chart='mostPopular',
    regionCode='KR',
    maxResults=100,
    # publishedAfter=published_after,
    # publishedBefore=published_before
).execute()

# Initialize an empty list to store the data
data = []

# Extract relevant information from each video
for video in search_response['items']:
    video_id = video['id']
    snippet = video['snippet']
    statistics = video['statistics']
    trending_date = None  # Not provided by the API
    title = snippet['title']
    channel_title = snippet['channelTitle']
    category_id = snippet['categoryId']
    publish_time = snippet['publishedAt']
    tags = ",".join(snippet.get('tags', []))
    views = statistics['viewCount']
    likes = statistics.get('likeCount', 0)
    dislikes = statistics.get('dislikeCount', 0)
    comment_count = statistics.get('commentCount', 0)
    thumbnail_link = snippet['thumbnails']['default']['url']
    # comments_disabled = snippet['comments']
    comments_disabled = snippet.get('comments_disabled', False)
    # ratings_disabled = snippet['ratings']
    video_error_or_removed = None  # Not provided by the API
    description = snippet['description']
    location = "KR"  # Not provided by the API

    # Append the extracted information to the data list
    data.append([video_id, trending_date, title, channel_title, category_id, publish_time, tags, views,
                 likes, dislikes, comment_count, thumbnail_link, comments_disabled,
                 video_error_or_removed, description, location])

# Create a DataFrame from the collected data
df = pd.DataFrame(data, columns=["video_id", "trending_date", "title", "channel_title", "category_id",
                                  "publish_time", "tags", "views", "likes", "dislikes", "comment_count",
                                  "thumbnail_link", "comments_disabled",
                                  "video_error_or_removed", "description", "location"])

# Display the DataFrame
print(df.shape)

df.to_csv('/Users/mili/Documents/IIT/CS571/Project/scraped_data/KRvideos.csv', index=False)



# # Call the I18nRegions: list API
# regions_response = youtube.i18nRegions().list(
#     part='snippet',
#     hl='en_US'  # Specify the language for the region names
# ).execute()

# # Process the response
# regions = regions_response['items']
# for region in regions:
#     region_code = region['id']
#     region_name = region['snippet']['name']
#     print(f"Region Code: {region_code}, Region Name: {region_name}")

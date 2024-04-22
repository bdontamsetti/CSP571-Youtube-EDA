import pandas as pd
from googleapiclient.discovery import build
from datetime import datetime, timedelta

# Set your API key
api_key = "AIzaSyCwkiuuUj4NboGxvjaFBZv2not2TrA6JlM"

# Create a YouTube API client
youtube = build('youtube', 'v3', developerKey=api_key)

# Define the target date for which you want to retrieve trending videos
target_date = datetime(2024, 1, 17)  # Change this to your desired date

# Define the start and end of the target date
published_after = target_date.strftime('%Y-%m-%dT00:00:00Z')
published_before = (target_date + timedelta(days=30)).strftime('%Y-%m-%dT00:00:00Z')
location = "IN"
# Use the Search.list method to retrieve trending videos for the target date
search_response = youtube.search().list(
    part='snippet',
    type='video',
    regionCode=location,
    # videoCategoryId='0',  # All categories
    maxResults=100,  # Adjust the number of results as needed
    publishedAfter=published_after,
    publishedBefore=published_before
).execute()

columns = ["video_id", "trending_date", "title", "channel_title", "category_id", "publish_time", "tags", "views", "likes", "dislikes", "comment_count", "thumbnail_link", "comments_disabled", "ratings_disabled", "video_error_or_removed", "description", "location"]
df = pd.DataFrame(columns=columns)

for video in search_response['items']:
    video_id = video['id']['videoId']
    snippet = video['snippet']
    trending_date = published_before
    title = snippet['title']
    channel_title = snippet['channelTitle']
    category_id = snippet.get('categoryId', '')
    publish_time = snippet['publishedAt']
    tags = snippet.get('tags', [])
    thumbnail_link = snippet['thumbnails']['default']['url']
    description = snippet.get('description', '')

    video_response = youtube.videos().list(
        part='snippet,statistics',
        id=video_id
    ).execute()

    statistics = video_response['items'][0]['statistics']
    views = statistics.get('viewCount', 0)
    likes = statistics.get('likeCount', 0)
    dislikes = statistics.get('dislikeCount', 0)
    comment_count = statistics.get('commentCount', 0)

    snippet_info = video_response['items'][0]['snippet']
    comments_disabled = snippet_info.get('commentsDisabled', False)
    ratings_disabled = snippet_info.get('ratingsDisabled', False)
    video_error_or_removed = snippet_info.get('videoErrorOrRemoved', False)

    df = df.append({
        "video_id": video_id,
        "trending_date": trending_date,
        "title": title,
        "channel_title": channel_title,
        "category_id": category_id,
        "publish_time": publish_time,
        "tags": tags,
        "views": views,
        "likes": likes,
        "dislikes": dislikes,
        "comment_count": comment_count,
        "thumbnail_link": thumbnail_link,
        "comments_disabled": comments_disabled,
        "ratings_disabled": ratings_disabled,
        "video_error_or_removed": video_error_or_removed,
        "description": description,
        "location": location,
    }, ignore_index=True)
# print(df.iloc[0])
# print(df.shape)

df.to_csv('INvideos.csv', index=False)

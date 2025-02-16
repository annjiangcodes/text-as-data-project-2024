import praw
import pandas as pd

# Replace with your own Reddit API credentials
client_id = 'SUCUWf9SRoqx-yN4gJ3WNg'
client_secret = 'deleted'
user_agent = '2526564987854'

# Create a Reddit instance
reddit = praw.Reddit(client_id=client_id,
                     client_secret=client_secret,
                     user_agent=user_agent)

# Access the Chicago subreddit
subreddit = reddit.subreddit('chicago')

# Search for posts related to immigrants or immigration
search_terms = ['immigrants', 'immigration', 'migrants']

# Initialize a list to store scraped data
data = []

# Iterate through search results
for submission in subreddit.search(' OR '.join(search_terms)):
    # Extract relevant information about the submission
    submission_data = {
        'post_id': submission.id,
        'title': submission.title,
        'author': submission.author.name if submission.author else 'N/A',
        'created_utc': submission.created_utc,
        'url': submission.url,
        'selftext': submission.selftext,
        'num_comments': submission.num_comments,
        'score': submission.score
    }

    # Extract comments from the submission
    submission.comments.replace_more(limit=0)  # Replace 'MoreComments' objects for full comments extraction
    for comment in submission.comments.list():
        comment_data = {
            'post_id': submission.id,
            'comment_id': comment.id,
            'comment_author': comment.author.name if comment.author else 'N/A',
            'comment_body': comment.body,
            'comment_score': comment.score,
            'comment_created_utc': comment.created_utc
        }
        data.append({**submission_data, **comment_data})
        

# Create a Pandas DataFrame
df = pd.DataFrame(data)

# Convert Unix timestamp to datetime
df['created_utc'] = pd.to_datetime(df['created_utc'], unit='s')
df['comment_created_utc'] = pd.to_datetime(df['comment_created_utc'], unit='s')

# Save data to a CSV file
df.to_csv('reddit_data_with_comments_v2.csv', index=False)

print('Data scraped and saved to reddit_data_with_comments_v2.csv')


print(f"Processing submission: {submission.title}")
print(f"Fetching comments for submission: {submission.title}")


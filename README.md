# American Idol data
Data in this repository comes from [Wikipedia](https://www.wikipedia.org/).

<p align="center">
<img src="https://user-images.githubusercontent.com/32500750/232919102-911a4ebb-700e-411e-b43e-571f7d4040cf.png" width="300" 
  alt="image of American Idol logo ">
</p>

## The Datasets

- `Songs/songs_all.csv` - songs that contestants sang and competed with on American Idol from seasons 1-18

- `metadata/auditions.csv` - audition, cities, dates, and venues
- `metadata/elimination_chart.csv` - eliminations by week. Varies season-to-season based on season-length and number of finalists competing
- `metadata/finalists.csv` - information on top contestants, including birthday, hometown, and description
- `metadata/ratings.csv` - episode ratings and views.
- `metadata/seasons.csv` - season-level information, including season winner, runner-up, release dates, and judges

## Data Dictionary

### `songs_all.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|season    |character | Season Number |
|week            |character | Week date and week description |
|order            |int    | Order contestants sang in |
|contestant            |character    | Competitor name |
|song            |character    | Name of song sung |
|artist            |character    | name of song's artist |
|song_theme            |character    | Week theme for songs sung |
|result            |character    | Outcome of week's competition |

### `auditions.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|audition_date_state    |date | Start date of audition |
|audition_date_end    |date | End date of audition |
|audition_city            |character | City where audition took place|
|audition_venue           |character | Preliminary location where auditions took place|
|season          |int | Season number |
|episodes          |character | Episode numbers at this audition location |
|episode_air_date        |character | Date episode aired |
|callback_venue         |character |  Filming and callback location where auditions took place |
|callback_date_state    |date | Start date of callback audition |
|callback_date_end    |date | End date of callback audition |
|tickets_to_hollywood         |character | Number of contestants selected from audition to go to Hollywood week |
|guest_judge            |character | Name of guest judge at audition |

### `elimination_chart.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|season    |int | Season Number |
|place            |character | Place (or place range) contestant finished in competition |
|gender            |character    | Gender of contestant |
|contestant            |character    | Competitor name |
|top_36            |character    | Top 36 eliminations |
|top_36_2            |character    | Top 36 eliminations (week 2) |
|top_36_3            |character    | Top 36 eliminations (week 3) |
|top_36_4            |character    | Top 36 eliminations (week 4) |
|top_32            |character    | Top 32 eliminations |
|top_32_2            |character    | Top 32 eliminations (week 2) |
|top_32_3            |character    | Top 32 eliminations (week 3) |
|top_32_4            |character    | Top 32 eliminations (week 4) |
|top_30            |character    | Top 30 eliminations |
|top_30_2            |character    | Top 30 eliminations (week 2) |
|top_30_3            |character    | Top 30 eliminations (week 3) |
|top_25            |character    | Top 25 eliminations |
|top_25_2            |character    | Top 25 eliminations (week 2) |
|top_25_3            |character    | Top 25 eliminations (week 3) |
|top_24          |character    | Top 24 eliminations |
|top_24_2            |character    | Top 24 eliminations (week 2) |
|top_24_3            |character    | Top 24 eliminations (week 3) |
|top_20          |character    | Top 20 eliminations |
|top_20_2            |character    | Top 20 eliminations (week 2) |
|top_16          |character    | Top 16 eliminations |
|top_14          |character    | Top 14 eliminations |
|top_13          |character    | Top 13 eliminations |
|top_12          |character    | Top 12 eliminations |
|top_11          |character    | Top 11 eliminations |
|top_11          |character    | Top 11 eliminations (week 2) |
|wildcard          |character    | Wildcard week eliminations |
|comeback          |character    | Comeback week eliminations |
|top_10          |character    | Top 10 eliminations |
|top_9            |character    | Top 9 eliminations |
|top_9_2            |character    | Top 9 eliminations (week 2) |
|top_8            |character    | Top 8 eliminations |
|top_8_2            |character    | Top 8 eliminations (week 2) |
|top_7            |character    | Top 7 eliminations |
|top_7_2            |character    | Top 7 eliminations (week 2) |
|top_6            |character    | Top 6 eliminations |
|top_6_2            |character    | Top 6 eliminations (week 2) |
|top_5            |character    | Top 5 eliminations |
|top_5_2            |character    | Top 5 eliminations (week 2) |
|top_4            |character    | Top 4 eliminations |
|top_4_2            |character    | Top 4 eliminations (week 2) |
|top_3            |character    | Top 3 eliminations |
|finale            |character    | Finale eliminations |

### `finalists.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|contestant    |character | Dog Breed |
|birthday    |character | Dog Breed |
|birthplace    |character | Contestant's city of birth |
|Description    |character | Description of contestant |
|season    |int | Season |


### `ratings.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|season    |int | Season |
|show_number    |int | Episode number in season |
|episode    |character | Episode name |
|airdate    |date | Date episode aired |
|18_49_rating_share    |character | Percentage of adults aged 18-49 estimated to have watched the episode (Nielsen TV ratings) |
|viewers_in_millions    |character | Number (in millions) that watched the episode |
|timeslot_et    |character | Episode timeslot in Eastern Time |
|dvr_18_49    |character | Percentage of adults aged 18-19 estimated to have watched the episode on DVR |
|dvr_viewers_millions    |character | Number (in millions) that watched the episode on DVR |
|total_18_49    |character | Total percentage of adults aged 18-49 estimated to have watched the episode |
|weekrank    |character | Ranking of episode performance by season |


### `seasons.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|season    |int | Season |
|winner    |character | Name of winner |
|runner_up    |character | Name of runner-up |
|original_release    |character | Original air dates |
|original_network    |character | Network aired on |
|hosted_by    |character | Host's name |
|judges    |character | Name of judges |
|no_of_episodes    |int | Episode name |
|finals_venue    |character | Venue of finale |
|mentor    |character | Name of season mentor |


********************************************************

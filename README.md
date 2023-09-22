# American Idol data
Data in this repository comes from [Wikipedia](https://www.wikipedia.org/).

<p align="center">
<img src="https://user-images.githubusercontent.com/32500750/232919102-911a4ebb-700e-411e-b43e-571f7d4040cf.png" width="300" 
  alt="image of American Idol logo ">
</p>

## The Datasets

- `Songs/songs_all.csv` - songs that contestants sang and competed with on American Idol from seasons 1-18

- `metadata/auditions.csv` - audition dates, cities, and venues
- `metadata/elimination_chart.csv` - eliminations by week
- `metadata/finalists.csv` - information on top contestants, including birthday, hometown, and description
- `metadata/ratings.csv` - episode ratings and views
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
|audition_date    |character | Date(s) of audition |
|audition_city            |character | City where audition took place|
|audition_venue           |character | Venue/building location where audition took place|
|season          |int | Season number |
|episodes          |character | Episode numbers at this audition location |
|episode_air_date        |character | Date episode aired |
|callback_venue         |character |  Venue/building location where callback audition took place |
|callback_audition_date         |character | Date(s) of callback audition |
|tickets_to_hollywood         |character | Number of contestants selected from audition to go to Hollywood week |
|guest_judge            |character | Name of guest judge at audition |
|filming_dates           |character | Date(s) of filming  |
|filming_venue         |character | Placement on scale of 1-5 for the breed's "Watchdog/Protective Nature" (Trait_Score) |

### `trait_description.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|Trait    |character | Dog Breed |
|Trait_1            |character | Value corresponding to `Trait` when `Trait_Score` = 1 |
|Trait_5            |character    | Value corresponding to `Trait` when `Trait_Score` = 5 |
|Description            |character | Long description of trait |

### `breed_rank_all.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|Breed    |character | Dog Breed |
|2013 Rank            |character | Popularity of breed based on AKC registration statistics in 2013 |
|2014 Rank            |character | Popularity of breed based on AKC registration statistics in 2014 |
|2015 Rank            |character | Popularity of breed based on AKC registration statistics in 2015 |
|2016 Rank            |character | Popularity of breed based on AKC registration statistics in 2016 |
|2017 Rank            |character | Popularity of breed based on AKC registration statistics in 2017 |
|2018 Rank            |character | Popularity of breed based on AKC registration statistics in 2018 |
|2019 Rank            |character | Popularity of breed based on AKC registration statistics in 2020 |
|2020 Rank            |character | Popularity of breed based on AKC registration statistics in 2019 |
|links            |character    | Link to the dog breed's AKC webpage |
|Image            |character    | Link to image of dog breed |

********************************************************

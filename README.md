# oscar-db -- A database of film award winners

## Goal

Construct a database of film award winners that can be used to predict oscar winners. The category we want to predict are:
  - Best Picture
  - Best Director
  - Best Actor
  - Best Actress
  - Best Supporting Actor
  - Best Supporting Actress
  
Our main predictors will be the winners of similar awards. See below for a list of potential candidates.

To run the web scrapers, use the bash script `munge/collect_data.sh`. Note that you first need to download the oscar dataset from Kaggle (https://www.kaggle.com/theacademy/academy-awards/data) and specify the path in `munge/oscar_munge.R`. Moreover, by definition, a web scraper is terribly non-robust. Therefore, you may have to change the code in the future.

To run the prediction analyses, use the bash script `src/predict_oscar.sh`.

## Awards

Below is a list of awards that we would like to include in the database. The asterisk correspond to the ones that already have a web scraper.

### Primary

  - Golden Globes (*)
  - Directors Guild of America Awards (*)
  - Producers Guild of America Awards (*)
  - Screen Actors Guild Awards (*)
  - Writers Guild of America Awards
  - British Academy Film Awards (*)

### Secondary

  - Critics' Choice Movie Awards
  - Chicago Film Critics Association (*)
  - Satellite Awards
  - Los Angeles Film Critics Association (*)
  - National Board Review (*)
  - New York Film Critics Circle (*)
  - National Society of Film Critics Awards (*)
  
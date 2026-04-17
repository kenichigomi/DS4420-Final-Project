# DS4420-Final-Project
By Troy Caron and Kenichi Gomi

### Overview
Serve speed is a wildly variable metric in tennis, as it is influenced by a variety of other variables such as a player’s physical attributes and form. We developed a Bayesian Linear Regression model and User-User Collaborative Filtering model to attempt to predict serve speed, giving developing players a benchmark to train towards based on real ATP data.

### Data
Serve speed data was collected from [Ultimate Tennis Statistics](https://ultimatetennisstatistics.com/) and physical attributes were scraped from [Tennis Explorer](https://www.tennisexplorer.com/), resulting in approximately 100 players after merging and removing missing values.

### Methods
* Bayesian Linear Regression (bayesian_ml.ipynb) -> returns a full posterior distribution over serve speed
* User-User Collaborative Filtering (collab_filtering.R) -> identifies physically similar players and predicts serve speed based off their recorded speeds

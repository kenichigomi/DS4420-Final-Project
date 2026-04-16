data <- read.csv("filtered_df.csv")

# preprocess data
colnames(data)[3] <- "serve_speed"


# one-hot encoding

# all players are male so we can filter them out
# we also don't really care about rank
data$rank <- NULL
data$X <- NULL
data$sex <- NULL
data$plays <- as.factor(data$plays)

encoded <- model.matrix(~ plays - 1, data = data)
data_encoded <- cbind(data, encoded)
data_encoded$plays <- NULL

# min-max scaling
min_max_scale <- function(x) {
  rng <- range(x, na.rm = TRUE)
  
  if (any(is.na(rng)) || rng[1] == rng[2]) return(rep(NA, length(x)))
  
  out <- (x - rng[1]) / (rng[2] - rng[1])
  out
}

data_scaled <- as.data.frame(lapply(data_encoded, min_max_scale))

data_scaled$singles_rank_current <- NULL
data_scaled$singles_rank_highest <- NULL
data_scaled$doubles_rank_current <- NULL
data_scaled$doubles_rank_highest <- NULL

user_collab_filter <- function(dataframe, target, similarity_metric, k) {
  # Convert dataframe to matrix
  users_matrix <- as.matrix(dataframe)
  tgt_idx <- target
  
  # Check if target exists
  if (target > nrow(users_matrix)) {
    message(paste("Error:", target, "not found in the dataset."))
    return(NULL)
  }
  
  # Check if target has any NA values
  if (!any(is.na(users_matrix[tgt_idx, ]))) {
    message(paste(target, "is not missing any data"))
    return(NULL)
  }
  
  # Step 1: Center data (subtract row means, ignoring NAs)
  row_means <- rowMeans(users_matrix, na.rm = TRUE)
  data_centered <- users_matrix - row_means
  
  # Step 2: Replace NA with 0
  data_centered[is.na(data_centered)] <- 0
  
  # Step 3 & 4: Calculate pairwise similarity scores and Min-Max scale
  if (!similarity_metric %in% c("L2", "cosine")) {
    message("Invalid input for similarity metric. Please pick between 'L2' or 'cosine'.")
    return(NULL)
    
  } else if (similarity_metric == "L2") {
    sim_list <- apply(data_centered, 1, function(row) {
      sqrt(sum((row - data_centered[tgt_idx, ])^2))
    })
    sim_list[tgt_idx] <- NA
    
    sim_list <- sim_list * -1
    sim_scaled <- (sim_list - min(sim_list, na.rm = TRUE)) /
      (max(sim_list, na.rm = TRUE) - min(sim_list, na.rm = TRUE))
    
  } else if (similarity_metric == "cosine") {
    tgt_vec <- data_centered[tgt_idx, ]
    sim_list <- apply(data_centered, 1, function(row) {
      sum(tgt_vec * row) / (sqrt(sum(tgt_vec^2)) * sqrt(sum(row^2)))
    })
    sim_list[tgt_idx] <- NA
    
    sim_scaled <- (sim_list - min(sim_list, na.rm = TRUE)) /
      (max(sim_list, na.rm = TRUE) - min(sim_list, na.rm = TRUE))
  }
  
  # Step 5: Identify k most similar users
  k_similar_idx <- order(sim_scaled, decreasing = TRUE, na.last = NA)[1:k]
  k_similar_scores <- sim_scaled[k_similar_idx]
  
  # Step 6: Predict missing ratings
  na_positions <- which(is.na(users_matrix[tgt_idx, ]))
  
  for (col in na_positions) {
    num <- 0
    denom <- 0
    
    for (i in seq_along(k_similar_idx)) {
      similar <- k_similar_idx[i]
      score <- k_similar_scores[i]
      
      if (!is.na(users_matrix[similar, col])) {
        num   <- num   + users_matrix[similar, col] * score
        denom <- denom + score
      } else {
        num   <- num   + mean(users_matrix[similar, ], na.rm = TRUE) * score
        denom <- denom + score
      }
    }
    
    users_matrix[tgt_idx, col] <- num / denom
  }
  
  return(users_matrix[tgt_idx, ])
}

copy_data <- data_scaled

new_player <- data.frame(serve_speed=NA,
                         height_cm=183,
                         weight_kg=67,
                         age=20,
                         playsleft=1,
                         playsright=0)
copy_data <- rbind(copy_data, new_player)

player_2 <- data.frame(serve_speed=NA,
                       height_cm=198,
                       weight_kg=94,
                       age=32,
                       playsleft=1,
                       playsright=0)
copy_data <- rbind(copy_data, player_2)

player_3 <- data.frame(serve_speed=NA,
                       height_cm=195,
                       weight_kg=95,
                       age=29,
                       playsleft=0,
                       playsright=1)
copy_data <- rbind(copy_data, player_3)

# Kenichi
user_collab_filter(copy_data, 108, "cosine", 3)["serve_speed"]
user_collab_filter(copy_data, 108, "L2", 3)["serve_speed"]

# Player 2
user_collab_filter(copy_data, 109, "cosine", 3)["serve_speed"]
user_collab_filter(copy_data, 109, "L2", 3)["serve_speed"]

# Player 3
user_collab_filter(copy_data, 110, "cosine", 3)["serve_speed"]
user_collab_filter(copy_data, 110, "L2", 3)["serve_speed"]

# Ben Shelton
user_collab_filter(copy_data, 1, "cosine", 3)["serve_speed"]
user_collab_filter(copy_data, 1, "L2", 3)["serve_speed"]
# somehow he's 60cm tall and 60kg

# RMSE
rmse <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("error, not same length")
  }
  
  valid <- !is.na(actual) & !is.na(predicted)
  sqrt(mean((actual[valid] - predicted[valid])^2))
}

# MAE
mae <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("error, not same length")
  }
  
  valid <- !is.na(actual) & !is.na(predicted)
  mean(abs(actual[valid] - predicted[valid]))
}

set.seed(53)
rmse_players <- sample(1:107, 10)

true_speed <- copy_data[rmse_players, "serve_speed"]

copy_data[rmse_players, "serve_speed"] <- NA

# k=3
pred_speed_k_3_cos <- c()
pred_speed_k_3_l2 <- c()

for (player in rmse_players) {
  pred <- user_collab_filter(copy_data, player, "cosine", 3)
  pred_speed_k_3_cos <- c(pred_speed_k_3_cos, pred["serve_speed"])
  
  pred <- user_collab_filter(copy_data, player, "L2", 3)
  pred_speed_k_3_l2 <- c(pred_speed_k_3_l2, pred["serve_speed"])
}

rmse(true_speed, pred_speed_k_3_cos)
rmse(true_speed, pred_speed_k_3_l2)

mae(true_speed, pred_speed_k_3_cos)
mae(true_speed, pred_speed_k_3_l2)

# k=10
pred_speed_k_10_cos <- c()
pred_speed_k_10_l2 <- c()

for (player in rmse_players) {
  pred <- user_collab_filter(copy_data, player, "cosine", 10)
  pred_speed_k_10_cos <- c(pred_speed_k_10_cos, pred["serve_speed"])
  pred <- user_collab_filter(copy_data, player, "L2", 10)
  pred_speed_k_10_l2 <- c(pred_speed_k_10_l2, pred["serve_speed"])
}

rmse(true_speed, pred_speed_k_10_cos)
rmse(true_speed, pred_speed_k_10_l2)

mae(true_speed, pred_speed_k_10_cos)
mae(true_speed, pred_speed_k_10_l2)

# k=30
pred_speed_k_30_cos <- c()
pred_speed_k_30_l2 <- c()

for (player in rmse_players) {
  pred <- user_collab_filter(copy_data, player, "cosine", 30)
  pred_speed_k_30_cos <- c(pred_speed_k_30_cos, pred["serve_speed"])
  pred <- user_collab_filter(copy_data, player, "L2", 30)
  pred_speed_k_30_l2 <- c(pred_speed_k_30_l2, pred["serve_speed"])
}

rmse(true_speed, pred_speed_k_30_cos)
rmse(true_speed, pred_speed_k_30_l2)

mae(true_speed, pred_speed_k_30_cos)
mae(true_speed, pred_speed_k_30_l2)

# # k=50
# pred_speed_k_50_cos <- c()
# pred_speed_k_50_l2 <- c()
# 
# for (player in rmse_players) {
#   pred <- user_collab_filter(copy_data, player, "cosine", 50)
#   pred_speed_k_50_cos <- c(pred_speed_k_50_cos, pred["serve_speed"])
#   pred <- user_collab_filter(copy_data, player, "L2", 50)
#   pred_speed_k_50_l2 <- c(pred_speed_k_50_l2, pred["serve_speed"])
# }
# 
# rmse(true_speed, pred_speed_k_50_cos)
# rmse(true_speed, pred_speed_k_50_l2)

all_y <- c(pred_speed_k_3_cos, pred_speed_k_3_l2,
           pred_speed_k_10_cos, pred_speed_k_10_l2,
           pred_speed_k_30_cos, pred_speed_k_30_l2
           )

plot(true_speed, pred_speed_k_3_cos, col="red", pch=19, ylim = range(all_y, na.rm = TRUE),
     main="True vs Predicted Serve Speeds",
     xlab="True Speed (km/h)",
     ylab="Predicted Speed (km/h)")
points(true_speed, pred_speed_k_3_l2, col="red", pch=17)
points(true_speed, pred_speed_k_10_cos, col="green", pch=19)
points(true_speed, pred_speed_k_10_l2, col="green", pch=17)
points(true_speed, pred_speed_k_30_cos, col="blue", pch=19)
points(true_speed, pred_speed_k_30_l2, col="blue", pch=17)
legend("topright",
       legend = c("k=3 Cosine", "k=3 L2",
                  "k=10 Cosine", "k=10 L2",
                  "k=30 Cosine", "k=30 L2"),
       col = c("red", "red", "green", "green", "blue", "blue"),
       pch = c(19, 17, 19, 17, 19, 17))


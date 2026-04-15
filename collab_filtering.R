data <- read.csv("filtered_df.csv")

# preprocess data

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
# min_max_scale <- function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }
# 
# cols_to_scale <- c("height_cm", 
#                    "weight_kg", 
#                    "age", 
#                    "singles_rank_current", 
#                    "singles_rank_highest",
#                    "doubles_rank_current",
#                    "doubles_rank_highest",
#                    "playsleft",
#                    "playsright")
# data_encoded[cols_to_scale] <- lapply(data_encoded[cols_to_scale], min_max_scale)
data_encoded$singles_rank_current <- NULL
data_encoded$doubles_rank_current <- NULL

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

new_player <- data.frame("value"=NA,
                         height_cm=183,
                         weight_kg=67,
                         age=20,
                         singles_rank_highest=1230,
                         doubles_rank_highest=1230,
                         playsleft=1,
                         playsright=0)
data_encoded <- rbind(data_encoded, new_player)

player_2 <- data.frame("value"=NA,
                       height_cm=198,
                       weight_kg=94,
                       age=32,
                       singles_rank_highest=35,
                       doubles_rank_highest=67,
                       playsleft=1,
                       playsright=0)
data_encoded <- rbind(data_encoded, player_2)

player_3 <- data.frame("value"=NA,
                       height_cm=195,
                       weight_kg=95,
                       age=29,
                       singles_rank_highest=6,
                       doubles_rank_highest=10,
                       playsleft=0,
                       playsright=1)
data_encoded <- rbind(data_encoded, player_3)


user_collab_filter(data_encoded, 108, "cosine", 30)
user_collab_filter(data_encoded, 108, "L2", 30)

user_collab_filter(data_encoded, 109, "cosine", 30)
user_collab_filter(data_encoded, 109, "L2", 30)

user_collab_filter(data_encoded, 110, "cosine", 30)
user_collab_filter(data_encoded, 110, "L2", 30)

# Ben Shelton
user_collab_filter(data_encoded, 1, "cosine", 30)
user_collab_filter(data_encoded, 1, "L2", 30)
# somehow he's 60cm tall and 60kg



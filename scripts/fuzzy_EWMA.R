# Functions that runs the FTS-EWMA forecast
fts <- function(EW=100,nclus=3,lambda=0.94,window=c('rolling','expanding')) {
  library(TTR)    # For EWMA
  library(e1071)  # For fuzzy logic
  library(fclust) # For fuzzy clustering
  library(zoo)
  # Apply Fuzzy C-Means clustering on the rolling window data
    fcm_result          <- cmeans(DTW, centers = nclus)
    cluster_memberships <- fcm_result$membership
    cluster_centroids   <- fcm_result$centers
    
    # Fuzzification: Assign each data point to a fuzzy state based on 
    # maximum membership degree
    fuzzy_states <- apply(cluster_memberships, 1, which.max)
    
    # Construct the FLR matrix
    flr_matrix <- matrix(0, nrow = nclus, ncol = nclus)
    for (i in 1:(length(fuzzy_states) - 1)) {
      
      from_state <- fuzzy_states[i]
      to_state   <- fuzzy_states[i + 1]
      flr_matrix[from_state, to_state] <- flr_matrix[from_state, to_state] + 1
      
    }
    
    # Normalize the FLR matrix to get transition probabilities
    for (i in 1:nclus) if (sum(flr_matrix[i, ]) > 0) flr_matrix[i, ] <- flr_matrix[i, ] / sum(flr_matrix[i, ])
    
    # Final membership degrees from FCM for the last data point in the window
    final_membership <- cluster_memberships[nrow(cluster_memberships), ]
    
    # Adjust membership degrees using transition probabilities 
    # FLR Matrix allows these final membership degrees to be weighted by previous (historical) transition probabilities
    adjusted_memberships <- final_membership * transition_probs
    
    # Calculate weighted forecast using the adjusted memberships and cluster centroids
    prds[w + 1] <- lambda * sum(adjusted_memberships * cluster_centroids) + (1 - lambda) * prds[w]
    
  }
  DT[,ncol(DT)] <- prds
  return(DT)

#======================== cluster ==============================================
#' Cluster rows or columns of a matrix
#' 
#' @description Convenience function for sorting the rows and/or columns of a 
#' matrix based on \link[stats]{hclust}.
#' 
#' @description Given a numeric matrix, or data frame, sort rows and/or 
#' columns based on correlation distance heirarchical clustering.
#'
#' @param m Numeric matrix.
#' @param method Clustering method passed to \link[stats]{hclust}. Defaults to
#' 'complete'.
#' @param rows Logical. Whether to cluster rows. Defaults to TRUE.
#' @param cols Logical. Whether to cluster columns. Defaults to TRUE.
#' 
#' @return Matrix with columns and/or rows sorted.
#' 
#' @importFrom stats cor dist hclust
#' @importFrom dplyr %>%
#' @export
#' 
cluster <- function(m, method = 'ward.D2', correlation = 'pearson', metric = 'euclidean', rows = T, cols = T) {
  if (rows) row <- robust_clust(t(m), method, correlation, metric) else row <- 1:nrow(m)
  if (cols) col <- robust_clust(m, method, correlation, metric) else col <- 1:ncol(m)
  m[row, col]
}

robust_clust <- function(m, method, correlation, metric) {

  if (min(dim(m)) > 1) {
    
    if (any(is.na(m))) m[is.na(m)] <- median(m, na.rm = TRUE) # TODO better impute
    
    out <- 
      (m + rnorm(length(m), 0, 0.00001)) %>% 
      cor(use = 'complete.obs', method = correlation) %>% 
      dist(metric) %>%
      hclust(method) %>%
      .$order
  } else {
    out <- 1:ncol(m)
  }
  return(out)
}

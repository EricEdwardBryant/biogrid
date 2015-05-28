#====================== plot.tbl_biogridr_aggregated ===========================
#' Plot a heatmap of aggregated BioGRID interactions
#' 
#' @description Plot a clustered heatmap of aggregated biogrid interactions.
#' 
#' @param x Aggregated BioGRID interactions (class = tbl_biogridr_aggregated)
#' @param type A string indicating which type of interaction to plot ('score', 
#' 'positive', 'negative', 'physical', 'other')
#' @param trunc value to truncate data. Truncation occurs before clustering. 
#' Defaults to 3.
#' @param clust Whether or not to cluster the data. Defaults to TRUE.
#' @param ... additional arguments passed to \link{cluster}.
#' 
#' @export
#' @importFrom dplyr %>% select_ add_rownames mutate 
#' @importFrom tidyr spread gather
#' @importFrom stats dist hclust
#' @importFrom ggplot2 theme_set theme_bw theme_update element_blank 
#' element_text ggplot aes geom_tile scale_fill_gradient2
#' 
plot.tbl_biogridr_aggregated <- function(
  x, type = 'score', bounds = c(-3, 3), density = 1, clust = TRUE, ...) {
  
  # coerce to matrix
  mat <- x %>%
    select_('a', 'b', 'type' = type) %>%
    spread(b, type, fill = 0) %>%
    assign_rownames('a') %>%
    as.matrix
  
  # reshape matrix
  if (!is.null(bounds)) {
    stopifnot(length(bounds) == 2)
    mat <- set_bounds(mat, min(bounds), max(bounds))
  }
  if (attr(x, 'net') == 'outer') {
    cols <- apply(mat, 2, function(c) sum(abs(c), na.rm = T) > (length(c) * density))
    if (length(which(cols)) < 2) {
      stop('No interactors met the density threshold of ', density, 
           '. Consider lowering the "density" argument.\n',
           '  See ?plot.tbl_biogridr_aggregated for more help.')
    }
    mat <- mat[, cols]
  }
  if (clust) mat <- cluster(mat, ...)

  ggtile(mat)
}


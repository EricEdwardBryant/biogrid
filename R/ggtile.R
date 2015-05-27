#======================== ggtile ===============================================
#' Plot a numeric matrix
#' 
#' @description Convenience function for plotting a numeric matrix as colored 
#' tiles using ggplot with row and column order preserved. Combine with 
#' \link{cluster} for a clustered heatmap.
#' 
#' @param m A numeric matrix
#' @param diverging Logical. Whether to use a diverging color scale.
#' @param ... other arguments passed to scale_fill_gradient 
#' \link[ggplot2]{discrete_scale}.
#' 
#' @return A ggplot object.
#' 
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom ggplot2 theme theme_bw element_blank element_text ggplot aes 
#' geom_tile scale_fill_gradient2 scale_fill_gradient
#' @export
#' 
ggtile <- function(m, diverging = TRUE, ...) {
  
  m_long <- m %>%
    as.data.frame %>%
    add_rownames('row') %>%
    mutate(row = factor(row, levels = row, ordered = TRUE)) %>%
    gather(col, value, -row)
  
  if (diverging) {
    scale_fill_grad <- function(...) {
      scale_fill_gradient2(low = 'turquoise1', mid = 'black', 
                           high = 'yellow', na.value = 'grey50', ...)
    }
  } else {
    scale_fill_grad <- function(...) {
      scale_fill_gradient(low = 'black', high = 'orange', na.value = 'grey50', ...)
    }
  }
  ggplot(m_long, aes(col, row)) +
    geom_tile(aes(fill = value), color = 'white') +
    scale_fill_grad(...) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

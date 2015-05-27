# Class assignment for use in dplyr piplelines
assign_class <- function(x, ...) { class(x) <- c(...); return(x) }

# Remove first class e.g. c('tbl_df', 'data.frame') to 'data.frame' 
drop_class <- function(x) assign_class(x, class(x)[-1])

# Attribute assignment for use in dplyr pipelines
assign_attr <- function(x, which, ...) { attr(x, which) <- c(...); return(x) }

# Rowname assignment for use in dplyr pipelines
assign_rownames <- function(tbl, col, drop = TRUE) {
  rownames(tbl) <- tbl[[col]]
  if (drop) tbl[[col]] <- NULL
  return(tbl) 
} 

# coerce values of a vector to upper and lower bounds without scaling
set_bounds <- function(x, lower, upper) {
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

#' @importFrom dplyr as_data_frame %>% as.tbl
#' @export
as.tbl.hclust <- function(x) {
  as_data_frame(x[c('labels', 'order')]) %>%
    assign_attr('hclust', x) %>%
    assign_class('tbl_hclust', class(.))
}

#' @importFrom stats as.hclust
#' @export
as.hclust.tbl_hclust <- function(x) attr(x, 'hclust') %>% assign_class('hclust')

# From Hadley Wickham. I use it to give NULL arguments default values
`%||%` <- function(a, b) if (is.null(a)) b else a



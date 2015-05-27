#' Chain functions into pipeline
#'
#' @description 
#' Chain functions to make nested function calls easier to read. 
#' In brief, \code{a \%>\% f(b)} is equivalent to \code{f(a, b)}. This function 
#' was exported from \code{dplyr} and originates from \code{magrittr}. See 
#' \link[dplyr]{chain} and \link[magrittr]{\%>\%} for more details.
#' 
#' @usage a \%>\% b
#'
#' @examples \dontrun{
#' set.seed(3)
#' ## Three ways to do a silly thing
#' # Nested functions
#' nested <- hclust(
#'   dist(
#'     cor(
#'       matrix(
#'         rnorm(100), 
#'         nrow = 4), 
#'       method = 'spearman'), 
#'     'minkowski'), 
#'   'complete')
#' 
#' # Temporary variable assignment (more readable)
#' temp   <- rnorm(100)
#' temp   <- matrix(temp, nrow = 4)
#' temp   <- cor(temp, method = 'spearman')
#' temp   <- dist(temp, 'minkowski')
#' temped <- hclust(temp, 'complete')
#' 
#' # Chained functions (most readable)
#' chained <- rnorm(100) %>% 
#'   matrix(nrow = 4) %>% 
#'   cor %>% 
#'   dist('minkowski') %>% 
#'   hclust('complete')
#' }
#'
#' @name %>%
#' @rdname chain
#' @importFrom dplyr %>%
#' @export
#' 
NULL

#' Create a table from a data source
#' 
#' @description
#' This is a generic exported directly from \link[dplyr]{dplyr} 
#' (see \link[dplyr]{tbl}).
#' 
#' @name tbl
#' @importFrom dplyr tbl
#' @export
NULL

#' Collect tbl_sql as an R data.frame.
#' 
#' @description
#' This is a generic exported directly from \link[dplyr]{dplyr} 
#' (see \link[dplyr]{collect}).
#' 
#' @name collect
#' @importFrom dplyr collect
#' @export
NULL

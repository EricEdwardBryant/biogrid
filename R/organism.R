#=========== organism ===============
#' Search for NCBI organism ID
#' 
#' @description
#' A convenience function that returns NCBI organism IDs based on a 
#' genus-species name. This is useful for filtering interactions for specific
#' organisms.
#' 
#' @param x a regular expression used to match genus-species names.
#' @param ... additional arguments passed to \link[base]{grepl}.
#' 
#' @examples \dontrun{
#' organism('cerevisiae')
#' }
#' 
#' @importFrom dplyr %>% tbl collect filter
#' @export
#' 
organism <- function(x, ...) {
  suppressWarnings(src_biogrid()) %>%
    tbl('organisms') %>%
    collect %>%
    filter(grepl(x, organism, ...)) %>%
    .[['id']]
}

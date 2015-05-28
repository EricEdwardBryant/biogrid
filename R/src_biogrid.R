#=================== src_biogridr Documentation =====================================
#' Connect to local BioGRID database
#' 
#' @description
#' Connect to the biogridr package's interaction database
#' 
#' @param path Path to BioGRID SQLite database. Defaults to 
#' \code{getOption('biogridr.db')}.
#' 
#' @details
#' BioGRID interactions are stored in a local SQLite database, and 
#' \code{src_biogridr()} returns a connection to this database via 
#' \link[dplyr]{dplyr}. This allows the user to aviod loading all 700,000+ 
#' interactions into memory by using \code{dplyr} to \link[dplyr]{select} 
#' columns and \link[dplyr]{filter} rows.
#' 
#' @examples \dontrun{
#' 
#' # First download all interaction data
#' # WARNING: This takes a few minutes!
#' update_biogridr()
#' 
#' # Connect to the database
#' src_biogridr()
#' 
#' # Query the database for a CTF4 outer network 
#' src_biogridr() %>%
#'   outer_net('CTF4')
#' 
#' # Aggregate the CTF4 outer network
#' src_biogridr() %>%
#'   outer_net('CTF4') %>%
#'   aggregate
#' 
#' # Use dplyr to make custom queries to the database
#' library(dplyr)
#' src_biogridr() %>%
#'   tbl('systems')
#'   
#' src_biogridr() %>%
#'   tbl('organisms')
#' 
#' genes <- c('TOF1', 'CTF4')
#' src_biogridr() %>%
#'   tbl('interactions') %>%
#'   filter(
#'     (a %in% genes | b %in% genes),
#'      a_organism == organism('cerevisiae'),
#'      b_organism == organism('cerevisiae')) %>%
#'   select(a, b)
#' }
#' 
#' @name src_biogridr
#' @aliases biogridr
#' @importFrom dplyr %>%
#' @importFrom DBI dbConnect dbGetInfo dbGetQuery
#' @importFrom RSQLite SQLite initExtension
#' @export
#' 
src_biogridr <- function(path = getOption('biogridr.db')) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Path does not exist", call. = FALSE)
  }
  con <- dbConnect(SQLite(), path)
  initExtension(con)
  release <- as.character(dbGetQuery(con, "SELECT release FROM log"))
  if (release == 'example') {
    warning('Currently using an example dataset.\n',
            'Use ?update_biogridr to download the latest release of ',
            'BioGRID interaction data.',
            call. = FALSE)
  }
  list(con = con, path = path, release = release, info = dbGetInfo(con)) %>%
    assign_class('src_biogridr', 'src_sqlite', 'src_sql', 'src')
}

#=========== src_desc ============
#' @importFrom dplyr src_desc
#' @export
src_desc.src_biogridr <- function(x) {
  paste0('sqlite ', x$info$serverVersion, ' [', x$release, ']')
}

#========== tbl ==================
#' @importFrom dplyr tbl tbl_sql
#' @export
tbl.src_biogridr <- function(src, from, ...) {
  src %>% 
    drop_class %>%
    tbl(from) %>%
    assign_class('tbl_biogridr', class(.))
}

#========= collect ==============
#' @importFrom dplyr collect
#' @export
collect.tbl_biogridr <- function(tbl) {
  tbl %>%
    drop_class %>%
    collect %>%
    assign_class('tbl_biogridr', class(.))
}

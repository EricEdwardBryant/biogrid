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
#' db <- src_biogridr()
#' 
#' # Query the database for a CTF4 outer network 
#' outer_net(db, 'CTF4')
#' 
#' # Aggregate the CTF4 outer network
#' aggregate(outer_net('CTF4'))
#' 
#' # Use dplyr to make custom queries to the database
#' library(dplyr)
#' tbl(db, 'systems')
#' tbl(db, 'organisms')
#' 
#' genes <- c('TOF1', 'CTF4')
#' organism <- organism('cerevisiae')
#' tbl(db, 'interactions') %>%
#'   filter(
#'     (a %in% genes | b %in% genes) &
#'      a_organism == organism &
#'      b_organism == organism
#'   ) %>%
#'   select(a, b) %>%
#'   collect()
#' }
#' 
#' @name src_biogridr
#' @aliases biogridr
#' @importFrom dplyr %>%
#' @importFrom DBI dbConnect dbGetQuery
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
  list(con = con, path = path, release = release) %>%
    add_class('src_biogridr')
}

#---- src_biogridr methods --------------------------------------------------
#' @export
print.src_biogridr <- function(x) {
  cat(paste0(
    'sqlite ', RSQLite::rsqliteVersion()[[2]], 
    ' [biogridr - ', x$release, ']\n',
    dplyr:::wrap('tbls: ', paste0(RSQLite::dbListTables(x$con), collapse = ', '))
  ))
}

#' @importFrom dbplyr tbl_sql
#' @export
tbl.src_biogridr <- function(src, from, ...) {
  dbplyr::tbl_sql('sqlite', src = src, from = from, ...) %>% add_class('tbl_biogridr')
}

#' @export
collect.tbl_rothfreezer <- function(x, ...) {
  
  result <- drop_class(x) %>% collect(...)
  coltypes <- x$src$cnf$column_types
  present <- intersect(names(coltypes), names(result))
  
  if (length(present)) {
    coerce <- coltypes[present]
    for (i in 1:length(coerce)) {
      type   <- coerce[[i]]
      column <- names(coerce[i])
      result[[column]] <- 
        switch(
          type,
          logical   = as.logical(result[[column]]),
          integer   = as.integer(result[[column]]),
          character = as.character(result[[column]]),
          numeric   = as.numeric(result[[column]]),
          double    = as.numeric(result[[column]]),
          factor    = as.factor(result[[column]])
        )
    }
  }
  # Coerce to desired column type
  return(result)
}

#---- OO Utilities ------------------------------------------------------------
# Add an S3 subclass in pipeline
# @param x an S3 object
# @param ... Character; S3 subclasses to add to object
add_class <- function(x, ...) { class(x) <- c(..., class(x)); return(x) }

# Drop an S3 subclass in pipeline
# @param x an S3 object
drop_class <- function(x) { class(x) <- class(x)[-1]; return(x)}

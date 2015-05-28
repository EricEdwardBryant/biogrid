.onLoad <- function(libname, pkgname) {


  op     <- options()
  dir    <- system.file('db', package = 'biogridr')
  db     <- paste(dir, 'biogridr.sqlite', sep = '/')
  config <- paste(dir, 'biogridr.yaml',   sep = '/')
  url    <- 'http://thebiogrid.org/downloads/archives/Latest%20Release/BIOGRID-ALL-LATEST.tab2.zip'
  
  # Create db and import source tables if it doesn't already exist
  if (!file.exists(db)) easydb::db_build(config)
  # Add interactions table if not present
  initialize_biogridr(db, url)
  
  # Set global package options without overriding those set by user
  default <- list(biogridr.url = url, biogridr.db = db, biogridr.config = config)
  toset <- !(names(default) %in% names(op))
  if (any(toset)) options(default[toset])

  invisible()
}

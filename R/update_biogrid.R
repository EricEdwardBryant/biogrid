#============ update_biogrid Documentation ========================
#' Update Local BioGRID Database
#' 
#' @description
#' Update your local BioGRID database with the latest interactions, or 
#' previously archived versions of the BioGRID database.
#' 
#' @param url URL to BIOGRID-ALL-<VERSION>.tab2.zip archive. Defaults to 
#' \code{getOption('biogrid.url')}.
#' @param db Path to an SQLite database. Defaults to 
#' \code{getOption('biogrid.db')}.
#' 
#' @details
#' This function downloads a BIOGRID-ALL-<VERSION>.tab2.zip to a temporary file,
#' which is then written to an SQLite database. The default biogrid url 
#' (\code{getOption('biogrid.url')}) is the latest release.
#' 
#' @importFrom dplyr %>% rename_
#' @importFrom data.table fread
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom utils unzip
#' @export
#' 
update_biogrid <- function(
  db = getOption('biogrid.db'), 
  url = getOption('biogrid.url')) {
  
  packagedb <- system.file('extdata/biogrid.sqlite', package = 'biogrid')
  
  # If creating a new database make a copy of the package's database
  if (db != packagedb & !file.exists(db)) file.copy(packagedb, db)
  
  # download zip archive to temp file
  zip <- tempfile()
  download.file(url, zip)
  message('Unzipping ...')
  tab2 <- unzip(zip, exdir = dirname(zip))[1]
  
  # rename is explict to encourage an error if biogrid changes naming scheme
  message('Reading ', basename(tab2), ' ...')
  interactions <- tab2 %>%
    fread(colClasses = 'character') %>%
    rename_(id             = ~`#BioGRID Interaction ID`,
            a_entrezgene   = ~`Entrez Gene Interactor A`,
            b_entrezgene   = ~`Entrez Gene Interactor B`,
            a_biogrid      = ~`BioGRID ID Interactor A`,
            b_biogrid      = ~`BioGRID ID Interactor B`,
            a_systematic   = ~`Systematic Name Interactor A`,
            b_systematic   = ~`Systematic Name Interactor B`,
            a              = ~`Official Symbol Interactor A`,
            b              = ~`Official Symbol Interactor B`,
            a_synonyms     = ~`Synonyms Interactor A`,
            b_synonyms     = ~`Synonyms Interactor B`,
            system         = ~`Experimental System`,
            system_type    = ~`Experimental System Type`,
            author         = ~`Author`,
            pmid           = ~`Pubmed ID`,
            a_organism     = ~`Organism Interactor A`,
            b_organism     = ~`Organism Interactor B`,
            throughput     = ~`Throughput`,
            score          = ~`Score`,
            modification   = ~`Modification`,
            phenotypes     = ~`Phenotypes`,
            qualifications = ~`Qualifications`,
            tags           = ~`Tags`,
            source_db      = ~`Source Database`)
  
  con <- dbConnect(SQLite(), dbname = db)
  on.exit(dbDisconnect(con))
  
  log <- data.frame(
    'date'    = Sys.time(), 
    'file'    = basename(tab2),
    'release' = gsub('*.tab2.txt$', '', basename(tab2)),
    'table'   = 'interactions', 
    'url'     = url,
    stringsAsFactors = FALSE)
  dbWriteTable(con, 'log', log, overwrite = TRUE, row.names = FALSE)
  
  message('Writing ', log$release, ' to ', db)
  dbWriteTable(
    con, 'interactions', interactions, 
    overwrite = TRUE, row.names = FALSE)
  return(invisible(TRUE))
}


#============ initialize_biogrid Documentation ========================
#' Initialize Local BioGRID Database
#' 
#' @description
#' Initialize your local BioGRID database with the latest interactions, or 
#' previously archived versions of the BioGRID database.
#' 
#' @param url URL to BIOGRID-ALL-<VERSION>.tab2.zip archive. Defaults to 
#' \code{getOption('biogrid.url')}.
#' @param db Path to an SQLite database. Defaults to 
#' \code{getOption('biogrid.db')}.
#' 
#' @details
#' If the local BioGRID database has not been initialized, this function will
#' run \link{update_biogrid} which downloads a BIOGRID-ALL-<VERSION>.tab2.zip 
#' to a temporary file, which is then written to an SQLite database.
#' 
#' @seealso \link{update_biogrid}
#' 
#' @importFrom dplyr %>% rename_ src_sqlite
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbListTables
#' @importFrom utils unzip
#' @export
#' 
initialize_biogrid <- function(db = getOption('biogrid.db'), 
                               url = getOption('biogrid.url')) {
  src <- src_sqlite(db)
  
  if (!('interactions' %in% dbListTables(src$con))) update_biogrid(db, url)
  return(invisible(TRUE))
}

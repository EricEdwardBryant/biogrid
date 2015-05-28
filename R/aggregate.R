#=========== aggregate ===============
#' Aggregate interaction data from BioGRID
#' 
#' @description 
#' Aggregates BioGRID interaction data based on several categories. 
#' For more information see Details.
#' 
#' @param x An object of class \code{tbl_biogridr} (i.e. interaction data derived 
#' from the 'interactions' table in your local BioGRID database).
#' @param neg Character vector of interaction system names that correspond to 
#' negative interactions (e.g. 'Negative Genetic').
#' @param pos Character vector of interaction system names that correspond to 
#' positive interactions (e.g. 'Positive Genetic').
#' @param phy Character vector of interaction system names that correspond to 
#' physical interactions (e.g. 'Two-hybrid').
#' @param und Character vector of interaction system names that should be 
#' treated as undirected (see Details).
#' 
#' @details 
#' Interactions in the BioGRID database are stored in an edge list format 
#' where each row corresponds to an observed interaction between two gene 
#' identifiers. Interactions are defined by the experimental system used to 
#' identify the relationship between two genes. Most of these experimental 
#' systems can be categorized into genetic interactions, which can be negative 
#' or positive, and physical interactions. \cr\cr
#' 
#' \bold{Undirected interactions:} \cr
#' Some experimental systems such as 'Synthetic Lethality' (SL) involve the 
#' deletion of either interacting gene, which makes them undirected since 
#' \code{A_mutant <-SL-> B_mutant} is equivalent to 
#' \code{B_mutant <-SL-> A_mutant}. For this reason, the negative interaction 
#' \code{A -> B} can be aggregated with the negative interaction \code{B -> A}. 
#' In this case the resulting table will include a row for \code{A -> B} and 
#' \code{B -> A}. Interaction systems such as 'Synthetic Dosage Lethality' are 
#' directed because \code{A_overexpressed <-SDL-> B_mutant} is not equivalent to
#' \code{B_overexpressed <-SDL-> A_mutant}. By default, such directed 
#' interaction systems are aggregated into the category 'other'. For
#' growth based experimental systems nodes would ideally be based on 
#' perturbation rather than gene, which would allow one to easily aggregate 
#' \code{A_perturbation <-neg/pos-> B_purturbation} with 
#' \code{B_perturbation <-neg/pos-> A_perturbation}.
#' 
#' @examples \dontrun{
#' src_biogridr() %>%
#'   outer_net('CTF4') %>%
#'   aggregate
#' }
#' 
#' @importFrom stats aggregate
#' @importFrom tidyr spread
#' @importFrom dplyr setdiff %>% filter select mutate bind_rows count ungroup 
#' arrange collect
#' @export
#' 
aggregate.tbl_biogridr <- function(x, neg = NULL, pos = NULL, phy = NULL, und = NULL) {
  
  if (inherits(x, 'tbl_sqlite')) x <- collect(x)
  
  # Define default categories
  pos <- pos %||% c('Synthetic Rescue', 'Positive Genetic')
  neg <- neg %||% c('Negative Genetic', 'Synthetic Growth Defect', 'Synthetic Lethality')
  phy <- phy %||% c('Affinity Capture-Luminescence', 'Affinity Capture-MS',
                    'Affinity Capture-Western', 'Co-fractionation',
                    'Co-localization', 'Co-purification', 'FRET', 'PCA',
                    'Two-hybrid', 'Biochemical Activity', 'Co-crystal Structure',
                    'Far Western', 'Protein-peptide', 'Reconstituted Complex')
  und <- und %||% c(neg, pos, phy)
  oth <- setdiff(x[['system']], c(pos, neg, phy))
  
  # initialize column variables
  negative = NULL
  positive = NULL
  physical = NULL
  other    = NULL
  
  # Undirected interactions will be combined forming a <-n-> b, and b <-n-> a
  undirected <- x %>% 
    filter(system %in% und) %>%
    select(a = b, b = a, system)
  
  # aggregate table of interactions
  out <- 
    x %>%
    select(a, b, system) %>%
    bind_rows(undirected) %>%
    mutate(type = ifelse(system %in% neg, 'negative',
                  ifelse(system %in% pos, 'positive',
                  ifelse(system %in% phy, 'physical', 'other')))) %>%
    select(a, b, type) %>%
    count(a, b, type) %>%
    ungroup %>%
    spread(type, n, fill = 0) %>%
    mutate(
      negative = negative %||% 0,  # ensure existence of columns
      positive = positive %||% 0,
      physical = physical %||% 0,
      other    = other    %||% 0,
      score = positive - negative) %>%
    select(a, b, score, negative, positive, physical, other) %>%
    arrange(a, score, b) 
  
  out %>%
    assign_class('tbl_biogridr_aggregated', 'tbl_df', 'tbl', 'data.frame') %>%
    assign_attr('net', 'custom') %>%
    assign_attr('group_definitions', list(undirected = und, negative = neg, 
                                          positive = pos, physical = phy, 
                                          other = oth))
}

#' @export
aggregate.tbl_biogridr_outer <- function(x, ...) {
  
  agg <- x %>%
    drop_class() %>%
    aggregate(...)
  
  agg %>%
    filter(a %in% attr(x, 'genes')) %>%
    assign_class(class(agg)) %>%
    assign_attr('group_definitions', attr(agg, 'group_definitions')) %>%
    assign_attr('net', 'outer') %>%
    assign_attr('genes', attr(x, 'genes'))
}

#' @export
aggregate.tbl_biogridr_inner <- function(x, ...) {
  
  agg <- x %>%
    drop_class() %>%
    aggregate(...) %>%
    assign_attr('net', 'inner') %>%
    assign_attr('genes', attr(x, 'genes'))
    
}

#=========== inner_net ==============
#' Select edges connecting a set of nodes
#' 
#' @description 
#' This is a generic function for selecting network edges connecting 
#' specified nodes. This "inner" network, where all edges connect only a set of 
#' nodes, can be contrasted to an "outer" network which contains all edges 
#' departing a set of nodes.
#' 
#' @seealso \link{outer_net}
#' @name inner_net
#' @export
#' 
inner_net <- function(src, ...) UseMethod('inner_net', src)

#' @param src An interaction network source (e.g. the result of \code{src_biogridr()}).
#' @param genes Gene identifiers (i.e. nodes).
#' @param org NCBI organism IDs used to filter BioGRID interactions. Defaults to 
#' \code{organism('cerevisiae')}.
#' @param ... other arguments passed to methods. For \code{src_biogridr} method, 
#' additional arguments are passed to \link[dplyr]{filter}.
#'
#' @examples \dontrun{
#' genes <- c('CTF4', 'TOF1')
#' src_biogridr() %>% inner_net(genes)
#' src_biogridr() %>% outer_net(genes)
#' }
#' 
#' @rdname inner_net
#' @importFrom dplyr %>% tbl filter collect
#' @export
#' 
inner_net.src_biogridr <- function(src, genes, org = organism('cerevisiae'), ...) {
  
  # This is a hack to get around https://github.com/hadley/dplyr/issues/511
  genes <- c(genes[1], genes)
  org <- c(org[1], org)
  
  src %>% tbl('interactions') %>%
    filter(
      (a %in% genes |
         a_systematic %in% genes |
         a_entrezgene %in% genes |
         a_biogrid %in% genes),
      (b %in% genes |
         b_systematic %in% genes |
         b_entrezgene %in% genes |
         b_biogrid %in% genes),
      (a_organism %in% org),
      (b_organism %in% org),
      ...) %>%
    collect %>%
    add_class('tbl_biogridr') %>%
    add_class('tbl_biogridr_inner') %>%
    assign_attr('genes', genes) %>%
    assign_attr('organisms', org)
}

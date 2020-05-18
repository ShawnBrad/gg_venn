#' @export
gg_venn<- function(overlap, ...){
  
  if (class(overlap) != 'list') stop('please enter a list')
  if (is.na(names(overlap))) names(overlap)= c('A','B','C','D')[1:length(overlap)]

  if (length(overlap) == 2){
    setup.list(overlap = overlap) %>%
      venn_2way(overlap = overlap, ...) %>%
      return()

  } else if (length(overlap) == 4){
    setup.list(overlap = overlap) %>%
      venn_4way(overlap = overlap, ...) %>%
      return()
  }
}

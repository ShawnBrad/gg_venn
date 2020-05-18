gg_venn<- function(overlap, ...){

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

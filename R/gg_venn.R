#' Creates a venn diagram from a supplied named list of 2 to 4
#'
#' @param overlap named list containing the elements to draw venn from
#' @param label.col  color or appropriately lengthed vector of colors to label venn
#' @param label.col color to label venns
#' @param label.venns TRUE or FALSE should individual venns be labeled 
#' 
#' 
#' @examples
#' gg_venn(list(group1 = c('a','b', 'c'), group2 = c('c','d','e')))
#' 
#' 
#' @export
gg_venn<- function(overlap,  label.col = rep('black',15),label.size = 6){
  
  if (class(overlap) != 'list') stop('please enter a list')
  if (length(names(overlap)) != length(overlap)) names(overlap)= c('A','B','C','D')[1:length(overlap)]

  if (length(overlap) == 2){
    setup.list(overlap = overlap) %>%
      venn_2way(overlap = overlap, label.col  = label.col , label.size = label.size) %>%
      return()
    
  } else if (length(overlap) == 3){
      setup.list(overlap = overlap) %>%
        venn_3way(overlap = overlap, label.col  = label.col ,label.size = label.size) %>%
        return()
      
      

  } else if (length(overlap) == 4){
    setup.list(overlap = overlap) %>%
      venn_4way(overlap = overlap,label.col  = label.col, label.size = label.size) %>%
      return()
  }
}

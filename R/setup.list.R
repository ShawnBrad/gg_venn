#' @import tidyverse
#' @import limma
#' @importFrom magrittr %>%
#' 
#' 

setup.list <- function(overlap){
# get venn counts using limma
overlap.df <- make.table(overlap)   %>%
    limma::vennCounts()

    class(overlap.df) <- 'matrix'
    colnames(overlap.df) <- c(names(overlap), 'Counts')

    return(overlap.df)
}



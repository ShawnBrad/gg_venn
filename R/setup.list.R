#' @import tidyverse
#' @import limma
#' @importFrom magrittr %>%

setup.list <- function(overlap){

# get all genes present in list
shared <- c()
for (iter in 1:length(overlap)){
  shared = union(shared,unlist(overlap[iter], use.names = F))
}
# create a table with genes
shared <- enframe(shared,value ='Genes',name = NULL)

# check if gene belongs to the seperate list items
overlap.df <- data.frame(dummy = seq(1, to = dim(shared)[1], by = 1))

#add row names
row.names(overlap.df) <- pull(shared,Genes)

for (iter in 1:length(overlap)){
  overlap.df <- add_column(overlap.df, row.names(overlap.df) %in% unlist(overlap[iter], use.names = F))
}

# remove dummy variable
overlap.df <- overlap.df[,-1]

# add col names
colnames(overlap.df) <- names(overlap)

# get venn counts using limma
overlap.df <- overlap.df   %>%
    limma::vennCounts()

    class(overlap.df) <- 'matrix'
    colnames(overlap.df) <- c(names(overlap), 'Counts')

    return(overlap.df)
}



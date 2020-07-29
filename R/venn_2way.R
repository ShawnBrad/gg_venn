#' @import ggplot2
#' @import ggforce
#' @importFrom magrittr %>%

venn_2way <- function(overlap.df, overlap, label.col,label.size ){

  
  if( length(label.col) < 3 &  length(label.col) != 1) {
    label.col = rep(label.col[1],3)
    warning('insufficent colors supplied, using the first supplied color only',call. = F)
    
  } else if( length(label.col) > 3) {
    label.col = label.col[1:3]
  }

  # determine scale for circles
  len.list <- c(length(unlist(overlap[[2]])), length(unlist(overlap[[1]])))
  len.list <- len.list/max(len.list)

  # cap lower bound of scaled circle to 10%
   if(min(len.list) < .1 ){
     len.list[which(len.list == min(len.list))] = .1
   }

  # create df to draw circles
  df.venn <- data.frame(x = c(0, 1)- min(len.list)/4,
                        y = c(0, 0),
                        venn.names = names(overlap))


  overlap.df <- as.data.frame(overlap.df[-1,]) %>%
    mutate(x = c(1.2+min(len.list)/4, .95 - min(len.list)/2, 0 - min(len.list)/2), y = c(0, 0, 0))

  # reverse x positions depending on which list is larger
  if(len.list[1] < len.list[2]){
    overlap.df$x = .95+ (overlap.df$x * -1)
  }


# plot venn
  base_venn <- ggplot(df.venn, aes(x0 = x, y0 = y, r = len.list, fill = venn.names)) +
    geom_circle(alpha = .5, size = 1, colour = 'grey50') +
    annotate("text", x = overlap.df$x, y = overlap.df$y, 
             label = overlap.df$Counts[c(1,3,2)], 
             size = 6, color = label.col[c(1,2,3)])

  base_venn = base_venn +
    theme_void() %>%
    return() 
}






#' @import ggplot2
#' @import ggforce
#' @importFrom magrittr %>%

venn_4way <- function(overlap.df, overlap, label.size, label.col, line.width ){

  if( length(label.col) < 15 &  length(label.col) != 1) {
    label.col = rep(label.col[1],15)
    warning('insufficent colors supplied, using the first supplied color only',call. = F)
    
  } else if( length(label.col) > 15) {
    label.col = label.col[1:15]
  }
  
  df <- data.frame(angles = c(5*pi/6, pi/6, 11*pi/6,7*pi/6),
                   x0 = c(-2,2,0,0),
                   y0 =  c(0,0,2,2),
                   a = rep(5,4),
                   b =  c(3,3,3,3),
                   venn.names = names(overlap))

  venn.coordinates <- data.frame(
    v.x = c(3,-3,0,5,4,3.5,2,-5,-3.5,-4,-2,0,-1.75,1.75,0),
    v.y = c(5,5,3.5,2,3.2,0,2,2,0,3.2,2,-2.5,-1,-1,.5))

  venn.df <- overlap.df[-1,] %>%
    as.data.frame() %>%
    add_column(venn.coordinates)

 base_venn <-  ggplot(df, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angles, fill = venn.names, color = venn.names)) +
    geom_ellipse(alpha= .4, size = line.width)+
    annotate("text",
             x = pull(venn.df, v.x),
             y = pull(venn.df, v.y),
             label = pull(venn.df, Counts),
             #label = c('d','c','cd','b','bd','bc','bcd','a','ad','ac','acd','ab','abd','abc','abcd'),
             size =label.size,
             color = label.col) 
 

 base_venn = base_venn +
    theme_void() %>%
    return()
}



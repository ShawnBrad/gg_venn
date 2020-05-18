#' @import ggplot2
#' @import ggforce
#' @importFrom magrittr %>%

venn_4way <- function(overlap.df, overlap, text.col = rep('black',15),title.col = 'black'){

  if( length(text.col) != 15) (text.col = rep(text.col[1],15))
  df <- data.frame(angles = c(5*pi/6, pi/6, 11*pi/6,7*pi/6),
                   x0 = c(-2,2,0,0),
                   y0 =  c(0,0,2,2),
                   a = rep(5,4),
                   b =  c(3,3,3,3),
                   venn.names = names(overlap))

  venn.coordinates <- data.frame(
    v.x = c(3,-3,0,5,4,3.5,2,-5,-3.5,-4,-2,0,-1.75,1.75,0),
    v.y = c(5,5,3.5,2,3.2,0,2,2,0,3.2,2,-2.5,-1,-1,.5))

  labels.coordinates <- data.frame(
    v.x = c(-6,6,-3.5,3.5),
    v.y = c(5,5,7,7),
    names = names(overlap))

  venn.df <- overlap.df[-1,] %>%
    as.data.frame() %>%
    add_column(venn.coordinates)

 base_venn <-  ggplot(df, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angles, fill = venn.names)) +
    geom_ellipse(alpha= .4)+
    annotate("text",
             x = pull(venn.df, v.x),
             y = pull(venn.df, v.y),
             label = pull(venn.df, Counts),
             size = 6,
             color = text.col) 
if (label.venns){
  base_venn = base_venn +
  annotate("text",
           x = pull(labels.coordinates, v.x),
           y = pull(labels.coordinates, v.y),
           label = pull(labels.coordinates, names),
           size = 8,
           color = title.col)
  }
 
 base_venn = base_venn +
    theme_void()+
    theme(legend.position = 'none') %>%
    return()
}



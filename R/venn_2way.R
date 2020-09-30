#' @import ggplot2
#' @import ggforce
#' @importFrom magrittr %>%

venn_2way <- function(overlap.df, overlap, label.col,label.size , line.width ){

  
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
  
  # determine x coordinates 
  
  x.cords = c(0, 0 + len.list[1])
  y.cords = c(0, 0)

  # create df to draw circles
  df.venn <- data.frame(x =  x.cords,
                        y = y.cords,
                        venn.names = names(overlap)[c(2,1)])
  
  
  intercept <- circles.intersect(r = len.list, x = x.cords, y = y.cords)
  
  #circles.intersect(r = c(1,1), x = c(0, (0 + 1)), y = c(0,0))
  
  # get coords for labels 
  x.labs.cord = c((x.cords[1] - len.list[1]) + len.list[1]/2,  intercept[1],
                  (x.cords[2] + len.list[2]) - len.list[2]/2)
                  
  
    
    
  #circles.intersect(r = c(1,.5),x = c(0,.5), y = c(0,0))


  overlap.df <- as.data.frame(overlap.df[-1,]) %>%
    mutate(x =  x.labs.cord, y = c(0, 0, 0))


# plot venn
  base_venn <- ggplot(df.venn, aes(x0 = x, y0 = y, r = len.list, fill = venn.names, color = venn.names)) +
    geom_circle(alpha = .5, size = line.width) +
    
    
    annotate("text", x = overlap.df$x, y = overlap.df$y, 
             label = overlap.df$Counts[c(1,2,3)], 
             size = 6, color = label.col[c(1,2,3)])

  base_venn = base_venn +
    theme_void() %>%
    return() 
}






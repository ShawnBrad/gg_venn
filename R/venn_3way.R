#' @import ggplot2
#' @import ggforce
#' @importFrom magrittr %>%

venn_3way <- function(overlap.df, overlap, label.size, label.col){
  len.list <- c(length(unlist(overlap[[1]])), length(unlist(overlap[[2]])), length(unlist(overlap[[3]]))) # count items
  
  
  if (len.list[1] == len.list[2] & len.list[2] == len.list[3]){
    r1 = r2 = r3 = 1
    x1 = 0 ; y1 = 0;
    x2 = 0 + r1; y2 = y1
    x3 = (x1 + x2)/2 ; y3 = mean(y1,y2) + r3
    
   } else {
     if (min(len.list)/max(len.list)< .1) lower = .1 
     if (min(len.list)/max(len.list)> .1) lower = min(len.list)/max(len.list)
     
      len.list <- scales::rescale(len.list, c(lower, 1))
      r1 = len.list[1]
      r2 = len.list[2]
      r3 = len.list[3]
      
      
      
 ######     # if circle 1 = ref ######
      if (len.list[1] == max(len.list)){
        # set 2nd circle to 3
        
        if(r2 < r3){
          x1 = 0 ; y1 = 0;
          x3 = 0+ r1; y3 = 0;
          
          circle.center <- circles.intersect(r = c(r1,r3),
                                             x = c(x1,x3),
                                             y = c(y1,y3))
          x2 = circle.center[1]
          y2 = circle.center[2]
          
          cr = sqrt(r2/2)/2
          
          label.cords <- rbind(  c(x3 + r3/2, y3 ), #c =.
                                 c(x2 , y2 + cr ), # b = .
                                 c(x2 + cr, y2 ), #bc = .
                                 c(x1 -r1/2, y1 ), # a = .
                                 c(x2 - cr, y2 ), # ac = .
                                 c(x3 - r3/2, y2 - 1.5*r2), #ab = .
                                 c(x2 , y2 - cr)) # abc = .
          
        }  else { # set secondary to 2
          
          x1 = 0 ; y1 = 0;
          x2 = 0+ r1; y2 = 0;
          
          circle.center <- circles.intersect(r = c(r1,r2),
                                             x = c(x1,x2),
                                             y = c(y1,y2))
          x3 = circle.center[1]
          y3 = circle.center[2]
          
          
          cr = sqrt(r3/2)/2
          
          label.cords <- rbind(  c(x3 , y3 + cr), #c =
                                 c(x2 + r2/2, y2 ), # b = 
                                 c(x3 + cr, y3 - r3/8 ), #bc = 
                                 c(x1 -r1/2, y1 ), # a = .
                                 c(x3 - cr, y3 ), # ac = .
                                 c(x2 - r2/2, y3 - r3*1.2), #ab = 
                                 c(x3 -r3/8 , y3 - cr)) # abc = 
          
          
        }
      }
      
      
######      # if circle 2 = ref ######
      if (len.list[2] == max(len.list)){
        
        # set 2nd circle to 3
        if(r1 < r3){
          x2 = 0 ; y2 = 0;
          x3 = 0+ r2; y3 = 0;
          
          circle.center <- circles.intersect(r = c(r2,r3),
                                             x = c(x2,x3),
                                             y = c(y2,y3))
          x1 = circle.center[1]
          y1 = circle.center[2]
          
          cr = sqrt(r1/2)/2
          
          label.cords <- rbind(  c(x3 + r3/2, y3 ), #c =
                                 c(x2 - r2/2, y2), # b = 
                                 c(x3 - r3/2, y3), #bc =
                                 c(x1 , y1 + cr), # a = 
                                 c(x1 + cr, y1 ), # ac = 
                                 c(x1 - cr, y1 ), #ab = 
                                 c(x1 , y1 - cr)) # abc = 
        
          
          
        } else { # set secondary to 1
          
          x2 = 0; y2 = 0;
          x1 = 0 + r2; y1 = 0;
          
          circle.center <- circles.intersect(r = c(r2,r1),
                                             x = c(x2,x1),
                                             y = c(y2,y1))
          x3 = circle.center[1]
          y3 = circle.center[2]
          
          
          cr = sqrt(r3/2)/2
          
            
          label.cords <- rbind(  c(x3, y3+ cr ), #c =
                              c(x2 - r2/2, y2), # b = 
                                c(x3 -cr, y3 ), #bc =
                               c(x1 + r1/2, y1), # a = 
                               c(x3 +cr, y3 - r3/8 ), # ac = 
                               c(x1 - r1/2, y1 - r1/2), #ab = 
                               c(x3 -r3/8 , y3 - cr)) # abc = 
         
        } 
        
      }
      
      
#####      # if circle 3 = ref ######
      if (len.list[3] == max(len.list)){
        
        # second to 2 
        if(r1 < r2){
          x3 = 0 ; y3 = 0;
          x2 = 0 + r3; y2 = 0;
          
          circle.center <- circles.intersect(r = c(r3,r2),
                                             x = c(x3,x2),
                                             y = c(y3,y2))
          x1 = circle.center[1]
          y1 = circle.center[2]
          
          cr = sqrt(r1/2)/2
          
          label.cords <- rbind(  c(x3 - r3/2, y3 ), #c =
                                 c(x2 + r2/2, y2), # b = .
                                 c(x2 - r2/2, y2 -r2/8), #bc = .
                                 c(x1 , y1+ cr ), # a = 
                                 c(x1 - cr, y1 ), # ac = .
                                 c(x1 +cr *1.25, y1 -r1/8 ), #ab = .
                                 c(x1, y1 - cr)) # abc = .
          
        } else {# set 2nd to 1 
          
          x3 = 0; y3 = 0;
          x1 = 0 + r3; y1 = y3
          
          
          circle.center <- circles.intersect(r = c(r3,r1),
                                             x = c(x3,x1),
                                             y = c(y3,y1))
          x2 = circle.center[1]
          y2 = circle.center [2]
          
          cr = sqrt(r2/2)/2
          
          label.cords <- rbind(  c(x3 - r3/2, y1 ), #c =
                                 c(x2 , y2 + cr), # b = .
                                 c(x2 + cr, y2 -r2/8 ), #bc = .
                                 c(x1 + r1/2, y1 ), # a = 
                                 c(x1 - r1/2, y2 - 1.25*r2 ), # ac = .
                                 c(x2 - cr, y2 ), #ab = .
                                 c(x2- r2/8 , y2 - cr)) # abc = .
        }
      }
      
    }
######## plot ##############
  df.venn <- data.frame(x = c(x1,x2,x3),
                        y = c(y1,y2,y3),
                        groups = names(overlap))
  
  
  c.labels  <-  data.frame(x.l =  label.cords[,1],
               y.l =  label.cords[,2],
               label.l = overlap.df[-1,4])#,
               #label.l = c('c', 'b', 'bc', 'a',  'ac', 'ab', 'abc' ))

 
  base_venn <-   ggplot(df.venn, aes(x0 = x, y0 = y, r =  len.list, fill = groups)) +
  geom_circle(alpha = .5, size = 1, colour = 'grey50')+
  annotate("text",
           x =  c.labels$x.l ,
           y =   c.labels$y.l,
           #label =  c.labels$label.l ,
           label = c('c', 'b', 'bc', 'a',  'ac', 'ab', 'abc' ),
          size = label.size,
           color = 'blue')#label.col)

  base_venn = base_venn +
    theme_void() %>%
    return() 
}
  
  


# create function to check where the circles intersect 
circles.intersect <- function(r, x,y ){
  # circle radius
  r0 = r[1] 
  r1 = r[2]
  # x coords
  x0 = x[1]
  x1 = x[2]
  # y coords
  y0 = y[1]
  y1 = y[2]
  
  d <- abs(x1 - x0)
 
  # find interct of circles using triangles method

  a = ( r0^2 - r1^2 + d^2 )/2*d 
  h = sqrt( r0^2 -a^2 )
  
  x2 = x0 + a
  
  y2 = y0 + (a* (y1 = y0)/d)
 
  
  
  
  x3 = x2 + (-h*( y1 - y0)/d)
  y3 = abs (y2 - (h* (x1 - x0 )/d))
  return(c(x3,y3))
}

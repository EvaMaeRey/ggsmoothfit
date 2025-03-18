library(statexpress)

stat_smooth_fit <- function(geom = "point", ...){
  
  qlayer(geom = geom,
         stat = StatSmoothFit, ...)
  
}


geom_smooth_fit <- function(...){
  
  qlayer(geom = qproto_update(GeomPoint, aes(colour = from_theme(accent))),
         stat = StatSmoothFit, ...)
  
}

geom_smooth_residuals <- function(...){
  
  qlayer(geom = qproto_update(GeomSegment, aes(colour = from_theme(accent))),
         stat = StatSmoothFit, ...)
  
}

mtcars %>% 
  ggplot() + 
  aes(x = wt, y = mpg) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth_fit() + 
  geom_smooth_residuals() + 
  geom_smooth_predict(xseq = 3, size = 8)


geom_squared_residuals <- function(...){
  
  qlayer(geom = qproto_update(GeomRect, 
                              aes(fill = from_theme(accent), 
                                  alpha = .2,
                                  color = from_theme(accent),
                                  linewidth = from_theme(linewidth*.2))),
         stat = StatSmoothErrorSq,
         ...)
  
}

standardize <- function(x){
  
  var_mean <- mean(x) 
  var_sd <- sd(x)
  
  (x-var_mean)/var_sd
  
}


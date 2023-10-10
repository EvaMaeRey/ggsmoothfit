geom_smooth_predict <- function(xseq,  mapping = NULL, data = NULL, ..., method = NULL, formula = NULL, se = TRUE, method.args = list(), na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE, color = "blue"){
  
  stat_smooth( mapping = mapping, data = data, geom = "point", position = "identity", xseq = xseq,  ..., method = method, formula = formula, se = se, method.args = list(), na.rm = na.rm, orientation = orientation, show.legend = show.legend, inherit.aes = inherit.aes, color = color
)
  
}

mtcars %>% 
  ggplot() + 
  aes(wt, mpg) +
  geom_point() +
  geom_smooth() + 
  geom_smooth_predict(xseq = 2:3)


mtcars %>% 
  ggplot() + 
  aes(wt, mpg) +
  geom_point() +
  geom_smooth(method = lm) + 
  geom_smooth_predict(xseq = 0,
                     method = lm, 
                     color = "red",
                     size = 4)

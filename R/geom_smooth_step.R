geom_smooth_step <- function(method = NULL, formula = y ~ x,
                              color = "darkred", xseq = 0:1){
  
  stat_smooth(method = method,
              formula = formula,
              geom = "segment", # draw fitted values as points
              color = color,
              xseq = xseq, # 'from', 'to' value pair
              aes(yend = after_stat(y[1]), # 'from' value of y
                  xend = xseq[2]), # 'to' value of x
              arrow = arrow(ends = c("last", "first"), 
                            length = unit(.1, "in")))
} 

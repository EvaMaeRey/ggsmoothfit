compute_group_smooth_fit <- function(data, scales, method = NULL, formula = NULL,
                           se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                           level = 0.95, method.args = list(),
                           na.rm = FALSE, flipped_aes = NA, xseq = NULL){
  
  if(is.null(xseq)){
    xseq <- data$x
    was_null = T
    }else{was_null = F}
  
  out <- ggplot2::StatSmooth$compute_group(data = data, scales = scales, 
                       method = method, formula = formula, 
                       se = FALSE, n= n, span = span, fullrange = fullrange,
                       xseq = xseq, 
                       level = .95, method.args = method.args, 
                       na.rm = na.rm, flipped_aes = flipped_aes) 
  
  if(was_null){
  out$x_obs <-  data$x
  out$y_obs <- data$y
  }
  
  out$xend <- out$x_obs
  out$yend <- out$y_obs
  
  out
  
}

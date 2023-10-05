compute_group_smooth_sq_error <- function(data, scales, method = NULL, formula = NULL,
                           se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                           level = 0.95, method.args = list(),
                           na.rm = FALSE, flipped_aes = NA){
  
 compute_group_smooth_fit(data = data, scales = scales, 
                       method = method, formula = formula, 
                       se = FALSE, n= n, span = span, fullrange = fullrange,
                       level = .95, method.args = method.args, 
                       na.rm = na.rm, flipped_aes = flipped_aes) %>% 
    dplyr::mutate(ymin = y,
           xmin = x,
           ymax = yend,
           xmax = x + (ymax - ymin))
  
}

StatSmoothFit <- ggplot2::ggproto("StatSmoothFit", 
                                  ggplot2::StatSmooth,
                                  compute_group = compute_group_smooth_fit,
                                  required_aes = c("x", "y"))

StatSmoothErrorSq <- ggplot2::ggproto("StatSmoothErrorSq", 
                                      ggplot2::StatSmooth,
                                      compute_group = compute_group_smooth_sq_error,
                                      required_aes = c("x", "y"))

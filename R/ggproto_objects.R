StatSmoothFit <- ggplot2::ggproto("StatSmoothFit", 
                                  ggplot2::StatSmooth,
                                  compute_group = compute_group_smooth_fit,
                                  default_aes = ggplot2::aes(xend = after_stat(x_obs), 
                                                             yend = after_stat(y_obs)))

StatSmoothErrorSq <- ggplot2::ggproto("StatSmoothErrorSq", 
                                      ggplot2::StatSmooth,
                                      compute_group = compute_group_smooth_sq_error)

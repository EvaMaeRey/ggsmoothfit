StatSmoothFit <- ggplot2::ggproto("StatSmoothFit", 
                                  ggplot2::StatSmooth,
                                  compute_group = compute_group_smooth_fit)

StatSmoothErrorSq <- ggplot2::ggproto("StatSmoothErrorSq", 
                                      ggplot2::StatSmooth,
                                      compute_group = compute_group_smooth_sq_error)

StatSmoothFit <- ggplot2::ggproto("StatSmoothFit", ggplot2::Stat,
  setup_params = ggplot2::StatSmooth$setup_params,
  extra_params = c("na.rm", "orientation"),
  compute_group = compute_group_smooth_fit,
  dropped_aes = c("weight"),
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(xend = after_stat(x_obs), yend = after_stat(y_obs))

)

StatSmoothErrorSq <- ggplot2::ggproto("StatSmoothErrorSq", ggplot2::Stat,
  setup_params = ggplot2::StatSmooth$setup_params,
  extra_params = c("na.rm", "orientation"),
  compute_group = compute_group_smooth_sq_error,
  dropped_aes = c("weight"),
  required_aes = c("x", "y")
)

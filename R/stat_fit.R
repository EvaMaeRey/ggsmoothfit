stat_fit <- function(mapping = NULL, data = NULL,
            geom = "point", position = "identity",
            ...,
            method = NULL,
            formula = NULL,
            se = TRUE,
            n = 80,
            span = 0.75,
            fullrange = FALSE,
            level = 0.95,
            method.args = list(),
            na.rm = FALSE,
            orientation = NA,
            show.legend = NA,
            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFit,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      span = span,
      ...
    )
  )
}

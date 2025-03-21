
- [ggsmoothfit](#ggsmoothfit)
- [Let’s build this functionality](#lets-build-this-functionality)
- [Step 0. Examine ggplot2::StatSmooth\$compute_group, and a dataframe
  that it
  returns](#step-0-examine-ggplot2statsmoothcompute_group-and-a-dataframe-that-it-returns)
- [Step 1. create
  compute_group_smooth_fit](#step-1-create-compute_group_smooth_fit)
- [Step 1.1 test compute group](#step-11-test-compute-group)
- [Step 2. Pass to ggproto](#step-2-pass-to-ggproto)
- [Try Out Stat](#try-out-stat)
- [Step 3. Pass to stat\_\*/ geom\_
  functions](#step-3-pass-to-stat_-geom_-functions)
- [And with lm](#and-with-lm)
- [Contrast to an empty model…](#contrast-to-an-empty-model)
- [Via @friendly ggplot2 extenders ggsprings discussion and springs
  extension case
  study](#via-friendly-ggplot2-extenders-ggsprings-discussion-and-springs-extension-case-study)
- [Part 2. Packaging and documentation 🚧
  ✅](#part-2-packaging-and-documentation--)
  - [minimal requirements for github package. Have
    you:](#minimal-requirements-for-github-package-have-you)
    - [Created files for package archetecture with
      `devtools::create("./ggbarlabs")`
      ✅](#created-files-for-package-archetecture-with-devtoolscreateggbarlabs-)
    - [Moved functions R folder? ✅](#moved-functions-r-folder-)
    - [Added roxygen skeleton? ✅](#added-roxygen-skeleton-)
    - [Managed dependencies ? ✅](#managed-dependencies--)
    - [Chosen a license? ✅](#chosen-a-license-)
  - [`devtools::check()` report](#devtoolscheck-report)
- [Don’t want to use ggsmoothfit? Here are some ways to get it done with
  base
  ggplot2!](#dont-want-to-use-ggsmoothfit-here-are-some-ways-to-get-it-done-with-base-ggplot2)
  - [Option 1. Verbal description and move
    on…](#option-1-verbal-description-and-move-on)
  - [Option 2: precalculate and plot](#option-2-precalculate-and-plot)
  - [Option 3: little known xseq argument and geom =
    “point”](#option-3-little-known-xseq-argument-and-geom--point)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggsmoothfit

<!-- badges: start -->
<!-- badges: end -->

The goal of {ggsmoothfit} is to let you visualize model fitted values
and residuals easily!

``` r
library(tidyverse, warn.conflicts = F)
library(ggsmoothfit)
mtcars %>% 
  ggplot() + 
  aes(wt, mpg) +
  geom_point() +
  geom_smooth() +
  ggsmoothfit:::geom_fit() + 
  ggsmoothfit:::geom_residuals() + 
  ggsmoothfit:::geom_smooth_fit(xseq = 2:3, size = 5) +
  ggsmoothfit:::geom_smooth_step(xseq = 2:3) + 
  ggsmoothfit:::geom_smooth_fit(xseq = 0, size = 5, method = lm)
```

# Let’s build this functionality

# Step 0. Examine ggplot2::StatSmooth\$compute_group, and a dataframe that it returns

Key take away: this function allows you to set values of x with the
*xseq argument*. Although the default is to create an evenly spaced
sequence.

``` r
ggplot2::StatSmooth$compute_group %>% capture.output() %>% .[1:10]
#>  [1] "<ggproto method>"                                                       
#>  [2] "  <Wrapper function>"                                                   
#>  [3] "    function (...) "                                                    
#>  [4] "compute_group(...)"                                                     
#>  [5] ""                                                                       
#>  [6] "  <Inner function (f)>"                                                 
#>  [7] "    function (data, scales, method = NULL, formula = NULL, se = TRUE, " 
#>  [8] "    n = 80, span = 0.75, fullrange = FALSE, xseq = NULL, level = 0.95, "
#>  [9] "    method.args = list(), na.rm = FALSE, flipped_aes = NA) "            
#> [10] "{"

library(dplyr)
mtcars %>%
  rename(x = wt, y = mpg, cat = am) %>%
  ggplot2::StatSmooth$compute_group(method = lm, 
                           formula = y ~ x, n = 7)
#>          x         y      ymin     ymax        se flipped_aes
#> 1 1.513000 29.198941 26.963760 31.43412 1.0944578          NA
#> 2 2.164833 25.715236 24.086350 27.34412 0.7975850          NA
#> 3 2.816667 22.231531 21.040552 23.42251 0.5831635          NA
#> 4 3.468500 18.747827 17.611376 19.88428 0.5564635          NA
#> 5 4.120333 15.264122 13.756629 16.77161 0.7381448          NA
#> 6 4.772167 11.780417  9.692002 13.86883 1.0225936          NA
#> 7 5.424000  8.296712  5.547468 11.04596 1.3461693          NA
```

# Step 1. create compute_group_smooth_fit

Here we’ll piggy back on StatSmooth\$compute_group, to create a
function, compute_group_smooth_fit. We ask that function to compute
predictions at the values of x observed in our data set. We also
preserve the values of y (as yend) so that we can draw in the residual
error.

xend and yend are computed to draw the segments visualizing the error.

``` r
compute_group_smooth_fit <- function(data, scales, method = NULL, formula = NULL,
                           xseq = NULL,
                           level = 0.95, method.args = list(),
                           na.rm = FALSE, flipped_aes = NA){
  
  if(is.null(xseq)){ # predictions based on observations 

  ggplot2::StatSmooth$compute_group(data = data, scales = scales, 
                       method = method, formula = formula, 
                       se = FALSE, n= 80, span = 0.75, fullrange = FALSE,
                       xseq = data$x, 
                       level = .95, method.args = method.args, 
                       na.rm = na.rm, flipped_aes = flipped_aes) |>
      dplyr::mutate(xend = data$x,
                    yend = data$y)
  
  }else{  # predict specific input values
    
  ggplot2::StatSmooth$compute_group(data = data, scales = scales, 
                       method = method, formula = formula, 
                       se = FALSE, n= 80, span = 0.75, fullrange = FALSE,
                       xseq = xseq, 
                       level = .95, method.args = method.args, 
                       na.rm = na.rm, flipped_aes = flipped_aes)   
    
  }
  
}
```

``` r
mtcars %>% 
  slice(1:10) %>% 
  rename(x = wt, y = mpg) %>% 
  compute_group_smooth_fit(method = lm, formula = y ~ x)
#>        x        y flipped_aes  xend yend
#> 1  2.620 22.54689          NA 2.620 21.0
#> 2  2.875 21.45416          NA 2.875 21.0
#> 3  2.320 23.83246          NA 2.320 22.8
#> 4  3.215 19.99719          NA 3.215 21.4
#> 5  3.440 19.03301          NA 3.440 18.7
#> 6  3.460 18.94731          NA 3.460 18.1
#> 7  3.570 18.47593          NA 3.570 14.3
#> 8  3.190 20.10432          NA 3.190 24.4
#> 9  3.150 20.27573          NA 3.150 22.8
#> 10 3.440 19.03301          NA 3.440 19.2
```

We’ll also create compute_group_smooth_sq_error, further piggybacking,
this time on the function we just build. This creates the ymin, ymax,
xmin and xmax columns needed to show the *squared* error. Initially, I’d
included this computation above, but the plot results can be bad, as the
‘flags’ that come off of the residuals effect the plot spacing even when
they aren’t used. Preferring to avoid this side-effect, we create two
functions (and later two ggproto objects). Note too that xmax is
computed in the units of y, and initial plotting can yield squares that
do not look like squares. Standardizing both variables, with coord_equal
will get us to squares.

``` r
compute_group_smooth_sq_error <- function(data, scales, method = NULL, 
                                          formula = NULL,
                          
                           level = 0.95, method.args = list(),
                           na.rm = FALSE, flipped_aes = NA){
  
 compute_group_smooth_fit(data = data, scales = scales, 
                       method = method, formula = formula, 
                       level = .95, method.args = method.args, 
                       na.rm = na.rm, flipped_aes = flipped_aes) %>% 
    dplyr::mutate(ymin = y,
           xmin = x,
           ymax = yend,
           xmax = x + (ymax - ymin))
  
}
```

# Step 1.1 test compute group

``` r
mtcars %>% 
  slice(1:10) %>% 
  rename(x = wt, y = mpg) %>% 
  compute_group_smooth_fit(method = lm, formula = y ~ x)
#>        x        y flipped_aes  xend yend
#> 1  2.620 22.54689          NA 2.620 21.0
#> 2  2.875 21.45416          NA 2.875 21.0
#> 3  2.320 23.83246          NA 2.320 22.8
#> 4  3.215 19.99719          NA 3.215 21.4
#> 5  3.440 19.03301          NA 3.440 18.7
#> 6  3.460 18.94731          NA 3.460 18.1
#> 7  3.570 18.47593          NA 3.570 14.3
#> 8  3.190 20.10432          NA 3.190 24.4
#> 9  3.150 20.27573          NA 3.150 22.8
#> 10 3.440 19.03301          NA 3.440 19.2

mtcars %>% 
  slice(1:10) %>% 
  rename(x = wt, y = mpg) %>% 
  compute_group_smooth_sq_error(method = lm, formula = y ~ x)
#>        x        y flipped_aes  xend yend     ymin  xmin ymax       xmax
#> 1  2.620 22.54689          NA 2.620 21.0 22.54689 2.620 21.0  1.0731060
#> 2  2.875 21.45416          NA 2.875 21.0 21.45416 2.875 21.0  2.4208382
#> 3  2.320 23.83246          NA 2.320 22.8 23.83246 2.320 22.8  1.2875387
#> 4  3.215 19.99719          NA 3.215 21.4 19.99719 3.215 21.4  4.6178145
#> 5  3.440 19.03301          NA 3.440 18.7 19.03301 3.440 18.7  3.1069900
#> 6  3.460 18.94731          NA 3.460 18.1 18.94731 3.460 18.1  2.6126945
#> 7  3.570 18.47593          NA 3.570 14.3 18.47593 3.570 14.3 -0.6059308
#> 8  3.190 20.10432          NA 3.190 24.4 20.10432 3.190 24.4  7.4856839
#> 9  3.150 20.27573          NA 3.150 22.8 20.27573 3.150 22.8  5.6742749
#> 10 3.440 19.03301          NA 3.440 19.2 19.03301 3.440 19.2  3.6069900
```

# Step 2. Pass to ggproto

``` r
StatSmoothFit <- ggplot2::ggproto("StatSmoothFit", 
                                  ggplot2::StatSmooth,
                                  compute_group = compute_group_smooth_fit,
                                  required_aes = c("x", "y"))

StatSmoothErrorSq <- ggplot2::ggproto("StatSmoothErrorSq", 
                                      ggplot2::StatSmooth,
                                      compute_group = compute_group_smooth_sq_error,
                                      required_aes = c("x", "y"))
```

# Try Out Stat

``` r
mtcars %>% 
  ggplot() + 
  aes(x = wt, y = mpg) + 
  geom_point() + 
  geom_smooth() + 
  geom_point(stat = StatSmoothFit, color = "blue") + 
  geom_segment(stat = StatSmoothFit, color = "blue")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Step 3. Pass to stat\_\*/ geom\_ functions

``` r
library(statexpress)

stat_smooth_fit <- function(geom = "point", ...){
  
  qlayer(geom = geom,
         stat = StatSmoothFit, ...)
  
}


geom_smooth_fit <- function(...){
  
  qlayer(geom = qproto_update(GeomPoint, aes(colour = from_theme(accent))),
         stat = StatSmoothFit, ...)
  
}

geom_smooth_residuals <- function(...){
  
  qlayer(geom = qproto_update(GeomSegment, aes(colour = from_theme(accent))),
         stat = StatSmoothFit, ...)
  
}

mtcars %>% 
  ggplot() + 
  aes(x = wt, y = mpg) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth_fit() + 
  geom_smooth_residuals() + 
  geom_smooth_fit(xseq = 2:3, size = 8)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](README_files/figure-gfm/stat_fit-1.png)<!-- -->

``` r
geom_smooth_residuals_squared <- function(...){
  
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
```

For best results, use standardized x, y and coord_equal() as shown below

``` r
mtcars %>% 
  ggplot() + 
  aes(x = wt, y = mpg) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth_fit() + 
  geom_smooth_residuals() +
  geom_smooth_residuals_squared()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r

last_plot() + 
  coord_equal()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r

last_plot() +
  aes(standardize(wt), standardize(mpg)) 
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

# And with lm

``` r
mtcars %>% 
  ggplot() +
  aes(wt, mpg) + 
  geom_point() +
  geom_smooth(alpha = .2, se = FALSE, method = lm) + 
  geom_smooth_fit(method = lm) + # wrap as geom_smooth_fit()
  geom_smooth_residuals(method = lm) + 
  geom_smooth_fit(xseq = 0, method = lm)
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Contrast to an empty model…

- show mean of y, residuals and squares (variance)

``` r
mtcars %>% 
  ggplot() +
  aes(standardize(wt), standardize(mpg)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ 1) + 
  geom_smooth_fit(method = lm, formula = y ~ 1) + # wrap as geom_smooth_fit()
  geom_smooth_residuals(method = lm, formula = y ~ 1) + 
  geom_smooth_residuals_squared(method = lm, formula = y ~ 1) + 
  coord_equal()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
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
```

``` r
mtcars |>
  ggplot(aes(wt, mpg)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_smooth_step(method = lm, xseq = 2:3)
#> `geom_smooth()` using formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Via @friendly [ggplot2 extenders ggsprings discussion](https://github.com/ggplot2-extenders/ggplot-extension-club/discussions/83) and [springs extension case study](https://ggplot2-book.org/ext-springs.html)

``` r
library(ggplot2)

create_spring <- function(x, 
                          y, 
                          xend, 
                          yend, 
                          diameter = 1, 
                          tension = 0.75, 
                          n = 50) {
  
  # Validate the input arguments
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }
  
  # Calculate the direct length of the spring path
  length <- sqrt((x - xend)^2 + (y - yend)^2)
  
  # Calculate the number of revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions
  
  # Calculate the sequence of radians and the x and y offset values
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)
  
  # Create and return the transformed data frame
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
  )
}

GeomSmoothSpring <- ggproto("GeomSmoothSpring", Geom,
  
  # Ensure that each row has a unique group id
  setup_data = function(data, params) {
    if (is.null(data$group)) {
      data$group <- seq_len(nrow(data))
    }
    if (anyDuplicated(data$group)) {
      data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
    }
    data
  },
  
  # Transform the data inside the draw_panel() method
  draw_panel = function(data, 
                        panel_params, 
                        coord, 
                        n = 50, 
                        arrow = NULL,
                        lineend = "butt", 
                        linejoin = "round", 
                        linemitre = 10,
                        na.rm = FALSE) {
    
    # Transform the input data to specify the spring paths
    cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
    
    data$diameter <- data$diameter %||%  (.025 * abs(min(data$x)-max(data$x)))
    data$springlength <- sqrt((data$x-data$xend)^2 + (data$y-data$yend)^2)
    data$tension <- data$tension %||% (1 * data$springlength)
    
    springs <- lapply(seq_len(nrow(data)), function(i) {
      spring_path <- create_spring(
        data$x[i], 
        data$y[i], 
        data$xend[i], 
        data$yend[i], 
        data$diameter[i], 
        data$tension[i], 
        n
      )
      cbind(spring_path, unclass(data[i, cols_to_keep]))
    })
    springs <- do.call(rbind, springs)
    
    # Use the draw_panel() method from GeomPath to do the drawing
    GeomPath$draw_panel(
      data = springs, 
      panel_params = panel_params, 
      coord = coord, 
      arrow = arrow, 
      lineend = lineend, 
      linejoin = linejoin, 
      linemitre = linemitre, 
      na.rm = na.rm
    )
  },
  
  # Specify the default and required aesthetics
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(
    colour = from_theme(accent), 
    linewidth = 0.5, 
    linetype = 1L, 
    alpha = NA
  )
  
)


anscombe |> 
  ggplot() + 
  aes(x = x1, y = y1) + 
  geom_point() + 
  geom_smooth(method = lm) +
  stat_smooth_fit(geom = GeomSmoothSpring, method = lm) + 
  ggchalkboard:::theme_blackboard()
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r

last_plot() + 
  aes(x = x2, y = y2)
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r

last_plot() + 
  aes(tension = 1)
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r

last_plot() + 
  aes(x = x3, y = y3) + 
  aes(tension = NULL)
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r

last_plot() + 
  aes(x = x4, y = y4)
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

``` r


anscombe |> 
  ggplot() + 
  aes(x = x2, y = y2) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ 1) +
  stat_smooth_fit(geom = GeomSmoothSpring, method = lm, formula = y ~ 1) + 
  ggchalkboard:::theme_blackboard()
```

![](README_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->

``` r

last_plot() + 
  aes(tension = 1)
```

![](README_files/figure-gfm/unnamed-chunk-10-7.png)<!-- -->

# Part 2. Packaging and documentation 🚧 ✅

## minimal requirements for github package. Have you:

### Created files for package archetecture with `devtools::create("./ggbarlabs")` ✅

### Moved functions R folder? ✅

``` r
knitr::knit_code$get() |> names()
#>  [1] "unnamed-chunk-1"               "unnamed-chunk-2"              
#>  [3] "compute_group_smooth_fit"      "unnamed-chunk-3"              
#>  [5] "compute_group_smooth_sq_error" "unnamed-chunk-4"              
#>  [7] "ggproto_objects"               "unnamed-chunk-5"              
#>  [9] "stat_fit"                      "stat_errorsq"                 
#> [11] "unnamed-chunk-6"               "unnamed-chunk-7"              
#> [13] "unnamed-chunk-8"               "geom_smooth_step"             
#> [15] "unnamed-chunk-9"               "unnamed-chunk-10"             
#> [17] "unnamed-chunk-11"              "unnamed-chunk-12"             
#> [19] "unnamed-chunk-13"              "unnamed-chunk-14"             
#> [21] "stat-smooth"                   "unnamed-chunk-15"             
#> [23] "unnamed-chunk-16"
```

``` r
knitrExtra::chunk_to_dir(c(
                         "geom_smooth_step",
                         "compute_group_smooth_fit", 
                         "compute_group_smooth_sq_error",
                         "ggproto_objects",
                         "stat_fit", 
                         "stat_errorsq"))
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
#> It seems you are currently knitting a Rmd/Qmd file. The parsing of the file will be done in a new R session.
```

### Added roxygen skeleton? ✅

for auto documentation and making sure proposed functions are *exported*

### Managed dependencies ? ✅

package dependencies managed, i.e. `depend::function()` in proposed
functions and declared in the DESCRIPTION

### Chosen a license? ✅

``` r
usethis::use_package("ggplot2")
usethis::use_mit_license()
```

## `devtools::check()` report

``` r
# rm(list = c("geom_barlab_count", "geom_barlab_count_percent"))
devtools::check(pkg = ".")
```

------------------------------------------------------------------------

# Don’t want to use ggsmoothfit? Here are some ways to get it done with base ggplot2!

## Option 1. Verbal description and move on…

“image a line that drops down from the observation to the model line”
use vanilla geom_smooth

``` r
mtcars %>% 
  ggplot() +
  aes(wt, mpg) + 
  geom_point() + 
  geom_smooth()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](README_files/figure-gfm/stat-smooth-1.png)<!-- -->

## Option 2: precalculate and plot

\[stack overflow example goes here.\]

## Option 3: little known xseq argument and geom = “point”

First a bit of under-the-hood thinking about geom_smooth/stat_smooth.

``` r
mtcars %>% 
  ggplot() +
  aes(wt, mpg) + 
  geom_smooth(n = 12) +
  stat_smooth(geom = "point", 
              color = "blue", 
              n = 12)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Specify xseq… Almost surely new to you (and probably more interesting to
stats instructors): predicting at observed values of x..

xseq has only recently been advertised, but possibly of interest..
<https://ggplot2.tidyverse.org/reference/geom_smooth.html>

``` r
# fit where the support is in the data... 
mtcars %>% 
  ggplot() +
  aes(wt, mpg) + 
  geom_point() +
  geom_smooth() + 
  stat_smooth(geom = "point",  color = "blue", # fitted values
              xseq = mtcars$wt) +
  stat_smooth(geom = "segment", color = "darkred", # residuals
              xseq = mtcars$wt,
              xend = mtcars$wt,
              yend = mtcars$mpg)
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

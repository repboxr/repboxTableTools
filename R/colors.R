examples = function() {
  # Example usage
  html_color_grad(c("#FF0000"))  # Single color case
  html_color_grad(c("#FF0000", "#00FF00", "#0000FF"))  # Gradient case
}

named_bg_colors = function(types) {
  n = length(types)
  colors = cell_bg_colors(n)
  names(colors) = types
  colors
}


html_color_grad <- function(colors) {
  if (length(colors) == 0) stop("At least one color is required.")

  if (length(colors) == 1) {
    css_style <- sprintf("background: %s;", colors)
  } else {
    color_string <- paste(colors, collapse = ", ")
    css_style <- sprintf("background: linear-gradient(45deg, %s);", color_string)
  }

  return(css_style)
}




cell_bg_colors = function(n) {
  if (n==0) return(character(0))
  halton = randtoolbox::halton(n+2, dim=2)
  halton = halton[-c(1,2),,drop=FALSE]
  h = (1-halton[,1])*15 + halton[,1] * 345

  s = 0.9*(1-halton[,2]) + 0.6*(halton[,2])
  #l = 0.5*(0.85*(1-halton[,2]) + 0.7*(halton[,2]))+0.5*(0.85*(1-halton[,3]) + 0.7*(halton[,3]))
  l = 0.85*(1-halton[,2]) + 0.7*(halton[,2])

  hsl_to_rgb(h,s,l)

}

explore_cell_bg_colors = function() {
  m = 4
  n = m*m
  df = expand.grid(x=1:m, y=1:m)
  df$color = c(cell_bg_colors(m*m))
  df$ind = 1:NROW(df)

  library(ggplot2)
  ggplot(df, aes(x=x,y=y, label=ind)) +
    geom_tile(fill=df$color) +
    geom_text() +
    theme_minimal()

  df$color
}


hsl_to_rgb <- function(h, s, l) {
  restore.point("hsl_to_rgb")
  h <- h / 360
  r <- g <- b <- 0.0
  hue_to_rgb <- function(p, q, t) {
    t[t<0] = t[t<0] + 1.0
    t[t>1] = t[t>1] - 1.0

    res = case_when(
      t<1/6 ~ p + (q - p) * 6.0 * t,
      t<1/2 ~ q,
      t<2/3 ~ p + ((q - p) * ((2/3) - t) * 6),
      TRUE ~ p
    )
    res
  }
  q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))
  p <- 2.0 * l - q
  r <- hue_to_rgb(p, q, h + 1/3)
  g <- hue_to_rgb(p, q, h)
  b <- hue_to_rgb(p, q, h - 1/3)


  return(rgb(r,g,b))
}

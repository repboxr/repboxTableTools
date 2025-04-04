example = function() {
  # Example for two cells:
  styles <- td_background(
    main_colors = c("#4facfe, #00f2fe", "#f06, #4a90e2"),
    ul_color    = c("yellow", ""),
    ur_color    = NA,
    br_color    = c("red", "green"),
    left_color  = c("green"),
    main_grad   = c("45deg"),
    border_size = "2px",
    triangle_size = "12px"
  )

  cat("Style for cell 1:\n", styles[1], "\n\n")
  cat("Style for cell 2:\n", styles[2], "\n")
}


#' Generate Vectorized Inline CSS Style for TD Cell Background with Optional Triangles and Borders
#'
#' This function creates a CSS background style string for use in an inline HTML style attribute of a `<td>` cell.
#' It is vectorized so that each input parameter is a character vector and the function returns a vector
#' of CSS style strings (one per cell). For each cell, `main_colors` should be a single string (possibly with
#' multiple comma-separated colors) defining the main background gradient. If a corner color (ul_color, ur_color,
#' bl_color, br_color) is not NA or empty, a triangle in that corner is added. Similarly, if a border color is provided,
#' a border is added on that side.
#'
#' @param main_colors A character vector; each element is a string containing one or more colors (comma separated)
#'   for the main background gradient.
#' @param ul_color Character vector for the upper-left triangle color; use NA or "" for none.
#' @param ur_color Character vector for the upper-right triangle color; use NA or "" for none.
#' @param bl_color Character vector for the bottom-left triangle color; use NA or "" for none.
#' @param br_color Character vector for the bottom-right triangle color; use NA or "" for none.
#' @param left_color Character vector for the left border color; use NA or "" for none.
#' @param right_color Character vector for the right border color; use NA or "" for none.
#' @param top_color Character vector for the top border color; use NA or "" for none.
#' @param bottom_color Character vector for the bottom border color; use NA or "" for none.
#' @param main_grad Character vector specifying the gradient direction (default "45deg").
#' @param border_size Character vector for the border width (default "2px").
#' @param triangle_size Character vector for the size of the triangle decorations (default "12px").
#'
#' @return A character vector containing the CSS style definitions.
#'
#' @examples
#' styles <- td_background(
#'   main_colors = c("#4facfe, #00f2fe", "#f06, #4a90e2"),
#'   ul_color    = c("yellow", "orange"),
#'   ur_color    = c("blue", ""),
#'   bl_color    = c("", "purple"),
#'   br_color    = c("red", "green"),
#'   left_color  = c("green", "pink"),
#'   right_color = c(NA, "gray"),
#'   top_color   = c("black", "black"),
#'   bottom_color= c("blue", "blue"),
#'   main_grad   = c("45deg", "60deg"),
#'   border_size = "2px",
#'   triangle_size = "12px"
#' )
#' cat(styles[1], "\n\n")
#' cat(styles[2], "\n")
td_background <- function(main_colors,
                          ul_color = NA, ur_color = NA,
                          bl_color = NA, br_color = NA,
                          left_color = NA, right_color = NA,
                          top_color = NA, bottom_color = NA,
                          main_grad = "45deg",
                          border_size = "2px",
                          triangle_size = "12px") {
  # Determine number of cells and recycle inputs if necessary
  n <- length(main_colors)
  main_colors   <- rep_len(main_colors, n)
  ul_color      <- rep_len(ul_color, n)
  ur_color      <- rep_len(ur_color, n)
  bl_color      <- rep_len(bl_color, n)
  br_color      <- rep_len(br_color, n)
  left_color    <- rep_len(left_color, n)
  right_color   <- rep_len(right_color, n)
  top_color     <- rep_len(top_color, n)
  bottom_color  <- rep_len(bottom_color, n)
  main_grad     <- rep_len(main_grad, n)
  border_size   <- rep_len(border_size, n)
  triangle_size <- rep_len(triangle_size, n)

  # Create main gradient string for each cell
  main_gradient <- paste0("linear-gradient(", main_grad, ", ", main_colors, ")")

  # For each corner, if the color is provided, create its linear-gradient string; otherwise, empty string.
  ul_img <- ifelse(!is.na(ul_color) & nzchar(ul_color),
                   paste0("linear-gradient(135deg, ", ul_color, " 50%, transparent 50%)"),
                   "")
  ur_img <- ifelse(!is.na(ur_color) & nzchar(ur_color),
                   paste0("linear-gradient(225deg, ", ur_color, " 50%, transparent 50%)"),
                   "")
  bl_img <- ifelse(!is.na(bl_color) & nzchar(bl_color),
                   paste0("linear-gradient(315deg, ", bl_color, " 50%, transparent 50%)"),
                   "")
  br_img <- ifelse(!is.na(br_color) & nzchar(br_color),
                   paste0("linear-gradient(45deg, ", br_color, " 50%, transparent 50%)"),
                   "")

  # Combine background-image strings in order: UL, UR, BL, BR triangles then the main gradient.
  bg_image <- paste(ul_img, ur_img, bl_img, br_img, main_gradient, sep = ", ")
  # Clean up: remove extra commas/spaces
  bg_image <- gsub("(,\\s*)+", ", ", bg_image)
  bg_image <- gsub("^(,\\s*)", "", bg_image)
  bg_image <- gsub("(,\\s*)$", "", bg_image)

  # Background-position for the triangles: use the corresponding corner position if color provided, else "".
  ul_pos <- ifelse(!is.na(ul_color) & nzchar(ul_color), "top left", "")
  ur_pos <- ifelse(!is.na(ur_color) & nzchar(ur_color), "top right", "")
  bl_pos <- ifelse(!is.na(bl_color) & nzchar(bl_color), "bottom left", "")
  br_pos <- ifelse(!is.na(br_color) & nzchar(br_color), "bottom right", "")
  bg_position <- paste(ul_pos, ur_pos, bl_pos, br_pos, "center", sep = ", ")
  bg_position <- gsub("(,\\s*)+", ", ", bg_position)
  bg_position <- gsub("^(,\\s*)", "", bg_position)
  bg_position <- gsub("(,\\s*)$", "", bg_position)

  # Background-size for triangles: if color provided, set to triangle_size; otherwise, empty.
  ul_size <- ifelse(!is.na(ul_color) & nzchar(ul_color),
                    paste(triangle_size, triangle_size),
                    "")
  ur_size <- ifelse(!is.na(ur_color) & nzchar(ur_color),
                    paste(triangle_size, triangle_size),
                    "")
  bl_size <- ifelse(!is.na(bl_color) & nzchar(bl_color),
                    paste(triangle_size, triangle_size),
                    "")
  br_size <- ifelse(!is.na(br_color) & nzchar(br_color),
                    paste(triangle_size, triangle_size),
                    "")
  bg_size <- paste(ul_size, ur_size, bl_size, br_size, "100% 100%", sep = ", ")
  bg_size <- gsub("(,\\s*)+", ", ", bg_size)
  bg_size <- gsub("^(,\\s*)", "", bg_size)
  bg_size <- gsub("(,\\s*)$", "", bg_size)

  # Build border styles if specified
  left_border   <- ifelse(!is.na(left_color) & nzchar(left_color),
                          paste0("border-left: ", border_size, " solid ", left_color, ";"),
                          "")
  right_border  <- ifelse(!is.na(right_color) & nzchar(right_color),
                          paste0("border-right: ", border_size, " solid ", right_color, ";"),
                          "")
  top_border    <- ifelse(!is.na(top_color) & nzchar(top_color),
                          paste0("border-top: ", border_size, " solid ", top_color, ";"),
                          "")
  bottom_border <- ifelse(!is.na(bottom_color) & nzchar(bottom_color),
                          paste0("border-bottom: ", border_size, " solid ", bottom_color, ";"),
                          "")
  border_styles <- paste(left_border, right_border, top_border, bottom_border)
  border_styles <- gsub("\\s+", " ", border_styles)

  # Combine all parts into the final style string for each cell.
  style <- paste0("background-image: ", bg_image, "; ",
                  "background-position: ", bg_position, "; ",
                  "background-size: ", bg_size, "; ",
                  "background-repeat: no-repeat; ",
                  border_styles)

  return(style)
}




#' Generate Inline CSS Style for TD Cell Background with Optional Triangles and Borders
#'
#' This function generates a CSS background and border style string
#' for use in an inline HTML style attribute of a `<td>` cell.
#' It allows adding colored triangles in any corner of the cell
#' and optional colored borders.
#'
#' @param main_colors A character vector of main background gradient colors.
#' @param ul_color Color for upper-left triangle. Default is NULL (no triangle).
#' @param ur_color Color for upper-right triangle. Default is NULL.
#' @param bl_color Color for bottom-left triangle. Default is NULL.
#' @param br_color Color for bottom-right triangle. Default is NULL.
#' @param left_color Color for left border. Default is NULL.
#' @param right_color Color for right border. Default is NULL.
#' @param top_color Color for top border. Default is NULL.
#' @param bottom_color Color for bottom border. Default is NULL.
#' @param main_grad Angle or direction of the main gradient (default "45deg").
#' @param border_size Size of the borders (default "2px").
#' @param triangle_size Size of the triangle decorations (default "12px").
#'
#' @return A character string containing the CSS style definition.
#'
#' @examples
#' td_background(
#'   main_colors = c("#4facfe", "#00f2fe"),
#'   ul_color = "yellow", br_color = "red",
#'   left_color = "green", bottom_color = "blue",
#'   triangle_size = "16px"
#' )
td_background_no_vec <- function(main_colors,
                          ul_color = NULL, ur_color = NULL,
                          bl_color = NULL, br_color = NULL,
                          left_color = NULL, right_color = NULL,
                          top_color = NULL, bottom_color = NULL,
                          main_grad = "45deg",
                          border_size = "2px",
                          triangle_size = "12px") {

  # Helper: Create gradient triangle
  triangle_gradient <- function(direction, color) {
    paste0("linear-gradient(", direction, ", ", color, " 50%, transparent 50%)")
  }

  # Triangles
  triangles <- c()
  positions <- c()
  sizes <- c()

  if (!is.null(ul_color)) {
    triangles <- c(triangles, triangle_gradient("135deg", ul_color))
    positions <- c(positions, "top left")
    sizes <- c(sizes, paste(triangle_size, triangle_size))
  }
  if (!is.null(ur_color)) {
    triangles <- c(triangles, triangle_gradient("225deg", ur_color))
    positions <- c(positions, "top right")
    sizes <- c(sizes, paste(triangle_size, triangle_size))
  }
  if (!is.null(bl_color)) {
    triangles <- c(triangles, triangle_gradient("315deg", bl_color))
    positions <- c(positions, "bottom left")
    sizes <- c(sizes, paste(triangle_size, triangle_size))
  }
  if (!is.null(br_color)) {
    triangles <- c(triangles, triangle_gradient("45deg", br_color))
    positions <- c(positions, "bottom right")
    sizes <- c(sizes, paste(triangle_size, triangle_size))
  }

  # Main background gradient
  main_gradient <- paste0("linear-gradient(", main_grad, ", ", paste(main_colors, collapse = ", "), ")")
  triangles <- c(triangles, main_gradient)
  positions <- c(positions, "center")
  sizes <- c(sizes, "100% 100%")

  # Compose background style
  bg_image <- paste(triangles, collapse = ", ")
  bg_position <- paste(positions, collapse = ", ")
  bg_size <- paste(sizes, collapse = ", ")

  # Border style
  borders <- c()
  if (!is.null(left_color)) {
    borders <- c(borders, paste0("border-left: ", border_size, " solid ", left_color, ";"))
  }
  if (!is.null(right_color)) {
    borders <- c(borders, paste0("border-right: ", border_size, " solid ", right_color, ";"))
  }
  if (!is.null(top_color)) {
    borders <- c(borders, paste0("border-top: ", border_size, " solid ", top_color, ";"))
  }
  if (!is.null(bottom_color)) {
    borders <- c(borders, paste0("border-bottom: ", border_size, " solid ", bottom_color, ";"))
  }

  # Final style string
  style <- paste0(
    "background-image: ", bg_image, "; ",
    "background-position: ", bg_position, "; ",
    "background-size: ", bg_size, "; ",
    "background-repeat: no-repeat; ",
    paste(borders, collapse = " ")
  )

  return(style)
}

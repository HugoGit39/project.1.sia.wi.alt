############################################################################################
#
#  Function for reactable layout
#
#############################################################################################

# Function to generate N-step palette from a hex color with varying alpha
generate_alpha_palette <- function(base_color = "#1c75bc", steps = 100) {
  # Remove '#' and convert hex to RGB
  rgb <- col2rgb(base_color)

  # Generate alpha values from 0 to 255 (00 to FF in hex)
  alphas <- round(seq(0, 255, length.out = steps))
  alpha_hex <- toupper(sprintf("%02X", alphas))  # convert to 2-digit hex

  # Combine base color with varying alpha
  paste0(base_color, alpha_hex)
}

# Helper: Map numeric to color
map_to_colors <- function(values, palette) {
  ranks <- rank(values, na.last = "keep", ties.method = "average")
  scaled <- scales::rescale(ranks, to = c(1, length(palette)), na.rm = TRUE)
  index <- round(scaled)
  palette[index]
}

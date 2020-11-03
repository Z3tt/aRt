library(ggplot2)
library(dplyr)
library(magick)

# Read in image and convert to grayscale
img <- image_read("split-bar/images/cedric.jpg") %>%
  image_convert(colorspace = "gray")

# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height

# Resize the longest dimension to 80 pixels
if (img_w >= img_h) {
  img <- image_resize(img, "80")
} else {
  img <- image_resize(img, ("x80"))
}

# Create array and number rows and columns
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# Create data frame from array and rename columns
img_df <- as.data.frame.table(img_array) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
    across(everything(), as.numeric),
    # convert b (0-255) to bf (1-0), so that "brighter" values become smaller bars, then multiple with 0.9 in order to get a horizontal margin between bars
    bf = (1 - b / 255) * 0.9  
  )

ggplot(img_df) +
  geom_rect(aes(xmin = x, xmax = x + bf, ymin = y, ymax = y + 0.85, fill = bf), color = NA) +
  scale_y_reverse() +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  scale_size(range = c(0, 1.4)) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect(color = "grey90", fill = "grey90"),
        plot.background = element_rect(color = "grey70", fill = "grey70")) +
  theme(legend.position = "none") +
  ggsave("split-bar/plots/cedric_bars_color.png", width = 4.6, height = 5)

ggplot(img_df) +
  geom_point(aes(x, y, size = bf), color = "#28a87d") +
  scale_y_reverse() +
  scale_size(range = c(0, 1.4)) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect(color = "grey90", fill = "grey90"),
        plot.background = element_rect(color = "grey70", fill = "grey70")) +
  theme(legend.position = "none") +
  ggsave("split-bar/plots/cedric_dots_filled.png", width = 4.6, height = 5)

ggplot(img_df) +
  geom_point(aes(x, y, size = bf), color = "#28a87d", shape = 21, fill = "transparent") +
  scale_y_reverse() +
  scale_size(range = c(0, 1.4)) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect(color = "grey90", fill = "grey90"),
        plot.background = element_rect(color = "grey70", fill = "grey70")) +
  theme(legend.position = "none") +
  ggsave("split-bar/plots/cedric_dots_outline.png", width = 4.6, height = 5)

ggplot(img_df) +
  geom_point(aes(x, y, size = bf, color = bf)) +
  #geom_point(aes(x, y, size = -b, color = -b), shape = 21, fill = "transparent") +
  scale_y_reverse() +
  #rcartocolor::scale_color_carto_c(palette = "Earth") +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  scale_size(range = c(0, 1.4)) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect(color = "grey70", fill = "grey70"),
        plot.background = element_rect(color = "grey90", fill = "grey90")) +
  theme(legend.position = "none") +
  ggsave("split-bar/plots/cedric_dots_filled_color_Viridis2.png", width = 4.6, height = 5)

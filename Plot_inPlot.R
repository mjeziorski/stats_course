# Othe ggplot example.
# This example was taken form an entru to R-bloggers kjytay. Is to have a plot inset into 
# other plot ggplot
library(ggplot2)

# generate fake data
set.seed(42)
n <- 1000
x <- runif(n) * 3
y <- x * sin(1/x) + rnorm(n) / 25
df <- data.frame(x = x, y = y)

# overall plot
p1 <- ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = FALSE) +
    theme_bw()
p1

# zoomed in plot
p2 <- ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = FALSE) +
    scale_x_continuous(limits = c(0, 0.5)) +
    scale_y_continuous(limits = c(-0.3, 0.6)) +
    theme_bw()
p2

# plot with inset
p1 + annotation_custom(ggplotGrob(p2), xmin = 1, xmax = 3, 
                       ymin = -0.3, ymax = 0.6)

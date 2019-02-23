## Heatmap ##

# Load the necessary packages
library(dplyr) 
library(tidyr)
library(ggplot2)
crimes <- read.csv("crimedata.csv", header = T)
head(crimes)
mcrimes<- separate(data = crimes, col = month, into = c("year", "month"), sep = "-")
head(mcrimes)

df <- mcrimes %>%
  filter(town == "Manchester") %>%
  group_by(category, year) %>%
  summarise(n = n())
df
ggplot(df, aes(x=category, y=year, fill=n)) +
  geom_tile(aes(fill=n)) +
  geom_text(aes(label=n), size=4, color="black") +
  scale_x_discrete("", expand = c(0,0)) +
  scale_y_discrete("", expand = c(0,-2)) +
  scale_fill_gradient("Frequency", low = "white", high = "steelblue") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text=element_text(size=12)) +
  theme(legend.position="none") +
  ggtitle("Crime levels in Manchester by year\n") +
  theme(plot.title = element_text(face="bold", size="20"))

ggsave("calendar_heatmap.png", scale = 1, dpi = 300)

## Using d3heatmap
install.packages("d3heatmap")
library(d3heatmap)

df <- mcrimes %>%
  filter(town == "Manchester") %>%
  group_by(category, year) %>%
  summarise(n = n()) %>%
  spread(as.numeric(year), n, fill = 0)
row.names(df) <- df$category
df$category <- NULL

d3heatmap(df, scale="column", dendrogram = "none",
          color = scales::col_quantile("Blues", NULL, 5),
          # color = scales::col_bin("Blues", NULL, bins = 7, pretty = TRUE, na.color = "#808080"),
          xaxis_font_size = 10, yaxis_font_size = 10)




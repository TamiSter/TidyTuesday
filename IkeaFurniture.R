#Packages to install...libraries to load ----
install.packages("tidytuesdayR")
install.packages("waffle")
install.packages("RColorBrewer")
install.packages("plotly")
library(tidyr)
library(dplyr)
library(ggplot2)
library(waffle)
library(RColorBrewer)
library(stringr)
library(plotly)

#Read tidytuesday data for the week ----
tuesdata <- tidytuesdayR::tt_load('2020-11-03')
ikea.csv <- tuesdata$ikea
#visualize the tibble
head(ikea.csv)
#The data seems so be tidy, each item is in its own row
#Show unique categories in the tibble
unique(ikea.csv$category)

#Beggin the analysis and filtering of data ----
#Store all the items that have an old price registered
WithOldPrice <- filter(ikea.csv,old_price != "No old price")
unique(WithOldPrice$category)

#The tally function counts the number of rows for each group, in this case, categories.
SumCategory <- WithOldPrice %>% 
  group_by(category) %>% 
  tally()
#I'm building a named vector to use with the waffle graph function.
CategorysVector <- SumCategory$n
names(CategorysVector) <- SumCategory$category

#This part was necessary to expand the number of colours used by Waffle
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

#First graphs show number of items in each category ----
#Bar plot to show number of items in each category
ggplot2::ggplot(WithOldPrice) +
  geom_bar(mapping = aes(x = category)) +
  coord_flip()

#Waffle graph to show number of items in each category
waffleGraph <- waffle(CategorysVector/5, 
                      rows = 20,
                      size = 1, 
                      colors = mycolors, 
                      legend_pos = "bottom",
                      xlab = "1 sq = 5 units",
                      title = "IKEA Total Items by Category") 
  
ggsave("WaffleItemsIKEA.png") 

colorVector <- CategorysVector
names(colorVector) <- mycolors

#Now we want to see price drop tendencies ----
#I'll try to modify the old_price column to make it a number
OldPrices <- str_remove(WithOldPrice$old_price, "SR ")
#So I rewrite my tibble with the old prices as numbers, removing the thousand comma and deleting the "NA"
WithOldPrice <- WithOldPrice %>% 
  mutate(old_price = as.numeric(gsub(",","", OldPrices))) %>% 
  #There are some products that show price/pack and those give NA when coerced
  filter(is.na(old_price) == FALSE)      


#Now I want to visualize the tendencies of products in different categories or maybe group by tendencies in price
WithOldPriceDelta <- WithOldPrice %>% 
  select(item_id,name,category,price,old_price) %>% 
  mutate(DeltaPrice = 0.27 * (as.numeric(WithOldPrice$price) - as.numeric(WithOldPrice$old_price)),
         NumItemsInCat = CategorysVector[category]) %>% 
  arrange(desc(NumItemsInCat))

#Joshua solution @delaBJL

OldPriceAvg <- WithOldPriceDelta %>% 
  group_by(category) %>% 
  summarise(num_in_category = n(),
            mean_delta=mean(DeltaPrice, na.rm = TRUE)) %>%
  arrange((num_in_category))

OldPriceAvg %>%
  mutate(category = factor(category, levels = OldPriceAvg$category)) %>%
  ggplot()+
  geom_bar(
    aes(x=mean_delta, y= category, fill = num_in_category),
    stat = "identity")+
  labs(y = "",
       x= "Mean Price Drop (US$)", 
       title = "IKEA Average Price Drop by Furniture Category", 
       fill = "Number of Items")+
  theme_minimal()
ggsave("IKEAPriceDrop.png")

#My original solution, that didn't work for the plot
WithOldPriceDelta <- WithOldPriceDelta %>% 
  group_by(category) %>% 
  mutate(Avg_PriceDrop = mean(DeltaPrice))

PriceDrops <- ggplot(WithOldPriceDelta) +
  geom_col(mapping = aes(x = reorder(category,NumItemsInCat, decreasing = TRUE), y = Avg_PriceDrop, 
                         fill = NumItemsInCat)) +
  coord_flip() +
  labs(x = "",
       y = "Mean price drop (US$)",
       caption = "@TamSternlieb",
       title = "IKEA Average Price Drop by Furniture Category",
       subtitle = "TidyTuesday - Week 45",
       fill = "Number of Items") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.background = element_blank()) 
#The plot I get shows bigger scales in the y axis. This is because
#ggplot is adding up all the rows for each category at the Avr_PriceDrop

#Lets use plotly! ----
fig <- plot_ly(WithOldPriceDelta, 
               x = ~WithOldPriceDelta$DeltaPrice, 
               y = ~WithOldPriceDelta$category, 
               name = "Price Drop US$", 
               type = 'scatter',
               mode = "markers", marker = list(color = ~WithOldPriceDelta$DeltaPrice),
               # Hover text:
               text = ~paste("Drop US$ ", WithOldPriceDelta$DeltaPrice, '$<br>ItemID:', WithOldPriceDelta$item_id, '$<br>Item Name:', WithOldPriceDelta$name),
               hoverinfo = 'text'
               ) 
fig <- fig %>% 
  layout(
    title = "Price Drop for Items in each category",
    xaxis = list(title = "Price Drop"),
    yaxis = list(title = ""),
    margin = list(l = 100)
  )


fig

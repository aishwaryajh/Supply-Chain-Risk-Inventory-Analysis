library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
setDT(data_exhibits_project)
data = copy(data_exhibits_project)
summary(data)
data$SKU <- as.character(data$SKU)
summary(data)
head(data)
names(data) = c("SKU", "stdprice", "onHandStock", "Inventoryinunitsonhand", 
                "APU", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May","June", 
                "July", "August","September", "APUTrend",
                "SOTD", "Demandvariability_COV", "LeadTimeindays")
data[, SKU := as.character(ceiling(as.numeric(SKU)))]

#top 10 highest priced SKUs
data_1 = data[1:10]
data_1

#10 lowest priced SKUs
data_2 <- tail(data[complete.cases(data), ], 10)
data_2
summary(data_2)

#bar plots for highest and lowest price
ggplot(data_1, aes(x = reorder(SKU, -stdprice), y = stdprice)) +
         geom_bar(stat = "identity", fill = "blue") +
         labs(x = "SKU", y = "Price of each unit of SKU ($)",
              title = "10 Highest priced SKUs") +
         theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data_1$stdprice), by = 500))

ggplot(data_2, aes(x = reorder(SKU, stdprice), y = stdprice)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "SKU", y = "Price of each unit of SKU ($)",
       title = "10 Lowest priced SKUs") +
  theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data_1$stdprice), by = 0.3))

#Line plots for on-hand stock

ggplot(data_1, aes(x = reorder(SKU, -stdprice), y = onHandStock, group = 1)) +
  geom_line(color = "orange", size = 1) + geom_point() +
  labs(x = "SKU", y = " Total stock on-hand as of 30th Sep'20 ($)",
       title = "Top 10 Highest Priced SKUs - On-Hand Stock") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data_1$onHandStock), by = 10000))

ggplot(data_2, aes(x = reorder(SKU, -stdprice), y = onHandStock, group = 1)) +
  geom_line(color = "orange", size = 1) + geom_point() +
  labs(x = "SKU", y = " Total stock on-hand as of 30th Sep'20 ($)",
       title = "Top 10 Lowest Priced SKUs - On-Hand Stock") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(data_2$onHandStock), by = 2000))

#line plots for APU Trend
ggplot(data_1, aes(x = reorder(SKU, -stdprice), y = APUTrend, group = 1)) +
  geom_line(color = "lightgreen", size = 1) + geom_point() +
  labs(x = "SKU", y = " % increase in APU consumption",
       title = "Top 10 Highest Priced SKUs -  Anticipated APU trend ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(data_2, aes(x = reorder(SKU, -stdprice), y = APUTrend, group = 1)) +
  geom_line(color = "lightgreen", size = 1) + geom_point() +
  labs(x = "SKU", y = " % increase in APU consumption",
       title = "Top 10 Lowest Priced SKUs -  Anticipated APU trend ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Multiple line plots for APU and on hand Inventory units
ggplot(data_1, aes(x = reorder(SKU, -stdprice))) +
  geom_line(aes(y = Inventoryinunitsonhand, color = "Inventory"),
            size = 1, group = 1) +
  geom_line(aes(y = APU, color = "APU"),
            size = 1, group = 1) +
  labs(x = "SKU", y = "Count",
       title = "Top 10 Highest Priced SKUs-Inventory and APU",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 220,20)) +
  scale_color_manual(values = c("Inventory" = "lightblue", "APU" = "red3"))

ggplot(data_2, aes(x = reorder(SKU, -stdprice))) +
  geom_line(aes(y = Inventoryinunitsonhand, color = "Inventory"),
            size = 1, group = 1) +
  geom_line(aes(y = APU, color = "APU"),
            size = 1, group = 1) +
  labs(x = "SKU", y = "Count",
       title = "Top 10 Lowest Priced SKUs-Inventory and APU",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 25000,1500)) +
  scale_color_manual(values = c("Inventory" = "lightblue", "APU" = "red3"))

# histogram for lead time
ggplot(data, aes(x = LeadTimeindays)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 350, by = 50))+
  labs(title = "Lead Time in days for all SKUs",
       x = "Lead Time (days)", y = "Frequency") +
  theme_minimal()



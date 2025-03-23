library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
setDT(data_exhibits_project)
data = copy(data_exhibits_project)
summary(data)
data$Revenue
data$Revenue <- as.numeric(data$Revenue)
names(data)
head(data)
names(data) = c("SupplierName", "Location", "Revenue", "CashfromOperations", "CreditRating",
                "S-OTD", "SingleSource", "IPProtection", "Datasecurity", "LabourUnrests", 
                "EnvironmentalIncidents")
names(data)
location_counts <- data %>%
  group_by(Location) %>%
  summarise(SupplierCount = n())
(location_counts)
location_counts <- na.omit(location_counts)
# Plot a bar graph
barplot(location_counts$SupplierCount, 
        names.arg = location_counts$Location,
        col = "navyblue",
        main = "Geographical Distribution of Suppliers",
        xlab = "",
        ylab = "Number of Suppliers",
        ylim = c(0, max(location_counts$SupplierCount) + 2),
        border = "black",
        las = 3 , # Rotate the x-axis labels for better readability
        cex.names = 0.8
)
SupplierName = c('Plaxian', 'GutesGlas', 'Boavidro', 'Saanch', 'RealGlass', 'Optikiet', 'BestOGlass', 
                'MedicMetric', 'Shale', 'Opticful', 'basicPharm', 'PharmyLeaf')
SingleSource = c('Y', 'N', 'N', 'N', 'Y', 'Y', 'Y', 'Y', 'N', 'N', 'Y', 'N')
IPProtection = c('N', 'N', 'N', 'N', 'Y', 'Y', 'N', 'Y', 'N', 'N', 'Y', 'N')
LabourUnrests = c('Y', 'N', 'N', 'N', 'Y', 'N', 'N', 'N', 'N', 'N', 'N', 'N')
EnvironmentalIncidents = c('N', 'N', 'N', 'N', 'Y', 'N', 'N', 'N', 'Y', 'N', 'N', 'N')
d = data.table(SupplierName, SingleSource, IPProtection, LabourUnrests, EnvironmentalIncidents )
d

d[, c("SingleSource", "IPProtection", "LabourUnrests", "EnvironmentalIncidents") := lapply(.SD, as.factor),
  .SDcols = c("SingleSource", "IPProtection", "LabourUnrests", "EnvironmentalIncidents")]

# Melt the data for easy plotting with ggplot2
melted_data <- melt(d, id.vars = "SupplierName")

# Create a grouped bar plot with facet_wrap
ggplot(melted_data, aes(x = variable, y = value, fill = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~SupplierName, scales = "free_y", ncol = 4) +
  labs(title = "Comparison of Supplier Characteristics",
       x = "Characteristic",
       y = "") +
  theme_minimal() +
  scale_fill_manual(values = c("Y" = "lightgreen", "N" = "red3")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text.y = element_blank())
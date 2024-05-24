# Install and load necessary packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, haven)

#install.packages("ggplot2")
#install.packages("haven")

library(ggplot2)
library(haven)

# Set directories
replication_dir <- "C:\\OLD Asus\\Cdoc\\Hs-Fresenius\\PDFs\\Third Semester\\Data Science\\Project\\142621-V1\\Replication_Data"
figures_dir <- "path/to/figures"

# Read the data
data <- read_dta(file.path(replication_dir, "cz_adult_emp_pov.dta"))



model<-lm(emp2000 ~ pov_rate , data = data)
show(model)

ggplot(data, aes(x = pov_rate, y = emp2000)) +
  stat_summary_bin(
    fun = mean, 
    bins = 20, 
    geom = "point",
    color = "black",
    size = 2,
    na.rm = TRUE
  ) + labs(
    x = "Poverty Rate (Percentage Points)",
    y = "Employment Rate (CZ-Level)",
    title = "Panel A. Adult Employment Rate (2000)"
  ) + 
  stat_smooth(formula = y ~ x, method = "lm", se = FALSE, colour = "red",na.rm = TRUE, linetype = 1) +
  scale_x_continuous(limits = c(5, 35), breaks = seq(5, 35, by = 5))+
  scale_y_continuous(limits = c(0.45, 0.70), breaks = seq(0.45, 0.70, by = 0.05))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank())  # Remove vertical gridlines 
  

# Save the plot to a file
ggsave(filename = file.path(figures_dir, "fig_emp_pov_rate.pdf"), width = 8, height = 6)

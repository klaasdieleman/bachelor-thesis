##
# Appendix 4 - R IRR scripts
#

# This scripts gets Cohen's Kappa and Gwet's AC1 for inter-rater reliability
# between two raters on a data set with a number of numerical variables.
# It requires a CSV data file with column names like 'q1_a' ... 'q19_a'
# for rater A and 'q1_b' ... 'q19_b' for rater B.

install.packages("irr")
install.packages("irrCAC")

data <- read.csv("./GRoLTS IRR dataset.csv")

library(irr)
library(irrCAC)
library(ggplot2)
library(dplyr)

ITEMS_COUNT <- 19

##
# First, we're outputting IRR values for each of the items separately
#

kappa_values <- numeric(ITEMS_COUNT)
kappa_pvalues <- numeric(ITEMS_COUNT)
AC1_values <- numeric(ITEMS_COUNT)
AC1_pvalues <- numeric(ITEMS_COUNT)

# Loop through the items,
for (i in 1:ITEMS_COUNT) {
  # collect all scores from rater A and rater B and put them into a data frame,
  rater_a <- data[[paste0("q", i, "_a")]]
  rater_b <- data[[paste0("q", i, "_b")]]
  ratings_item <- data.frame(rater_a, rater_b)
  
  # get the Kappa and AC1 for this item,
  # and add them to their results collections.
  kappa_values[i] <- kappa2(ratings_item)$value
  
  kappa_result <- kappa2(ratings_item)
  kappa_values[i]  <- round(kappa_result$value, 2)
  kappa_pvalues[i]   <- round(kappa_result$p.value, 4)
  
  AC1_result <- gwet.ac1.raw(ratings_item)
  AC1_values[i]  <- round(AC1_result$est$coeff.val, 2)
  AC1_pvalues[i]   <- round(AC1_result$est$p.value, 4)
}

# put the results in a data frame, to easily read their values and p-values
kappa_results <- data.frame(
  kappa  = kappa_values,
  p = kappa_pvalues
)
AC1_results <- data.frame(
  ac1  = AC1_values,
  p = AC1_pvalues
)

print(kappa_results)
print(AC1_results)


##
# Second, we're outputting the grand total IRR for all items over all records.
#

# Select columns for each of the two raters
cols_rater_a <- grep("_a$", names(data), value = TRUE)
cols_rater_b <- grep("_b$", names(data), value = TRUE)

# Create subsets per rater
data_rater_a <- data[, cols_rater_a]
data_rater_b <- data[, cols_rater_b]

# Flatten data sets into a vector per rater
vector_rater_a <- as.vector(as.matrix(data_rater_a))
vector_rater_b <- as.vector(as.matrix(data_rater_b))

# Put all scores for both raters into a data frame and get the grand total
# Kappa and Gwet's AC1 values.
ratings_all <- data.frame(vector_rater_a, vector_rater_b)
kappa_result_all <- kappa2(ratings_all)
AC1_result_all <- gwet.ac1.raw(ratings_all)

print(kappa_result_all)
print(AC1_result_all$est)


##
# Third, output descriptive statistics on the number of positively scored items
# for each rater.
positives_count_a <- rowSums(data_rater_a)
positives_count_b <- rowSums(data_rater_b)

cat("Rater A",
    "\n  Mean", mean(positives_count_a),
    "\n  SD", sd(positives_count_a),
    "\n  Range", min(positives_count_a), "-", max(positives_count_a),
    "\n\n",
    "Rater B",
    "\n  Mean", mean(positives_count_b),
    "\n  SD", sd(positives_count_b),
    "\n  Range", min(positives_count_b), "-", max(positives_count_b))


##
# Fourth, we're creating a box plot for these stats

# Combine into a data frame
df <- data.frame(
  Score = c(positives_count_a, positives_count_b),
  Rater = factor(rep(c("Human", "LLM"), each = 37), levels = c("Human", "LLM"))
)

# Export to PDF
pdf("boxplot_raters.pdf", width = 3.5, height = 4, family = "sans", pointsize = 11)

# Create the plot
ggplot(df, aes(x = Rater, y = Score, fill = Rater)) +
  geom_boxplot(width = 0.35, outlier.shape = 21, outlier.fill = "white") +
  scale_fill_manual(values = c("Human" = "#52822b", "LLM" = "#2f63b5")) +
  stat_summary(fun = median, geom = "crossbar", width = 0.35, color = "white", linewidth = 0.25) +
  coord_cartesian(ylim = c(0, 19)) +
  scale_y_continuous(breaks = seq(0, 19, 5), minor_breaks = seq(0, 19, 1), expand = c(0,0)) +
  labs(
    x = NULL,
    y = "Reported Items (0-19)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size = 10, family = "sans"),
    axis.text.x = element_blank(), # element_text(color = "black", size = 10, family = "sans"),
    axis.text.y = element_text(color = "black", size = 8, family = "sans"),
    axis.ticks = element_blank(),
    axis.ticks.y.left = element_line(color = "gray30", linewidth = 0.3),
    axis.ticks.length = unit(4, "pt"),
    panel.grid = element_blank()
  ) +
  annotate("text", x = 1.5, y = 18.5, label = "italic(N) ~ '=' ~ 37",
    size = 3, hjust = 0.5, family = "sans", parse = TRUE) +
  annotate("text", x = 1, y = 10, label = "Human",
    size = 3, family = "sans", color = "white") +
  annotate("text", x = 2, y = 7.5, label = "LLM",
    size = 3, family = "sans", color = "white")

# Close the PDF device
dev.off()

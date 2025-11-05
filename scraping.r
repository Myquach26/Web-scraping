# Suppose you are an investor.
# You want to diversify your porfolio within European stock markets.

# Load necessary libraries
library(tidyverse)
library(rvest)
library(corrplot)
# Set high timeout just in case the server is slow
options(timeout = 300)

# --- 1. SET UP AND SCRAPE DATA ---

# Trading Economics - Major Stock Market Indexes Correlation (Last 90 Days)
url <- "https://tradingeconomics.com/stocks/correlations"
# In case we want last 180 days 
# url <- "https://tradingeconomics.com/stocks/correlations?d=180"

# Parse HTML and extract the tables
tryCatch({
  page <- read_html(url)
  
  # FALLBACK: Use the simple "table" selector, which is more reliable.
  # This returns a LIST of all tables on the page.
  all_tables <- page %>% 
    html_nodes("table") %>% 
    html_table(fill = TRUE)
  
  if (length(all_tables) == 0) {
    stop("No tables found on the page. The website structure may have changed completely.")
  }
  
  # The correlation data is usually the first (or sometimes second) table.
  # We will assume it is the largest table or the one with more than 1 column.
  # A simple heuristic is to find the first table with more than one column.
  df <- NULL
  for (i in 1:length(all_tables)) {
    if (ncol(all_tables[[i]]) > 1) {
      df <- all_tables[[i]]
      break
    }
  }
  
  if (is.null(df)) {
    stop("Found tables, but none contain multiple columns (indicating correlation data).")
  }
  
}, error = function(e) {
  cat("Error during scraping:", conditionMessage(e), "\n")
  # Return a stop signal so the script doesn't proceed with non-data
  stop("Web scraping failed.")
})

# --- 2. DATA CLEANING AND PREPARATION ---

# Tidyverse pipeline for cleaning:

cor_matrix <- df %>%
  # 1. Use the first column as row names
  column_to_rownames(var = names(.)[1]) %>%
  # 2. Convert all columns to numeric (correlation values)
  # 'across' is used for modern tidyverse column selection/mutation
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  # 3. Convert the data frame to a matrix for corrplot
  as.matrix()

# --- 3. SELECTION AND FINAL ADJUSTMENTS ---

# Define the European stock indexes you want to include
european_indexes <- c("GB100", "DE40", "FR40", "IT40", "ES35")

# Select the European stock indexes from the matrix
# Use tryCatch to gracefully handle cases where an index might be missing
tryCatch({
  final <- cor_matrix[european_indexes, european_indexes]
}, error = function(e) {
  cat("Error selecting European indexes. Check if all indexes (GB100, DE40, etc.) exist in the scraped data.\n")
  stop(conditionMessage(e))
})

# Fill in the diagonal with 1 (since correlation with self is perfect)
diag(final) <- 1

# Display the final matrix
print(final)

# --- 4. PLOTTING THE CORRELATION HEATMAP (Enhanced Visualization) ---

# Set a title based on the data period (assuming 90 days if no parameter is used)
plot_title <- "European Stock Index Correlations (Last 90 Days)"

# Define the custom color palette
custom_palette <- colorRampPalette(c("darkred", "white", "darkblue"))(100)

# Function to combine two visualization methods in one plot
corrplot(final, 
  method = "circle", 
  type = "upper", 
  tl.col = "black", 
  col = colorRampPalette(c("darkred", "white", "darkblue"))(100), 
  addCoef.col = "white",  # Use black for coefficients for better contrast
  diag = TRUE, 
  addgrid.col = NA, 
  tl.srt = 45, 
  tl.cex = 1.2, 
  cl.pos = "n", 
  mar = c(0, 0, 5, 0), # Margin to accommodate the title
  title = plot_title 
) 

# =============================================================================
# ITAO7106 Marketing Analytics - Queen's University Belfast
# Student Name   : Jai Tajvir Baloda
# Student ID     : 40485958
# Assignment     : Market STP Strategy and 4P Marketing Mix
# Submission Date: 15th May 2026
# =============================================================================

# =============================================================================
# SECTION 0: PACKAGE INSTALLATION AND LIBRARY LOADING
# =============================================================================

# I installed and loaded all libraries required across the entire assignment
# in a single unified block at the outset. This ensures all dependencies are
# available before any analysis begins and avoids redundant install/library
# calls throughout the script.

# if (!require("tidyverse"))    install.packages("tidyverse")
# if (!require("lubridate"))    install.packages("lubridate")
# if (!require("scales"))       install.packages("scales")
# if (!require("ggcorrplot"))   install.packages("ggcorrplot")
# if (!require("RColorBrewer")) install.packages("RColorBrewer")
# if (!require("patchwork"))    install.packages("patchwork")
# if (!require("knitr"))        install.packages("knitr")
# if (!require("gridExtra"))    install.packages("gridExtra")
# if (!require("cluster"))      install.packages("cluster")
# if (!require("factoextra"))   install.packages("factoextra")
# if (!require("dendextend"))   install.packages("dendextend")
# if (!require("MASS"))         install.packages("MASS")
# if (!require("caret"))        install.packages("caret")
# if (!require("fmsb"))         install.packages("fmsb")

library(tidyverse)
library(lubridate)
library(scales)
library(ggcorrplot)
library(RColorBrewer)
library(patchwork)
library(knitr)
library(gridExtra)
library(cluster)
library(factoextra)
library(dendextend)
library(MASS)
library(caret)
library(fmsb)

# I overrode the select() conflict caused by MASS masking dplyr::select,
# ensuring all subsequent select() calls use the dplyr version throughout.
select <- dplyr::select

# =============================================================================
# PART 1 — STEP 1: EXPLORATORY DATA ANALYSIS (EDA)
# Datasets: All five datasets loaded, cleaned, and inspected here.
#           Retail + Prospect are the primary datasets for Part 1 analysis.
#           Conjoint, Product Profiles, and Product Attributes are cleaned
#           here and saved for use in Part 2.
# =============================================================================

# -----------------------------------------------------------------------------
# SECTION 1: DATA LOADING — ALL FIVE DATASETS
# -----------------------------------------------------------------------------

# I loaded all five datasets at the outset so that each could be inspected,
# cleaned, and saved in a single, self-contained EDA script.

# Part 1 datasets
retail_raw    <- read_csv("Retail transaction data.csv",       show_col_types = FALSE)
prospect_raw  <- read_csv("Prospect customers info.csv",       show_col_types = FALSE)

# Part 2 datasets
conjoint_raw  <- read_csv("Conjoint survey results-1.csv",     show_col_types = FALSE)
profiles_raw  <- read_csv("Product profiles.csv",              show_col_types = FALSE)
prod_attr_raw <- read_csv("Product attributes information.csv",show_col_types = FALSE)

# I confirmed the dimensions of each dataset to verify successful loading.
cat("=== Dataset Dimensions ===\n")
cat("Retail transaction data      :", dim(retail_raw),    "\n")
cat("Prospect customers info      :", dim(prospect_raw),  "\n")
cat("Conjoint survey results      :", dim(conjoint_raw),  "\n")
cat("Product profiles             :", dim(profiles_raw),  "\n")
cat("Product attributes info      :", dim(prod_attr_raw), "\n")

# -----------------------------------------------------------------------------
# SECTION 2: INITIAL INSPECTION — STRUCTURE AND MISSING VALUES
# -----------------------------------------------------------------------------

cat("\n=== RETAIL: Structure ===\n");    glimpse(retail_raw)
cat("\n=== PROSPECT: Structure ===\n");  glimpse(prospect_raw)
cat("\n=== CONJOINT: Structure ===\n");  glimpse(conjoint_raw)
cat("\n=== PROFILES: Structure ===\n");  glimpse(profiles_raw)
cat("\n=== PROD ATTR: Structure ===\n"); glimpse(prod_attr_raw)

cat("\n=== Missing Values Summary ===\n")
cat("Retail:\n");    print(colSums(is.na(retail_raw)))
cat("Prospect:\n");  print(colSums(is.na(prospect_raw)))
cat("Conjoint:\n");  print(colSums(is.na(conjoint_raw)))
cat("Profiles:\n");  print(colSums(is.na(profiles_raw)))
cat("Prod Attr:\n"); print(colSums(is.na(prod_attr_raw)))

cat("\n=== Duplicate Rows ===\n")
cat("Retail duplicates    :", sum(duplicated(retail_raw)),    "\n")
cat("Prospect duplicates  :", sum(duplicated(prospect_raw)),  "\n")
cat("Conjoint duplicates  :", sum(duplicated(conjoint_raw)),  "\n")
cat("Profiles duplicates  :", sum(duplicated(profiles_raw)),  "\n")
cat("Prod Attr duplicates :", sum(duplicated(prod_attr_raw)), "\n")

# -----------------------------------------------------------------------------
# SECTION 3: DATA CLEANING
# -----------------------------------------------------------------------------

# I defined a reusable outlier-capping (Winsorisation) function using a 3xIQR
# fence. Rather than deleting outlier rows, I capped extreme values to preserve
# all valid transaction records while reducing the distorting effect of extremes.
cap_outliers <- function(x, multiplier = 3) {
  Q1      <- quantile(x, 0.25, na.rm = TRUE)
  Q3      <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower   <- Q1 - multiplier * IQR_val
  upper   <- Q3 + multiplier * IQR_val
  pmin(pmax(x, lower), upper)
}

# I also defined a helper function to apply consistent demographic factor
# labelling across both the retail and prospect datasets.
label_demographics <- function(df) {
  df %>%
    mutate(
      Married = factor(Married,
                       levels = c(0, 1),
                       labels = c("Not Married", "Married")),
      Education = factor(Education,
                         levels = c(1, 2, 3),
                         labels = c("No University Degree",
                                    "Undergraduate",
                                    "Postgraduate")),
      Work = factor(Work,
                    levels = 1:11,
                    labels = c("Health Services",
                               "Financial Services",
                               "Sales",
                               "Advertising / PR",
                               "Education",
                               "Construction / Logistics",
                               "Engineering",
                               "Technology",
                               "Retailing / Services",
                               "SME / Self-Employed",
                               "Transportation"))
    )
}

# ---- 3a. RETAIL TRANSACTION DATA CLEANING ----

# Step 1: I removed the two fully duplicated rows, as these were exact repeat
# entries of the same transaction and would artificially inflate spending totals.
retail_clean <- retail_raw %>% distinct()
cat("\nAfter removing duplicates       :", nrow(retail_clean), "rows\n")

# Step 2: I removed the 36 rows where ProductID was recorded as 'PNA'
# (not available). These rows had no valid product identifier or category
# and would contribute nothing to category-level or RFM analyses.
retail_clean <- retail_clean %>% filter(ProductID != "PNA")
cat("After removing PNA ProductIDs   :", nrow(retail_clean), "rows\n")

# Step 3: I removed the remaining rows with missing ProductCategory values.
retail_clean <- retail_clean %>% filter(!is.na(ProductCategory))
cat("After removing missing Category :", nrow(retail_clean), "rows\n")

# Step 4: I converted InvoiceDate to a proper Date object.
retail_clean <- retail_clean %>%
  mutate(InvoiceDate = as.Date(InvoiceDate, format = "%Y-%m-%d"))

# Step 5: I created a TotalSpend variable as UnitPrice x Quantity.
retail_clean <- retail_clean %>%
  mutate(TotalSpend = UnitPrice * Quantity)

# Step 6: I applied Winsorisation (3xIQR capping) to UnitPrice and Quantity.
# UnitPrice had a maximum of £1,592 against a mean of £4.13, and Quantity
# reached 1,930 against a mean of approximately 10 — both far beyond
# plausible single-item grocery transaction values.
retail_clean <- retail_clean %>%
  mutate(
    UnitPrice  = cap_outliers(UnitPrice),
    Quantity   = cap_outliers(Quantity),
    TotalSpend = UnitPrice * Quantity      # Recalculated after capping
  )

# Step 7: I converted all categorical code columns to labelled factor variables.
retail_clean <- label_demographics(retail_clean)
cat("Final retail_clean dimensions   :", dim(retail_clean), "\n")

# ---- 3b. PROSPECT CUSTOMERS DATA CLEANING ----

# I inspected the prospect dataset and found no missing values, no duplicates,
# and no values outside the valid ranges for any variable. No rows were removed.
prospect_clean <- prospect_raw %>% label_demographics()
cat("Final prospect_clean dimensions :", dim(prospect_clean), "\n")

# ---- 3c. CONJOINT SURVEY DATA CLEANING ----

# I verified that the conjoint dataset contained no missing values, no
# duplicates, and that all 200 respondents rated all 16 product profiles.
conjoint_clean <- conjoint_raw %>%
  mutate(
    format         = factor(format,
                            levels = c("Instant", "Capsule", "Ground", "Whole bean")),
    strength       = factor(strength,
                            levels = c("Mild", "Medium", "Dark"),
                            ordered = TRUE),
    origin         = factor(origin,
                            levels = c("House blend",
                                       "100% Arabica blend",
                                       "Single-origin")),
    sustainability = factor(sustainability, levels = c("No", "Yes"))
  )
cat("Final conjoint_clean dimensions :", dim(conjoint_clean), "\n")

# ---- 3d. PRODUCT PROFILES DATA CLEANING ----

# I confirmed that the product profiles dataset was fully clean — no missing
# values, no duplicates, and exactly 16 orthogonal profiles.
profiles_clean <- profiles_raw %>%
  mutate(
    format         = factor(format,
                            levels = c("Instant", "Capsule", "Ground", "Whole bean")),
    strength       = factor(strength,
                            levels = c("Mild", "Medium", "Dark"),
                            ordered = TRUE),
    origin         = factor(origin,
                            levels = c("House blend",
                                       "100% Arabica blend",
                                       "Single-origin")),
    sustainability = factor(sustainability, levels = c("No", "Yes"))
  )
cat("Final profiles_clean dimensions :", dim(profiles_clean), "\n")

# ---- 3e. PRODUCT ATTRIBUTES DATA CLEANING ----

# I corrected the column name typo 'sustaintability_claim' (extra 'n') to
# 'sustainability_claim'. The low taste_quality score for Starbucks1 (0.239)
# was retained as a genuine low-rated data point after outlier inspection.
prod_attr_clean <- prod_attr_raw %>%
  rename(sustainability_claim = sustaintability_claim) %>%
  mutate(
    brand                = str_extract(product_name, "^[A-Za-z]+"),
    is_decaf             = factor(is_decaf,
                                  levels = c(0, 1),
                                  labels = c("Not Decaf", "Decaf")),
    sustainability_claim = factor(sustainability_claim,
                                  levels = c(0, 1),
                                  labels = c("No Claim", "Sustainability Claim"))
  )
cat("Final prod_attr_clean dimensions:", dim(prod_attr_clean), "\n")

# -----------------------------------------------------------------------------
# SECTION 4: DESCRIPTIVE STATISTICS
# -----------------------------------------------------------------------------

cat("\n=== RETAIL: Descriptive Statistics ===\n")
retail_clean %>%
  select(UnitPrice, Quantity, TotalSpend, Income, Age, HouseholdSize) %>%
  summary() %>% print()

cat("\n=== PROSPECT: Descriptive Statistics ===\n")
prospect_clean %>%
  select(Income, Age, HouseholdSize) %>%
  summary() %>% print()

cat("\n=== CONJOINT: Rating and Price Statistics ===\n")
conjoint_clean %>%
  select(price, rating) %>%
  summary() %>% print()

cat("\n=== PRODUCT ATTRIBUTES: Perceptual Rating Statistics ===\n")
prod_attr_clean %>%
  select(price, price_100g, strength_level,
         convenience, authenticity, premium,
         perceived_sustainability, taste_quality) %>%
  summary() %>% print()

cat("\n--- Key Counts ---\n")
cat("Retail: Unique customers       :", n_distinct(retail_clean$CustomerID),   "\n")
cat("Retail: Unique invoices        :", n_distinct(retail_clean$InvoiceNo),     "\n")
cat("Retail: Date range             :",
    as.character(min(retail_clean$InvoiceDate)), "to",
    as.character(max(retail_clean$InvoiceDate)), "\n")
cat("Retail: Product categories     :", n_distinct(retail_clean$ProductCategory), "\n")
cat("Conjoint: Respondents          :", n_distinct(conjoint_clean$respondent_id),  "\n")
cat("Conjoint: Profiles per person  :", n_distinct(conjoint_clean$productNo),      "\n")
cat("Product attributes: Brands     :", n_distinct(prod_attr_clean$brand),         "\n")

# -----------------------------------------------------------------------------
# SECTION 5: VISUALISATIONS — EDA (Figures 1–8)
# -----------------------------------------------------------------------------

# I defined a consistent custom ggplot theme to ensure a professional and
# uniform visual style is applied to all plots throughout this assignment.
theme_assignment <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle    = element_text(size = 10, hjust = 0.5, colour = "grey45"),
      axis.title       = element_text(size = 11),
      axis.text        = element_text(size = 9),
      legend.title     = element_text(size = 10, face = "bold"),
      legend.text      = element_text(size = 9),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 10)
    )
}

# ---- FIGURE 1: Monthly Transaction Volume Over Time ----
# I plotted the number of transactions per month to understand temporal
# purchasing patterns across the January to August 2025 observation window.

monthly_vol <- retail_clean %>%
  mutate(Month = floor_date(InvoiceDate, "month")) %>%
  count(Month, name = "Transactions")

p1 <- ggplot(monthly_vol, aes(x = Month, y = Transactions)) +
  geom_area(fill = "#2E86AB", alpha = 0.15) +
  geom_line(colour = "#2E86AB", linewidth = 1.3) +
  geom_point(colour = "#2E86AB", size = 3) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(labels = comma, limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Figure 1: Monthly Transaction Volume (Jan–Aug 2025)",
    subtitle = "Number of transaction line items recorded per calendar month",
    x = "Month", y = "Number of Transactions"
  ) +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)

# ---- FIGURE 2: Revenue and Transaction Count by Product Category ----
# I created a dual-panel bar chart showing total revenue and transaction count
# by product category. This segment-wise view identifies the most valuable and
# most frequently purchased categories, supporting the business case for a
# new private-label coffee product.

cat_summary <- retail_clean %>%
  group_by(ProductCategory) %>%
  summarise(Revenue = sum(TotalSpend), Transactions = n(), .groups = "drop")

p2a <- ggplot(cat_summary,
              aes(x = reorder(ProductCategory, Revenue), y = Revenue)) +
  geom_col(fill = "#3A7D44", alpha = 0.85) +
  geom_text(aes(label = paste0("£", round(Revenue / 1000, 1), "k")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = label_dollar(prefix = "£"),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Total Revenue", x = NULL, y = "Revenue (£)") +
  theme_assignment()

p2b <- ggplot(cat_summary,
              aes(x = reorder(ProductCategory, Transactions), y = Transactions)) +
  geom_col(fill = "#E07B39", alpha = 0.85) +
  geom_text(aes(label = comma(Transactions)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Transaction Count", x = NULL, y = "No. of Transactions") +
  theme_assignment()

p2 <- p2a + p2b +
  plot_annotation(
    title    = "Figure 2: Revenue and Transaction Count by Product Category",
    subtitle = "Segment-wise view across all 10 product categories (Jan–Aug 2025)",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
                     plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey45")))
print(p2)

# ---- FIGURE 3: Customer Demographics — Age, Income, Household Size ----
# I produced a three-panel demographic overview for unique current customers
# to understand the distribution of key profiling variables.

cust_demo <- retail_clean %>% distinct(CustomerID, .keep_all = TRUE)

p3a <- ggplot(cust_demo, aes(x = Age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 fill = "#6A5ACD", colour = "white", alpha = 0.8) +
  geom_density(colour = "#3B2F8F", linewidth = 1.1) +
  labs(title = "Age Distribution", x = "Age (years)", y = "Density") +
  theme_assignment()

p3b <- ggplot(cust_demo, aes(x = Income)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 fill = "#2E86AB", colour = "white", alpha = 0.8) +
  geom_density(colour = "#1A5276", linewidth = 1.1) +
  scale_x_continuous(labels = dollar_format(prefix = "£")) +
  labs(title = "Income Distribution", x = "Annual Income (£k)", y = "Density") +
  theme_assignment()

p3c <- cust_demo %>%
  count(HouseholdSize) %>%
  ggplot(aes(x = factor(HouseholdSize), y = n, fill = factor(HouseholdSize))) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.2) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Household Size", x = "No. of People in Household", y = "Count") +
  theme_assignment()

p3 <- p3a + p3b + p3c +
  plot_annotation(
    title    = "Figure 3: Customer Demographics — Age, Income & Household Size",
    subtitle = "One record per unique current customer (n = 925)",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
                     plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey45")))
print(p3)

# ---- FIGURE 4: Education, Marital Status & Occupation ----
# I visualised three remaining demographic variables as a combined figure
# to support customer profiling and segment characterisation in Steps 3–4.

edu_dist <- cust_demo %>% count(Education) %>% mutate(Pct = n / sum(n) * 100)

p4a <- ggplot(edu_dist, aes(x = Education, y = Pct, fill = Education)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")), vjust = -0.4, size = 3.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Education Level", x = NULL, y = "% of Customers") +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))

married_dist <- cust_demo %>%
  count(Married) %>%
  mutate(Pct = n / sum(n) * 100, Label = paste0(Married, "\n", round(Pct, 1), "%"))

p4b <- ggplot(married_dist, aes(x = "", y = Pct, fill = Married)) +
  geom_col(width = 1, colour = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5),
            size = 3.8, fontface = "bold", colour = "white") +
  scale_fill_manual(values = c("Not Married" = "#E07B39", "Married" = "#2E86AB")) +
  labs(title = "Marital Status", fill = NULL) +
  theme_void() +
  theme(plot.title  = element_text(face = "bold", size = 12, hjust = 0.5),
        legend.text = element_text(size = 9))

work_dist <- cust_demo %>%
  count(Work) %>% mutate(Pct = n / sum(n) * 100) %>% arrange(desc(Pct))

p4c <- ggplot(work_dist, aes(x = reorder(Work, Pct), y = Pct)) +
  geom_col(fill = "#3A7D44", alpha = 0.82) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")), hjust = -0.1, size = 2.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Occupation Sector", x = NULL, y = "% of Customers") +
  theme_assignment()

p4 <- (p4a | p4b) / p4c +
  plot_annotation(
    title    = "Figure 4: Education, Marital Status & Occupation of Current Customers",
    subtitle = "Segment-wise demographic breakdown (n = 925 unique customers)",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
                     plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey45")))
print(p4)

# ---- FIGURE 5: Income — Current vs. Prospect Customers ----
# I overlaid the income density curves of current and prospective customers
# to assess demographic similarity. Close overlap supports LDA generalisation.

income_compare <- bind_rows(
  cust_demo      %>% select(Income) %>% mutate(Group = "Current Customers (n=925)"),
  prospect_clean %>% select(Income) %>% mutate(Group = "Prospect Customers (n=5,000)")
)

p5 <- ggplot(income_compare, aes(x = Income, fill = Group, colour = Group)) +
  geom_density(alpha = 0.30, linewidth = 1.1) +
  scale_fill_manual(values   = c("Current Customers (n=925)"    = "#2E86AB",
                                 "Prospect Customers (n=5,000)" = "#E07B39")) +
  scale_colour_manual(values = c("Current Customers (n=925)"    = "#2E86AB",
                                 "Prospect Customers (n=5,000)" = "#E07B39")) +
  scale_x_continuous(labels = dollar_format(prefix = "£")) +
  labs(
    title    = "Figure 5: Income Distribution — Current vs. Prospect Customers",
    subtitle = "Overlapping density curves; close alignment supports LDA generalisation",
    x = "Annual Household Income (£k)", y = "Density", fill = NULL, colour = NULL
  ) +
  theme_assignment()
print(p5)

# ---- FIGURE 6: Segment-wise — Age by Product Category ----
# I created a boxplot showing customer age distributions across product
# categories, directly informing target segment selection for coffee.

p6 <- ggplot(retail_clean,
             aes(x = reorder(ProductCategory, Age, FUN = median),
                 y = Age, fill = ProductCategory)) +
  geom_boxplot(alpha = 0.75, outlier.size = 1, outlier.alpha = 0.4,
               show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title    = "Figure 6: Age Distribution by Product Category",
    subtitle = "Segment-wise boxplot — categories ordered by median customer age",
    x = "Product Category", y = "Customer Age (years)"
  ) +
  theme_assignment()
print(p6)

# ---- FIGURE 7: Segment-wise — Average Spend by Education & Marital Status ----
# I computed average spend broken down by education and marital status to
# identify high-spending demographic segments for strategic targeting.

avg_spend_seg <- retail_clean %>%
  group_by(Education, Married) %>%
  summarise(AvgSpend = mean(TotalSpend), .groups = "drop")

p7 <- ggplot(avg_spend_seg, aes(x = Education, y = AvgSpend, fill = Married)) +
  geom_col(position = "dodge", alpha = 0.85, colour = "white") +
  geom_text(aes(label = paste0("£", round(AvgSpend, 2))),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 3.0) +
  scale_fill_manual(values = c("Not Married" = "#E07B39", "Married" = "#2E86AB")) +
  scale_y_continuous(labels = label_dollar(prefix = "£"),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "Figure 7: Average Spend per Transaction by Education & Marital Status",
    subtitle = "Segment-wise view — interaction of education level and marital status",
    x = "Education Level", y = "Average Spend per Transaction (£)", fill = "Marital Status"
  ) +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
print(p7)

# ---- FIGURE 8: Part 2 Dataset Overview — Conjoint Ratings & Perceptual Scores ----
# I produced a two-panel overview of the Part 2 datasets. The left panel
# confirms conjoint ratings are well-distributed across the 1–7 scale.
# The right panel confirms adequate perceptual variance for PCA in Part 2.

p8a <- ggplot(conjoint_clean, aes(x = factor(rating))) +
  geom_bar(fill = "#6A5ACD", alpha = 0.85) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, size = 3.0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Conjoint Survey: Rating Distribution",
       x = "Purchase Likelihood Rating (1–7)", y = "Frequency") +
  theme_assignment()

prod_attr_long <- prod_attr_clean %>%
  select(product_name, convenience, authenticity,
         premium, perceived_sustainability, taste_quality) %>%
  pivot_longer(-product_name, names_to = "Attribute", values_to = "Score") %>%
  mutate(Attribute = str_to_title(str_replace_all(Attribute, "_", " ")))

p8b <- ggplot(prod_attr_long, aes(x = Attribute, y = Score, fill = Attribute)) +
  geom_boxplot(alpha = 0.75, outlier.size = 1.5, outlier.alpha = 0.5,
               show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 7.5)) +
  labs(title = "Product Attributes: Perceptual Score Distributions",
       x = "Perceptual Attribute", y = "Customer Rating (1–7)") +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p8 <- p8a + p8b +
  plot_annotation(
    title    = "Figure 8: Part 2 Dataset Overview — Conjoint Ratings & Product Perceptions",
    subtitle = paste0("Left: conjoint rating distribution (n=3,200 ratings). ",
                      "Right: perceptual attribute scores across 24 coffee products."),
    theme    = theme(plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
                     plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey45")))
print(p8)

# -----------------------------------------------------------------------------
# SECTION 6: SAVE ALL CLEANED DATASETS
# -----------------------------------------------------------------------------

write_csv(retail_clean,    "retail_clean.csv")
write_csv(prospect_clean,  "prospect_clean.csv")
write_csv(conjoint_clean,  "conjoint_clean.csv")
write_csv(profiles_clean,  "profiles_clean.csv")
write_csv(prod_attr_clean, "prod_attr_clean.csv")

cat("\n=== All Cleaned Datasets Saved Successfully ===\n")
cat("retail_clean.csv     :", nrow(retail_clean),    "rows,", ncol(retail_clean),    "cols\n")
cat("prospect_clean.csv   :", nrow(prospect_clean),  "rows,", ncol(prospect_clean),  "cols\n")
cat("conjoint_clean.csv   :", nrow(conjoint_clean),  "rows,", ncol(conjoint_clean),  "cols\n")
cat("profiles_clean.csv   :", nrow(profiles_clean),  "rows,", ncol(profiles_clean),  "cols\n")
cat("prod_attr_clean.csv  :", nrow(prod_attr_clean), "rows,", ncol(prod_attr_clean), "cols\n")

# =============================================================================
# PART 1 — STEP 2: RFM ANALYSIS
# PART 1 — STEP 3: CLUSTER ANALYSIS (Hierarchical + K-Means)
# =============================================================================

# I loaded the cleaned retail dataset produced in Step 1.
retail_clean <- read_csv("retail_clean.csv", show_col_types = FALSE) %>%
  mutate(
    InvoiceDate = as.Date(InvoiceDate),
    Married     = factor(Married, levels = c("Not Married", "Married")),
    Education   = factor(Education, levels = c("No University Degree",
                                               "Undergraduate", "Postgraduate")),
    Work        = factor(Work, levels = c("Health Services", "Financial Services",
                                          "Sales", "Advertising / PR", "Education",
                                          "Construction / Logistics", "Engineering",
                                          "Technology", "Retailing / Services",
                                          "SME / Self-Employed", "Transportation"))
  )

cat("Retail clean loaded:", nrow(retail_clean), "rows,",
    n_distinct(retail_clean$CustomerID), "unique customers\n")

# -----------------------------------------------------------------------------
# SECTION 7: CALCULATE RECENCY, FREQUENCY, AND MONETARY (RFM)
# -----------------------------------------------------------------------------

# I calculated the three RFM dimensions for each unique customer.
# The reference date was set to one day after the most recent transaction
# (2025-08-17), ensuring the most recent customer receives a Recency of 1 day.

ref_date <- max(retail_clean$InvoiceDate) + 1
cat("Reference date for Recency calculation:", as.character(ref_date), "\n")

rfm_raw <- retail_clean %>%
  group_by(CustomerID) %>%
  summarise(
    Recency   = as.numeric(ref_date - max(InvoiceDate)),
    Frequency = n_distinct(InvoiceNo),
    Monetary  = sum(TotalSpend),
    .groups   = "drop"
  )

cat("\nRFM table dimensions:", dim(rfm_raw), "\n")
cat("\n--- RFM Descriptive Statistics ---\n")
summary(rfm_raw[, c("Recency", "Frequency", "Monetary")])

# -----------------------------------------------------------------------------
# SECTION 8: RFM SCORING — SEQUENTIAL SORTING METHOD
# -----------------------------------------------------------------------------

# I applied the sequential sorting method to assign RFM scores. Customers are
# first ranked by Recency, then Frequency within Recency groups, then Monetary
# within Recency-Frequency groups. This hierarchical approach reflects the
# convention where Recency is the strongest predictor of future behaviour
# (Palmatier et al., 2022).

rfm_sequential <- rfm_raw %>%
  mutate(R_score = ntile(-Recency, 5)) %>%
  group_by(R_score) %>%
  mutate(F_score = ntile(Frequency, 5)) %>%
  group_by(R_score, F_score) %>%
  mutate(M_score = ntile(Monetary, 5)) %>%
  ungroup() %>%
  mutate(
    RFM_Score_Sequential = R_score + F_score + M_score,
    RFM_Cell_Sequential  = paste0(R_score, F_score, M_score)
  )

cat("\n--- Sequential RFM Score Distribution ---\n")
print(table(rfm_sequential$RFM_Score_Sequential))

# I assigned customer segment labels based on the sequential RFM total score.
rfm_sequential <- rfm_sequential %>%
  mutate(Segment_Sequential = case_when(
    RFM_Score_Sequential >= 13 ~ "Champions",
    RFM_Score_Sequential >= 10 ~ "Loyal Customers",
    RFM_Score_Sequential >= 8  ~ "Potential Loyalists",
    RFM_Score_Sequential >= 5  ~ "At Risk",
    TRUE                       ~ "Lost Customers"
  )) %>%
  mutate(Segment_Sequential = factor(Segment_Sequential,
                                     levels = c("Champions", "Loyal Customers",
                                                "Potential Loyalists", "At Risk",
                                                "Lost Customers")))

cat("\n--- Sequential Segment Distribution ---\n")
print(table(rfm_sequential$Segment_Sequential))

# -----------------------------------------------------------------------------
# SECTION 9: RFM SCORING — INDEPENDENT SORTING METHOD
# -----------------------------------------------------------------------------

# I applied the independent sorting method where Recency, Frequency, and
# Monetary are each scored independently using quintiles, without nesting.

rfm_independent <- rfm_raw %>%
  mutate(
    R_score = ntile(-Recency,  5),
    F_score = ntile(Frequency, 5),
    M_score = ntile(Monetary,  5)
  ) %>%
  mutate(
    RFM_Score_Independent = R_score + F_score + M_score,
    RFM_Cell_Independent  = paste0(R_score, F_score, M_score)
  )

cat("\n--- Independent RFM Score Distribution ---\n")
print(table(rfm_independent$RFM_Score_Independent))

rfm_independent <- rfm_independent %>%
  mutate(Segment_Independent = case_when(
    RFM_Score_Independent >= 13 ~ "Champions",
    RFM_Score_Independent >= 10 ~ "Loyal Customers",
    RFM_Score_Independent >= 8  ~ "Potential Loyalists",
    RFM_Score_Independent >= 5  ~ "At Risk",
    TRUE                        ~ "Lost Customers"
  )) %>%
  mutate(Segment_Independent = factor(Segment_Independent,
                                      levels = c("Champions", "Loyal Customers",
                                                 "Potential Loyalists", "At Risk",
                                                 "Lost Customers")))

cat("\n--- Independent Segment Distribution ---\n")
print(table(rfm_independent$Segment_Independent))

# -----------------------------------------------------------------------------
# SECTION 10: RFM TABLES
# -----------------------------------------------------------------------------

cat("\n=== RFM TABLE — SEQUENTIAL METHOD ===\n")
rfm_table_sequential <- rfm_sequential %>%
  group_by(Segment_Sequential) %>%
  summarise(Customers = n(), Avg_Recency = round(mean(Recency), 1),
            Avg_Frequency = round(mean(Frequency), 2),
            Avg_Monetary  = round(mean(Monetary), 2),
            Total_Revenue = round(sum(Monetary), 2), .groups = "drop") %>%
  arrange(desc(Avg_Monetary))
print(rfm_table_sequential)

cat("\n=== RFM TABLE — INDEPENDENT METHOD ===\n")
rfm_table_independent <- rfm_independent %>%
  group_by(Segment_Independent) %>%
  summarise(Customers = n(), Avg_Recency = round(mean(Recency), 1),
            Avg_Frequency = round(mean(Frequency), 2),
            Avg_Monetary  = round(mean(Monetary), 2),
            Total_Revenue = round(sum(Monetary), 2), .groups = "drop") %>%
  arrange(desc(Avg_Monetary))
print(rfm_table_independent)

cat("\n=== METHOD COMPARISON: Segment Assignment Differences ===\n")
comparison <- rfm_sequential %>%
  select(CustomerID, Segment_Sequential) %>%
  left_join(rfm_independent %>% select(CustomerID, Segment_Independent),
            by = "CustomerID") %>%
  mutate(Agreement = Segment_Sequential == Segment_Independent)

cat("Customers assigned to same segment by both methods:",
    sum(comparison$Agreement), "/", nrow(comparison),
    paste0("(", round(mean(comparison$Agreement)*100, 1), "%)\n"))

rfm_combined <- rfm_sequential %>%
  left_join(
    rfm_independent %>%
      select(CustomerID, R_score_ind = R_score, F_score_ind = F_score,
             M_score_ind = M_score, RFM_Score_Independent,
             RFM_Cell_Independent, Segment_Independent),
    by = "CustomerID"
  )

# -----------------------------------------------------------------------------
# SECTION 11: RFM VISUALISATIONS (Figures 9–10)
# -----------------------------------------------------------------------------

seg_colours <- c(
  "Champions"           = "#2E86AB",
  "Loyal Customers"     = "#3A7D44",
  "Potential Loyalists" = "#F4A261",
  "At Risk"             = "#E07B39",
  "Lost Customers"      = "#C0392B"
)

# ---- FIGURE 9: RFM Segment Comparison — Sequential vs. Independent ----
seg_seq <- rfm_sequential %>% count(Segment_Sequential) %>%
  rename(Segment = Segment_Sequential) %>% mutate(Method = "Sequential")
seg_ind <- rfm_independent %>% count(Segment_Independent) %>%
  rename(Segment = Segment_Independent) %>% mutate(Method = "Independent")

seg_compare <- bind_rows(seg_seq, seg_ind) %>%
  mutate(Method = factor(Method, levels = c("Sequential", "Independent")))

p9 <- ggplot(seg_compare, aes(x = Segment, y = n, fill = Segment)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.2, fontface = "bold") +
  facet_wrap(~ Method) +
  scale_fill_manual(values = seg_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "Figure 9: RFM Segment Distribution — Sequential vs. Independent Method",
    subtitle = "Number of customers assigned to each segment under both scoring methods",
    x = "Customer Segment", y = "Number of Customers"
  ) +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
print(p9)

# ---- FIGURE 10: RFM Segment Profiles ----
rfm_profile <- rfm_independent %>%
  group_by(Segment_Independent) %>%
  summarise(Recency = mean(Recency), Frequency = mean(Frequency),
            Monetary = mean(Monetary), .groups = "drop") %>%
  pivot_longer(-Segment_Independent, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = factor(Metric, levels = c("Recency", "Frequency", "Monetary")),
    Segment_Independent = factor(Segment_Independent,
                                 levels = c("Champions", "Loyal Customers",
                                            "Potential Loyalists", "At Risk",
                                            "Lost Customers"))
  )

p10 <- ggplot(rfm_profile,
              aes(x = Segment_Independent, y = Value, fill = Segment_Independent)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = round(Value, 1)), vjust = -0.3, size = 3.0) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = seg_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title    = "Figure 10: RFM Segment Profiles (Independent Method)",
    subtitle = "Mean Recency (days), Frequency (orders), and Monetary (£) per segment",
    x = "Customer Segment", y = "Mean Value"
  ) +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
print(p10)

# -----------------------------------------------------------------------------
# SECTION 12: DATA PREPARATION FOR CLUSTERING
# -----------------------------------------------------------------------------

# I standardised RFM data to zero mean and unit variance before clustering.
# Without standardisation, unscaled Monetary values would dominate distances.

rfm_scaled <- rfm_raw %>%
  column_to_rownames("CustomerID") %>%
  scale() %>%
  as.data.frame()

cat("\n--- Scaled RFM Summary (should have mean ~0, sd ~1) ---\n")
print(summary(rfm_scaled))

# -----------------------------------------------------------------------------
# SECTION 13: HIERARCHICAL CLUSTERING
# -----------------------------------------------------------------------------

# I performed hierarchical clustering using Ward's minimum variance linkage,
# which minimises total within-cluster variance at each merge step and produces
# compact, well-separated clusters (Palmatier et al., 2022).

dist_matrix <- dist(rfm_scaled, method = "euclidean")
hc_model    <- hclust(dist_matrix, method = "ward.D2")

cat("\nHierarchical clustering completed using Ward's linkage.\n")
cat("Number of observations:", length(hc_model$order), "\n")

# ---- FIGURE 11: Dendrogram ----
dend          <- as.dendrogram(hc_model)
dend_coloured <- color_branches(dend, k = 4, col = brewer.pal(4, "Set1"))

p11_func <- function() {
  par(mar = c(4, 4, 4, 2))
  plot(dend_coloured,
       main = "Figure 11: Dendrogram — Hierarchical Clustering (Ward's Method)",
       sub  = "Cut at k = 4 clusters; branch colours indicate cluster assignment",
       ylab = "Height (Within-cluster variance)", leaflab = "none",
       cex.main = 1.1, cex.sub = 0.85, cex.lab = 0.95)
  abline(h = 10, col = "red", lty = 2, lwd = 1.5)
  legend("topright", legend = paste("Cluster", 1:4),
         fill = brewer.pal(4, "Set1"), bty = "n", cex = 0.85)
}
p11_func()

hc_clusters <- cutree(hc_model, k = 4)
cat("\nHierarchical cluster sizes (k=4):\n")
print(table(hc_clusters))

# -----------------------------------------------------------------------------
# SECTION 14: ELBOW METHOD — OPTIMAL NUMBER OF CLUSTERS
# -----------------------------------------------------------------------------

# I applied the elbow method to identify the optimal k for K-means clustering.
# The optimal k is the point where additional clusters yield diminishing
# returns in WSS reduction (James et al., 2013).

set.seed(40485958)
wss_values <- map_dbl(1:10, function(k) {
  km <- kmeans(rfm_scaled, centers = k, nstart = 25, iter.max = 300)
  km$tot.withinss
})

elbow_df <- tibble(k = 1:10, WSS = wss_values)
cat("\n--- Within-cluster Sum of Squares by k ---\n")
print(elbow_df)

# ---- FIGURE 12: Elbow Method Plot ----
p12 <- ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line(colour = "#2E86AB", linewidth = 1.3) +
  geom_point(colour = "#2E86AB", size = 3.5) +
  geom_point(data = filter(elbow_df, k == 4), aes(x = k, y = WSS),
             colour = "#C0392B", size = 5, shape = 18) +
  annotate("text", x = 4.3, y = wss_values[4] + 15,
           label = "Optimal k = 4\n(elbow point)", colour = "#C0392B",
           size = 3.5, hjust = 0) +
  geom_vline(xintercept = 4, colour = "#C0392B", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title    = "Figure 12: Elbow Method — Optimal Number of K-Means Clusters",
    subtitle = "Total within-cluster sum of squares (WSS) for k = 1 to 10",
    x = "Number of Clusters (k)", y = "Total Within-Cluster SS (WSS)"
  ) +
  theme_assignment()
print(p12)

# -----------------------------------------------------------------------------
# SECTION 15: K-MEANS CLUSTERING WITH DIFFERENT INITIAL START POINTS
# -----------------------------------------------------------------------------

# I applied K-means with three different random seed configurations to address
# sensitivity to initial centroid placement. Each run uses nstart = 25.
# The solution with the lowest total WSS is retained as the final model
# (James et al., 2013).

set.seed(42)
km_run1 <- kmeans(rfm_scaled, centers = 4, nstart = 25, iter.max = 300)
cat("\nK-Means Run 1 (seed=42): Total WSS =", round(km_run1$tot.withinss, 2),
    "| Iterations =", km_run1$iter, "\n")
cat("Cluster sizes:", km_run1$size, "\n")

set.seed(123)
km_run2 <- kmeans(rfm_scaled, centers = 4, nstart = 25, iter.max = 300)
cat("\nK-Means Run 2 (seed=123): Total WSS =", round(km_run2$tot.withinss, 2),
    "| Iterations =", km_run2$iter, "\n")
cat("Cluster sizes:", km_run2$size, "\n")

set.seed(2025)
km_run3 <- kmeans(rfm_scaled, centers = 4, nstart = 25, iter.max = 300)
cat("\nK-Means Run 3 (seed=2025): Total WSS =", round(km_run3$tot.withinss, 2),
    "| Iterations =", km_run3$iter, "\n")
cat("Cluster sizes:", km_run3$size, "\n")

best_wss <- min(km_run1$tot.withinss, km_run2$tot.withinss, km_run3$tot.withinss)
km_final <- if (km_run1$tot.withinss == best_wss) km_run1 else
  if (km_run2$tot.withinss == best_wss) km_run2 else km_run3

cat("\nFinal K-Means model selected: Total WSS =", round(km_final$tot.withinss, 2), "\n")
cat("Final cluster sizes:", km_final$size, "\n")

comparison_df <- tibble(
  Run   = c("Run 1 (seed=42)", "Run 2 (seed=123)", "Run 3 (seed=2025)"),
  WSS   = c(km_run1$tot.withinss, km_run2$tot.withinss, km_run3$tot.withinss),
  Iters = c(km_run1$iter, km_run2$iter, km_run3$iter)
)
cat("\n--- K-Means Stability Comparison ---\n")
print(comparison_df)

# -----------------------------------------------------------------------------
# SECTION 16: CLUSTER ASSIGNMENT AND LABELLING
# -----------------------------------------------------------------------------

rfm_clustered <- rfm_raw %>%
  mutate(KMeans_Cluster = factor(km_final$cluster))

rfm_clustered <- rfm_clustered %>%
  left_join(
    rfm_independent %>%
      select(CustomerID, R_score, F_score, M_score,
             RFM_Score_Independent, Segment_Independent),
    by = "CustomerID"
  )

cat("\n=== K-Means Cluster Centroids (Original Units) ===\n")
cluster_profiles <- rfm_clustered %>%
  group_by(KMeans_Cluster) %>%
  summarise(n_customers   = n(),
            Avg_Recency   = round(mean(Recency),   1),
            Avg_Frequency = round(mean(Frequency), 2),
            Avg_Monetary  = round(mean(Monetary),  2), .groups = "drop")
print(cluster_profiles)

# I labelled clusters by inspecting centroid characteristics:
# highest monetary + lowest recency = Champions; lowest across all = Lost Customers.
cluster_labels <- cluster_profiles %>%
  arrange(desc(Avg_Monetary)) %>%
  mutate(Cluster_Label  = c("Champions", "Loyal Customers",
                            "At Risk", "Lost Customers"),
         KMeans_Cluster = as.character(KMeans_Cluster)) %>%
  select(KMeans_Cluster, Cluster_Label)

# I dropped any existing Cluster_Label column before joining to prevent
# duplicate column conflicts if this block is re-run.
rfm_clustered <- rfm_clustered %>%
  mutate(KMeans_Cluster = as.character(KMeans_Cluster)) %>%
  select(-any_of("Cluster_Label")) %>%
  left_join(cluster_labels, by = "KMeans_Cluster") %>%
  mutate(KMeans_Cluster = factor(KMeans_Cluster),
         Cluster_Label  = factor(Cluster_Label,
                                 levels = c("Champions", "Loyal Customers",
                                            "At Risk", "Lost Customers")))

cat("\n--- Final Cluster Label Assignment ---\n")
print(rfm_clustered %>% count(KMeans_Cluster, Cluster_Label) %>% arrange(KMeans_Cluster))

cluster_colours <- c(
  "Champions"       = "#2E86AB",
  "Loyal Customers" = "#3A7D44",
  "At Risk"         = "#F4A261",
  "Lost Customers"  = "#C0392B"
)

# ---- FIGURE 13: K-Means Cluster Scatter Plot ----
p13 <- ggplot(rfm_clustered, aes(x = Recency, y = Monetary, colour = Cluster_Label)) +
  geom_point(alpha = 0.55, size = 1.8) +
  stat_ellipse(aes(fill = Cluster_Label), type = "norm", level = 0.75,
               geom = "polygon", alpha = 0.08, show.legend = FALSE) +
  scale_colour_manual(values = cluster_colours) +
  scale_fill_manual(values   = cluster_colours) +
  scale_y_continuous(labels  = label_dollar(prefix = "£")) +
  labs(
    title    = "Figure 13: K-Means Cluster Solution — Recency vs. Monetary",
    subtitle = "k = 4 clusters; ellipses show 75% confidence region per cluster",
    x = "Recency (days since last purchase)", y = "Total Monetary Value (£)", colour = "Cluster"
  ) +
  theme_assignment()
print(p13)

# -----------------------------------------------------------------------------
# SECTION 17: SAVE CLUSTERING OUTPUTS
# -----------------------------------------------------------------------------

write_csv(rfm_clustered,         "rfm_clustered.csv")
write_csv(rfm_sequential,        "rfm_sequential.csv")
write_csv(rfm_independent,       "rfm_independent.csv")
write_csv(rfm_table_sequential,  "rfm_table_sequential.csv")
write_csv(rfm_table_independent, "rfm_table_independent.csv")

cat("\n=== All RFM and Clustering outputs saved ===\n")
cat("rfm_clustered.csv:", nrow(rfm_clustered), "customers,",
    n_distinct(rfm_clustered$KMeans_Cluster), "clusters\n")

# =============================================================================
# PART 1 — STEP 4: CUSTOMER PROFILING
# PART 1 — STEP 5: LDA, ANOVA, AND CONFUSION MATRIX
# PART 1 — STEP 6: TARGET SEGMENT SELECTION AND JUSTIFICATION
# =============================================================================

# I loaded the cleaned retail dataset and the clustered RFM output.
retail_clean   <- read_csv("retail_clean.csv",   show_col_types = FALSE) %>%
  mutate(InvoiceDate = as.Date(InvoiceDate))
rfm_clustered  <- read_csv("rfm_clustered.csv",  show_col_types = FALSE)
prospect_clean <- read_csv("prospect_clean.csv", show_col_types = FALSE)

# -----------------------------------------------------------------------------
# SECTION 18: MERGE CLUSTER ASSIGNMENTS WITH DEMOGRAPHIC DATA
# -----------------------------------------------------------------------------

# I extracted one demographic record per customer and merged with clustered RFM.

demographics <- retail_clean %>%
  distinct(CustomerID, .keep_all = TRUE) %>%
  select(CustomerID, Age, Income, HouseholdSize, Married, Education, Work)

rfm_full <- rfm_clustered %>%
  left_join(demographics, by = "CustomerID") %>%
  mutate(
    Married   = factor(Married, levels = c("Not Married", "Married")),
    Education = factor(Education, levels = c("No University Degree",
                                             "Undergraduate", "Postgraduate")),
    Work      = factor(Work, levels = c("Health Services", "Financial Services",
                                        "Sales", "Advertising / PR", "Education",
                                        "Construction / Logistics", "Engineering",
                                        "Technology", "Retailing / Services",
                                        "SME / Self-Employed", "Transportation"))
  )

cat("RFM + Demographics dataset:", nrow(rfm_full), "customers\n")
cat("Cluster breakdown:\n")
print(table(rfm_full$Cluster_Label))

# -----------------------------------------------------------------------------
# SECTION 19: CLUSTER PROFILE SUMMARY TABLE
# -----------------------------------------------------------------------------

cluster_profile_table <- rfm_full %>%
  group_by(Cluster_Label) %>%
  summarise(
    N_Customers  = n(),
    Avg_Recency  = round(mean(Recency),      1),
    Avg_Frequency= round(mean(Frequency),    2),
    Avg_Monetary = round(mean(Monetary),     2),
    Avg_Age      = round(mean(Age),          1),
    Avg_Income_k = round(mean(Income),       1),
    Avg_HH_Size  = round(mean(HouseholdSize),2),
    Pct_Married  = round(mean(Married == "Married") * 100, 1),
    .groups      = "drop"
  ) %>%
  arrange(desc(Avg_Monetary))

cat("\n=== CUSTOMER PROFILE TABLE — K-MEANS CLUSTERS (k=4) ===\n")
print(cluster_profile_table)

cat("\n=== Education Distribution by Cluster ===\n")
edu_by_cluster <- rfm_full %>%
  group_by(Cluster_Label, Education) %>% summarise(n = n(), .groups = "drop") %>%
  group_by(Cluster_Label) %>% mutate(Pct = round(n / sum(n) * 100, 1)) %>% ungroup()
print(edu_by_cluster)

cat("\n=== Occupation Distribution by Cluster ===\n")
work_by_cluster <- rfm_full %>%
  group_by(Cluster_Label, Work) %>% summarise(n = n(), .groups = "drop") %>%
  group_by(Cluster_Label) %>% mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  arrange(Cluster_Label, desc(Pct)) %>% ungroup()
print(work_by_cluster)

# Cluster profile narrative:
# CHAMPIONS (n=51):  Recency 18d, Frequency 12.2, Monetary £362 — most engaged,
#                    highest-value customers.
# LOYAL CUSTOMERS (n=220): Recency 32d, Frequency 5.7, Monetary £147 — consistent
#                    engagement, clear growth pathway to Champions.
# AT RISK (n=414):   Recency 48d, Frequency 2.1, Monetary £45 — declining
#                    engagement, at risk of lapsing.
# LOST CUSTOMERS (n=237): Recency 142d, Frequency 1.4, Monetary £33 — highly
#                    disengaged, reactivation requires significant investment.

# -----------------------------------------------------------------------------
# SECTION 20: CUSTOMER PROFILING VISUALISATIONS (Figures 14–15)
# -----------------------------------------------------------------------------

# ---- FIGURE 14: Radar Chart — Cluster Profiles ----
profile_radar <- cluster_profile_table %>%
  select(Cluster_Label, Avg_Recency, Avg_Frequency,
         Avg_Monetary, Avg_Age, Avg_Income_k) %>%
  mutate(Avg_Recency   = 1 - rescale(Avg_Recency),
         Avg_Frequency = rescale(Avg_Frequency),
         Avg_Monetary  = rescale(Avg_Monetary),
         Avg_Age       = rescale(Avg_Age),
         Avg_Income_k  = rescale(Avg_Income_k)) %>%
  column_to_rownames("Cluster_Label")

colnames(profile_radar) <- c("Recency\n(inversed)", "Frequency",
                             "Monetary", "Age", "Income")

radar_data    <- rbind(rep(1, 5), rep(0, 5), profile_radar)
radar_colours <- c("#2E86AB", "#3A7D44", "#F4A261", "#C0392B")

p14_func <- function() {
  par(mar = c(1, 1, 2, 1))
  radarchart(radar_data, axistype = 1, pcol = radar_colours,
             pfcol = adjustcolor(radar_colours, alpha.f = 0.15), plwd = 2.5,
             cglcol = "grey70", cglty = 1, axislabcol = "grey40",
             caxislabels = c("0","0.25","0.5","0.75","1"), vlcex = 0.9,
             title = "Figure 14: Customer Cluster Profiles — Radar Chart\n(Normalised dimensions)")
  legend(x = "topright", legend = rownames(profile_radar),
         col = radar_colours, lty = 1, lwd = 2, bty = "n", cex = 0.82)
}
p14_func()

# ---- FIGURE 15: Cluster Size and Revenue Contribution ----
cluster_revenue <- rfm_full %>%
  group_by(Cluster_Label) %>%
  summarise(N_Customers = n(), Total_Revenue = sum(Monetary), .groups = "drop") %>%
  mutate(Revenue_Share = Total_Revenue / sum(Total_Revenue) * 100)

p15a <- ggplot(cluster_revenue,
               aes(x = reorder(Cluster_Label, -N_Customers),
                   y = N_Customers, fill = Cluster_Label)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = N_Customers), vjust = -0.4, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = cluster_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Customer Count per Cluster", x = "Cluster", y = "Number of Customers") +
  theme_assignment()

p15b <- ggplot(cluster_revenue,
               aes(x = reorder(Cluster_Label, -Total_Revenue),
                   y = Total_Revenue, fill = Cluster_Label)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0("£", round(Total_Revenue/1000, 1), "k\n(",
                               round(Revenue_Share, 1), "%)")),
            vjust = -0.3, size = 3.2, lineheight = 1.1) +
  scale_fill_manual(values = cluster_colours) +
  scale_y_continuous(labels = label_dollar(prefix = "£"),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Total Revenue Contribution per Cluster",
       x = "Cluster", y = "Total Revenue (£)") +
  theme_assignment()

p15 <- p15a + p15b +
  plot_annotation(
    title    = "Figure 15: Cluster Size and Revenue Contribution",
    subtitle = "Customer count and cumulative monetary value generated per segment",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
                     plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey45")))
print(p15)

# -----------------------------------------------------------------------------
# SECTION 21: LDA MODEL — TRAINING AND EVALUATION
# -----------------------------------------------------------------------------

# I applied LDA to assess cluster separability and to classify prospects.
# The model uses both RFM (behavioural) and demographic features for maximum
# discriminant power (Palmatier et al., 2022).

rfm_lda_data <- rfm_full %>%
  mutate(Married_num   = as.numeric(Married) - 1,
         Education_num = as.numeric(Education),
         Work_num      = as.numeric(Work),
         Cluster_num   = as.numeric(KMeans_Cluster))

lda_features <- c("Recency", "Frequency", "Monetary",
                  "Age", "Income", "HouseholdSize",
                  "Married_num", "Education_num", "Work_num")

set.seed(45)
train_idx  <- createDataPartition(rfm_lda_data$KMeans_Cluster, p = 0.80, list = FALSE)
train_data <- rfm_lda_data[ train_idx, ]
test_data  <- rfm_lda_data[-train_idx, ]

cat("\nTraining set:", nrow(train_data), "customers\n")
cat("Test set    :", nrow(test_data),  "customers\n")
cat("Train cluster distribution:\n"); print(table(train_data$Cluster_Label))
cat("Test cluster distribution:\n");  print(table(test_data$Cluster_Label))

# I constructed the model formula and fitted LDA on the training split.
lda_formula <- as.formula(paste("KMeans_Cluster ~",
                                paste(lda_features, collapse = " + ")))
lda_model   <- lda(lda_formula, data = train_data)

cat("\n=== LDA MODEL SUMMARY ===\n")
print(lda_model)

# I generated class predictions on training and test sets.
pred_train_obj <- predict(lda_model, newdata = train_data)
pred_train     <- pred_train_obj$class
pred_test_obj  <- predict(lda_model, newdata = test_data)
pred_test      <- pred_test_obj$class

train_acc <- mean(pred_train == train_data$KMeans_Cluster)
test_acc  <- mean(pred_test  == test_data$KMeans_Cluster)

cat(sprintf("\nTraining Accuracy : %.2f%%\n", train_acc * 100))
cat(sprintf("Test Accuracy     : %.2f%%\n",  test_acc  * 100))

# ── ANOVA TEST ────────────────────────────────────────────────────────────────
# I ran one-way ANOVA on each RFM feature to formally test whether cluster means
# are statistically different — a necessary condition for meaningful LDA.
cat("\n=== ANOVA: Cluster Separation Tests ===\n")
for (var in c("Recency", "Frequency", "Monetary")) {
  aov_result <- aov(as.formula(paste(var, "~ KMeans_Cluster")), data = rfm_lda_data)
  cat(sprintf("\n%s ~ Cluster:\n", var))
  print(summary(aov_result))
}

# ── CONFUSION MATRIX ─────────────────────────────────────────────────────────
test_actual_factor <- factor(test_data$KMeans_Cluster)
test_pred_factor   <- factor(pred_test, levels = levels(test_actual_factor))

cm <- confusionMatrix(data = test_pred_factor, reference = test_actual_factor)
cat("\n=== CONFUSION MATRIX — TEST SET ===\n")
print(cm)

cm_df <- as.data.frame(cm$table) %>% rename(Predicted = Prediction, Actual = Reference)
level_labels <- c("1" = "At Risk", "2" = "Lost Customers",
                  "3" = "Champions", "4" = "Loyal Customers")

cm_df <- cm_df %>%
  mutate(Predicted = recode(as.character(Predicted), !!!level_labels),
         Actual    = recode(as.character(Actual),    !!!level_labels))

# ── PROSPECT PREDICTION ───────────────────────────────────────────────────────
# I applied the fitted LDA model to all 5,000 prospect customers.
prospect_lda_data <- prospect_clean %>%
  mutate(
    Married_num   = as.numeric(factor(Married, levels = c("Not Married", "Married"))) - 1,
    Education_num = as.numeric(factor(Education, levels = c("No University Degree",
                                                            "Undergraduate",
                                                            "Postgraduate"))),
    Work_num      = as.numeric(factor(Work,
                                      levels = c("Health Services", "Financial Services",
                                                 "Sales", "Advertising / PR", "Education",
                                                 "Construction / Logistics", "Engineering",
                                                 "Technology", "Retailing / Services",
                                                 "SME / Self-Employed", "Transportation")))
  )

# I created dummy RFM columns set to training medians, since prospects have
# no transaction history. Demographics drive the prediction in practice.
prospect_lda_data <- prospect_lda_data %>%
  mutate(Recency   = median(train_data$Recency),
         Frequency = median(train_data$Frequency),
         Monetary  = median(train_data$Monetary))

prospect_pred_obj <- predict(lda_model, newdata = prospect_lda_data)
prospect_clean    <- prospect_clean %>%
  mutate(KMeans_Cluster = prospect_pred_obj$class,
         Cluster_Label  = recode(as.character(KMeans_Cluster), !!!level_labels),
         Cluster_Label  = factor(Cluster_Label,
                                 levels = c("Champions", "Loyal Customers",
                                            "At Risk", "Lost Customers")))

cat("\n=== PROSPECT SEGMENT PREDICTIONS ===\n")
print(table(prospect_clean$Cluster_Label))

# ---- FIGURE 16: Confusion Matrix Heatmap + Prospect Distribution ----
p16a <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = Freq), size = 5, fontface = "bold",
            colour = ifelse(cm_df$Freq > (max(cm_df$Freq) / 2), "white", "grey20")) +
  scale_fill_gradient(low = "#EAF4FB", high = "#2E86AB", name = "Count") +
  labs(title    = "Confusion Matrix (Test Set)",
       subtitle = paste0("Overall accuracy: ", round(test_acc * 100, 1), "%"),
       x = "Predicted Cluster", y = "Actual Cluster") +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "right")

prospect_dist <- prospect_clean %>% count(Cluster_Label) %>% mutate(Pct = n / sum(n) * 100)

p16b <- ggplot(prospect_dist, aes(x = reorder(Cluster_Label, -n), y = n, fill = Cluster_Label)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, "\n(", round(Pct, 1), "%)")),
            vjust = -0.3, size = 3.5, lineheight = 1.1) +
  scale_fill_manual(values = cluster_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title    = "Prospect Customers: Predicted Segment",
       subtitle = "LDA-predicted cluster for 5,000 prospect customers",
       x = "Predicted Segment", y = "Number of Prospects") +
  theme_assignment()

p16 <- p16a + p16b +
  plot_annotation(
    title    = "Figure 16: LDA Performance — Confusion Matrix & Prospect Targeting",
    subtitle = "Left: test set classification accuracy. Right: prospect segment predictions.",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
                     plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "grey45")))
print(p16)

# -----------------------------------------------------------------------------
# SECTION 22: TARGET SEGMENT SELECTION AND JUSTIFICATION
# -----------------------------------------------------------------------------

# I selected LOYAL CUSTOMERS as the primary target segment for the new PB
# coffee product based on the following evidence:
#
# 1. SIZE: n=220 — commercially viable, focused enough for targeted strategy.
# 2. REVENUE: avg £147.24/customer, second-highest revenue pool (£32,392 total).
# 3. ENGAGEMENT: avg 32 days recency, 5.72 orders — active, regular purchasers.
# 4. GROWTH: clear pathway to Champions; bridging gap = strategic opportunity.
# 5. NOT CHAMPIONS: already maximally engaged; cannibalisation risk outweighs gain.
# 6. NOT AT RISK/LOST: insufficient frequency/spend to justify launch investment.
# 7. DEMOGRAPHICS: avg age 54, income £68k, HH size 2.84 — quality-conscious
#    adults aligned with a premium, sustainable PB coffee proposition.

cat("\n=== STEP 6: TARGET SEGMENT — LOYAL CUSTOMERS ===\n")
cat("\nKey metrics for selected target segment:\n")

loyal_profile <- rfm_full %>%
  filter(Cluster_Label == "Loyal Customers") %>%
  summarise(N_Customers   = n(),
            Avg_Recency   = round(mean(Recency),   1),
            Avg_Frequency = round(mean(Frequency), 2),
            Avg_Monetary  = round(mean(Monetary),  2),
            Total_Revenue = round(sum(Monetary),   2),
            Avg_Age       = round(mean(Age),       1),
            Avg_Income_k  = round(mean(Income),    1),
            Pct_Married   = round(mean(Married == "Married") * 100, 1),
            Avg_HH_Size   = round(mean(HouseholdSize), 2))
print(t(loyal_profile))

# ---- FIGURE 17: Target Segment Spotlight ----
spotlight_data <- rfm_full %>%
  group_by(Cluster_Label) %>%
  summarise(`Avg Monetary (£)` = mean(Monetary), `Avg Frequency` = mean(Frequency),
            `Avg Recency (days, lower=better)` = mean(Recency), .groups = "drop") %>%
  pivot_longer(-Cluster_Label, names_to = "Metric", values_to = "Value") %>%
  mutate(IsTarget      = ifelse(Cluster_Label == "Loyal Customers",
                                "Target Segment", "Other Segments"),
         Cluster_Label = factor(Cluster_Label,
                                levels = c("Champions", "Loyal Customers",
                                           "At Risk", "Lost Customers")))

p17 <- ggplot(spotlight_data,
              aes(x = Cluster_Label, y = Value, fill = Cluster_Label, alpha = IsTarget)) +
  geom_col() +
  geom_text(aes(label = round(Value, 1)), vjust = -0.4, size = 3.2) +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = cluster_colours) +
  scale_alpha_manual(values = c("Target Segment" = 1.0, "Other Segments" = 0.45)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(title    = "Figure 17: Target Segment Selection — Loyal Customers Spotlight",
       subtitle = "Highlighted segment selected for PB coffee product targeting (Part 2)",
       x = "Customer Segment", y = "Mean Value", fill = "Cluster", alpha = "Role") +
  theme_assignment() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")
print(p17)

# -----------------------------------------------------------------------------
# SECTION 23: SAVE PART 1 OUTPUTS
# -----------------------------------------------------------------------------

write_csv(rfm_full,       "rfm_profiled.csv")
write_csv(prospect_clean, "prospect_targeted.csv")

cat("\n=== All outputs saved ===\n")
cat("rfm_profiled.csv      :", nrow(rfm_full),       "customers\n")
cat("prospect_targeted.csv :", nrow(prospect_clean), "prospects\n")
cat("\n=== PART 1 COMPLETE — Target segment: LOYAL CUSTOMERS (n=220) ===\n")

# =============================================================================
# PART 2 — STEP 1: FRACTIONAL FACTORIAL DESIGN
# PART 2 — STEP 2: PART-WORTHS AND WILLINGNESS TO PAY
# PART 2 — STEP 3: ATTRIBUTE IMPORTANCE
# =============================================================================

# I loaded the three Part 2 datasets.
conjoint_clean <- read_csv(
  "Conjoint survey results-1.csv",
  show_col_types = FALSE) %>%
  mutate(format         = factor(format, levels = c("Instant","Capsule","Ground","Whole bean")),
         strength       = factor(strength, levels = c("Mild","Medium","Dark")),
         origin         = factor(origin, levels = c("House blend","100% Arabica blend",
                                                    "Single-origin")),
         sustainability = factor(sustainability, levels = c("No","Yes")))

profiles_clean <- read_csv(
  "Product profiles.csv",
  show_col_types = FALSE) %>%
  mutate(format         = factor(format, levels = c("Instant","Capsule","Ground","Whole bean")),
         strength       = factor(strength, levels = c("Mild","Medium","Dark")),
         origin         = factor(origin, levels = c("House blend","100% Arabica blend",
                                                    "Single-origin")),
         sustainability = factor(sustainability, levels = c("No","Yes")))

prod_attr_clean <- read_csv(
  "Product attributes information.csv",
  show_col_types = FALSE) %>%
  rename(sustainability_claim = sustaintability_claim) %>%
  mutate(brand = str_extract(product_name, "^[A-Za-z]+"))

cat("Conjoint loaded  :", nrow(conjoint_clean), "rows,",
    n_distinct(conjoint_clean$respondent_id), "respondents\n")
cat("Profiles loaded  :", nrow(profiles_clean), "profiles\n")
cat("Prod attr loaded :", nrow(prod_attr_clean), "products\n")

# -----------------------------------------------------------------------------
# SECTION 24: STEP 1 — WHY ONLY 16 PRODUCT PROFILES?
# -----------------------------------------------------------------------------

# I calculated the full factorial design size to demonstrate why a subset was
# required. With 5 attributes at 4, 4, 3, 3, and 2 levels, the total possible
# combinations are 4 x 4 x 3 x 3 x 2 = 288 profiles. Asking 200 respondents
# to rate all 288 is cognitively infeasible. A fractional factorial design
# selects the minimum orthogonal subset needed to estimate all part-worths.

full_factorial <- 4 * 4 * 3 * 3 * 2
cat("\n=== STEP 1: FRACTIONAL FACTORIAL DESIGN ===\n")
cat("Full factorial size (4 x 4 x 3 x 3 x 2):", full_factorial, "profiles\n")
cat("Selected orthogonal subset              :", nrow(profiles_clean), "profiles\n")
cat("Reduction factor                        :", round(full_factorial / nrow(profiles_clean), 1), "x\n")

cat("\n--- Attribute Level Frequency in the 16 Profiles ---\n")
cat("Price levels:\n");         print(table(profiles_clean$price))
cat("Format levels:\n");        print(table(profiles_clean$format))
cat("Strength levels:\n");      print(table(profiles_clean$strength))
cat("Origin levels:\n");        print(table(profiles_clean$origin))
cat("Sustainability levels:\n"); print(table(profiles_clean$sustainability))

params_needed <- (4-1) + (4-1) + (3-1) + (3-1) + (2-1) + 1
df_residual   <- nrow(profiles_clean) - params_needed
cat("\nParameters to estimate (incl. intercept):", params_needed, "\n")
cat("Profiles available                       :", nrow(profiles_clean), "\n")
cat("Residual degrees of freedom              :", df_residual, "\n")
cat("Conclusion: 16 profiles is the minimum orthogonal subset that allows\n")
cat("all", params_needed - 1, "part-worths + intercept to be estimated independently.\n")

# -----------------------------------------------------------------------------
# SECTION 25: STEP 2 — PART-WORTH COMPUTATION AND WTP
# -----------------------------------------------------------------------------

# I computed individual-level part-worths by running a separate OLS regression
# for each of the 200 respondents. Reference levels: Price £3, Format Instant,
# Strength Mild, Origin House blend, Sustainability No.

cat("\n=== STEP 2: INDIVIDUAL-LEVEL PART-WORTHS ===\n")

respondent_ids <- unique(conjoint_clean$respondent_id)

partworth_list <- lapply(respondent_ids, function(rid) {
  df_r  <- conjoint_clean %>% filter(respondent_id == rid)
  model <- lm(rating ~ factor(price) + format + strength + origin + sustainability,
              data = df_r)
  coef_vec <- coef(model); coef_vec["respondent_id"] <- rid; coef_vec
})

all_coef_names <- unique(unlist(lapply(partworth_list, names)))
partworth_df <- do.call(rbind, lapply(partworth_list, function(x) {
  out <- setNames(rep(NA_real_, length(all_coef_names)), all_coef_names)
  out[names(x)] <- x; out
})) %>%
  as.data.frame() %>%
  rename(Intercept        = `(Intercept)`,
         Price_4          = `factor(price)4`,
         Price_5          = `factor(price)5`,
         Price_6          = `factor(price)6`,
         Format_Capsule   = `formatCapsule`,
         Format_Ground    = `formatGround`,
         Format_Wholebean = `formatWhole bean`,
         Strength_Medium  = `strengthMedium`,
         Strength_Dark    = `strengthDark`,
         Origin_Arabica   = `origin100% Arabica blend`,
         Origin_Single    = `originSingle-origin`,
         Sustain_Yes      = `sustainabilityYes`) %>%
  mutate(respondent_id = respondent_ids)

cat("Part-worth matrix dimensions:", nrow(partworth_df), "respondents x",
    ncol(partworth_df) - 1, "coefficients\n")

mean_pw <- partworth_df %>%
  select(-respondent_id) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

cat("\n--- Mean Part-Worths Across 200 Respondents ---\n")
print(t(mean_pw))

# ── WILLINGNESS TO PAY (WTP) — AGGREGATE METHOD ──────────────────────────────
# I calculated WTP using aggregate mean part-worths — the standard approach
# (Orme, 2006; Palmatier et al., 2022). Individual-level price slopes can be
# near-zero for some respondents, causing extreme WTP values if averaged directly.

cat("\n=== WILLINGNESS TO PAY (WTP) — AGGREGATE METHOD ===\n")

mean_pw_price_vec <- c(0, mean_pw$Price_4, mean_pw$Price_5, mean_pw$Price_6)
price_vals_vec    <- c(3, 4, 5, 6)
agg_price_slope   <- coef(lm(mean_pw_price_vec ~ price_vals_vec))[2]

cat(sprintf("Aggregate price sensitivity: %.4f utility per £1 increase\n", agg_price_slope))

wtp_agg <- tibble(
  Attribute = c("Format: Capsule","Format: Ground","Format: Whole Bean",
                "Strength: Medium","Strength: Dark",
                "Origin: 100% Arabica","Origin: Single-Origin","Sustainability: Yes"),
  PartWorth = c(mean_pw$Format_Capsule, mean_pw$Format_Ground, mean_pw$Format_Wholebean,
                mean_pw$Strength_Medium, mean_pw$Strength_Dark,
                mean_pw$Origin_Arabica, mean_pw$Origin_Single, mean_pw$Sustain_Yes),
  WTP = c(-mean_pw$Format_Capsule, -mean_pw$Format_Ground, -mean_pw$Format_Wholebean,
          -mean_pw$Strength_Medium, -mean_pw$Strength_Dark,
          -mean_pw$Origin_Arabica, -mean_pw$Origin_Single,
          -mean_pw$Sustain_Yes) / agg_price_slope,
  Category = c("Format","Format","Format","Strength","Strength",
               "Origin","Origin","Sustainability")
) %>% arrange(desc(WTP))

cat("\n--- Aggregate WTP (£, vs reference levels) ---\n")
cat("Reference: Instant | Mild | House blend | No sustainability\n\n")
print(wtp_agg %>% mutate(across(c(PartWorth, WTP), ~ round(.x, 3))))

# ---- FIGURE 18: Mean Part-Worth Utilities ----
# I visualised mean part-worths to show the direction and magnitude of consumer
# preferences. Reference levels are shown at zero.

pw_plot_df <- tribble(
  ~Attribute,       ~Level,                ~PartWorth,
  "Price",          "£3 (ref)",             0,
  "Price",          "£4",                   mean_pw$Price_4,
  "Price",          "£5",                   mean_pw$Price_5,
  "Price",          "£6",                   mean_pw$Price_6,
  "Format",         "Instant (ref)",         0,
  "Format",         "Capsule",              mean_pw$Format_Capsule,
  "Format",         "Ground",               mean_pw$Format_Ground,
  "Format",         "Whole Bean",           mean_pw$Format_Wholebean,
  "Strength",       "Mild (ref)",            0,
  "Strength",       "Medium",               mean_pw$Strength_Medium,
  "Strength",       "Dark",                 mean_pw$Strength_Dark,
  "Origin",         "House blend (ref)",     0,
  "Origin",         "100% Arabica",         mean_pw$Origin_Arabica,
  "Origin",         "Single-Origin",        mean_pw$Origin_Single,
  "Sustainability", "No (ref)",              0,
  "Sustainability", "Yes",                  mean_pw$Sustain_Yes
) %>%
  mutate(Attribute = factor(Attribute, levels = c("Price","Format","Strength",
                                                  "Origin","Sustainability")),
         Direction = ifelse(PartWorth >= 0, "Positive", "Negative"))

p18 <- ggplot(pw_plot_df, aes(x = reorder(Level, PartWorth), y = PartWorth, fill = Direction)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = round(PartWorth, 3),
                hjust = ifelse(PartWorth >= 0, -0.1, 1.1)), size = 3.0) +
  coord_flip() +
  facet_wrap(~ Attribute, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Positive" = "#2E86AB", "Negative" = "#C0392B")) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(title    = "Figure 18: Mean Part-Worth Utilities by Attribute Level",
       subtitle = "Average across 200 respondents; reference levels shown at zero",
       x = "Attribute Level", y = "Part-Worth Utility") +
  theme_assignment()
print(p18)

# ---- FIGURE 19: Willingness to Pay ----
p19 <- ggplot(wtp_agg, aes(x = reorder(Attribute, WTP), y = WTP, fill = Category)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = paste0("£", round(WTP, 2)),
                hjust = ifelse(WTP >= 0, -0.12, 1.12)), size = 3.2, fontface = "bold") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = label_dollar(prefix = "£"),
                     expand = expansion(mult = c(0.3, 0.3))) +
  labs(title    = "Figure 19: Willingness to Pay by Attribute Level",
       subtitle = "Aggregate WTP (£) vs reference levels (Instant | Mild | House blend | No sustainability)",
       x = "Attribute Level", y = "Willingness to Pay (£)", fill = "Attribute") +
  theme_assignment()
print(p19)

# -----------------------------------------------------------------------------
# SECTION 26: STEP 3 — ATTRIBUTE IMPORTANCE
# -----------------------------------------------------------------------------

# I computed attribute importance as the normalised range of part-worths across
# all levels of each attribute, summing to 100% (Orme, 2006).

cat("\n=== STEP 3: ATTRIBUTE IMPORTANCE ===\n")

importance_list <- lapply(respondent_ids, function(rid) {
  row <- partworth_df %>% filter(respondent_id == rid)
  
  range_price    <- max(0, row$Price_4, row$Price_5, row$Price_6, na.rm = TRUE) -
    min(0, row$Price_4, row$Price_5, row$Price_6, na.rm = TRUE)
  range_format   <- max(0, row$Format_Capsule, row$Format_Ground,
                        row$Format_Wholebean, na.rm = TRUE) -
    min(0, row$Format_Capsule, row$Format_Ground,
        row$Format_Wholebean, na.rm = TRUE)
  range_strength <- max(0, row$Strength_Medium, row$Strength_Dark, na.rm = TRUE) -
    min(0, row$Strength_Medium, row$Strength_Dark, na.rm = TRUE)
  range_origin   <- max(0, row$Origin_Arabica, row$Origin_Single, na.rm = TRUE) -
    min(0, row$Origin_Arabica, row$Origin_Single, na.rm = TRUE)
  range_sustain  <- max(0, row$Sustain_Yes, na.rm = TRUE) -
    min(0, row$Sustain_Yes, na.rm = TRUE)
  
  total <- range_price + range_format + range_strength + range_origin + range_sustain
  
  data.frame(respondent_id      = rid,
             Imp_Price          = range_price    / total * 100,
             Imp_Format         = range_format   / total * 100,
             Imp_Strength       = range_strength / total * 100,
             Imp_Origin         = range_origin   / total * 100,
             Imp_Sustainability = range_sustain  / total * 100)
})

importance_df <- bind_rows(importance_list)

importance_summary <- importance_df %>%
  select(-respondent_id) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Attribute", values_to = "Importance") %>%
  mutate(Attribute = recode(Attribute, "Imp_Price" = "Price", "Imp_Format" = "Format",
                            "Imp_Strength" = "Strength", "Imp_Origin" = "Origin",
                            "Imp_Sustainability" = "Sustainability")) %>%
  arrange(desc(Importance)) %>%
  mutate(Importance = round(Importance, 2))

cat("\n--- Mean Attribute Importance Scores (%) ---\n")
print(importance_summary)
cat("\nSum of importance scores:", round(sum(importance_summary$Importance), 1), "%\n")

# ---- FIGURE 20: Attribute Importance Bar Chart ----
attr_colours <- c("Price" = "#C0392B", "Format" = "#2E86AB", "Strength" = "#3A7D44",
                  "Origin" = "#F4A261", "Sustainability" = "#6A5ACD")

p20 <- ggplot(importance_summary,
              aes(x = reorder(Attribute, Importance), y = Importance, fill = Attribute)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Importance, 1), "%")),
            hjust = -0.1, size = 4.0, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = attr_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Figure 20: Relative Attribute Importance in Coffee Purchase Decisions",
       subtitle = "Mean importance scores (%) across 200 respondents; scores sum to 100%",
       x = "Attribute", y = "Mean Importance (%)") +
  theme_assignment()
print(p20)

importance_sd <- importance_df %>%
  select(-respondent_id) %>%
  summarise(across(everything(), ~ sd(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Attribute", values_to = "SD") %>%
  mutate(Attribute = recode(Attribute, "Imp_Price" = "Price", "Imp_Format" = "Format",
                            "Imp_Strength" = "Strength", "Imp_Origin" = "Origin",
                            "Imp_Sustainability" = "Sustainability"))

importance_full <- importance_summary %>%
  left_join(importance_sd, by = "Attribute") %>%
  mutate(SD = round(SD, 2))

cat("\n--- Attribute Importance with Standard Deviation ---\n")
print(importance_full)

write_csv(partworth_df,    "partworth_individual.csv")
write_csv(wtp_agg,         "wtp_aggregate.csv")
write_csv(importance_df,   "importance_individual.csv")
write_csv(importance_full, "importance_summary.csv")



# =============================================================================
# PART 2 — STEP 4: MARKET SHARE PREDICTION
# =============================================================================

# I used the first-choice rule (maximum utility) to predict market share.
# Each respondent is assumed to choose the product with the highest predicted
# utility. Market share = proportion of respondents choosing each product.

cat("\n=== STEP 4: MARKET SHARE PREDICTION ===\n")

market_profiles <- tribble(
  ~Profile,         ~price, ~format,   ~strength, ~origin,              ~sustainability,
  "Our PB Product",      5, "Capsule", "Dark",    "100% Arabica blend", "Yes",
  "Competitor A",        4, "Capsule", "Medium",  "House blend",        "No",
  "Competitor B",        6, "Capsule", "Dark",    "100% Arabica blend", "No",
  "Competitor C",        5, "Capsule", "Mild",    "Single-origin",      "Yes"
)

cat("\n--- Competitive Set ---\n")
print(market_profiles)

predict_utility <- function(pw_row, price, format, strength, origin, sustain) {
  price_pw    <- case_when(price == 3 ~ 0, price == 4 ~ pw_row$Price_4,
                           price == 5 ~ pw_row$Price_5, price == 6 ~ pw_row$Price_6)
  format_pw   <- case_when(format == "Instant" ~ 0, format == "Capsule" ~ pw_row$Format_Capsule,
                           format == "Ground" ~ pw_row$Format_Ground,
                           format == "Whole bean" ~ pw_row$Format_Wholebean)
  strength_pw <- case_when(strength == "Mild" ~ 0, strength == "Medium" ~ pw_row$Strength_Medium,
                           strength == "Dark" ~ pw_row$Strength_Dark)
  origin_pw   <- case_when(origin == "House blend" ~ 0,
                           origin == "100% Arabica blend" ~ pw_row$Origin_Arabica,
                           origin == "Single-origin" ~ pw_row$Origin_Single)
  sustain_pw  <- ifelse(sustain == "Yes", pw_row$Sustain_Yes, 0)
  pw_row$Intercept + price_pw + format_pw + strength_pw + origin_pw + sustain_pw
}

utility_matrix <- lapply(respondent_ids, function(rid) {
  pw_row <- partworth_df %>% filter(respondent_id == rid)
  utils  <- sapply(1:nrow(market_profiles), function(i) {
    predict_utility(pw_row, price = market_profiles$price[i],
                    format = market_profiles$format[i],
                    strength = market_profiles$strength[i],
                    origin = market_profiles$origin[i],
                    sustain = market_profiles$sustainability[i])
  })
  names(utils) <- market_profiles$Profile
  as.data.frame(t(utils)) %>% mutate(respondent_id = rid)
}) %>% bind_rows()

cat("\n--- First 6 respondents' predicted utilities ---\n")
print(head(utility_matrix))

utility_matrix <- utility_matrix %>%
  mutate(Choice = apply(select(., -respondent_id), 1, function(x) names(which.max(x))))

market_share <- utility_matrix %>%
  count(Choice) %>%
  mutate(Market_Share_Pct = round(n / sum(n) * 100, 1),
         Choice = factor(Choice, levels = market_profiles$Profile)) %>%
  arrange(Choice)

cat("\n--- Market Share Prediction (First-Choice Rule) ---\n")
print(market_share)

# ---- FIGURE 21: Market Share ----
share_colours <- c("Our PB Product" = "#2E86AB", "Competitor A" = "#E07B39",
                   "Competitor B" = "#C0392B",   "Competitor C" = "#3A7D44")

p21 <- ggplot(market_share,
              aes(x = reorder(Choice, -Market_Share_Pct), y = Market_Share_Pct, fill = Choice)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(Market_Share_Pct, "%\n(n=", n, ")")),
            vjust = -0.3, size = 4.0, fontface = "bold", lineheight = 1.1) +
  scale_fill_manual(values = share_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "Figure 21: Predicted Market Share — First-Choice Rule",
       subtitle = "Capsule, £5, Dark, 100% Arabica, Sustainable vs. three competitors",
       x = "Product", y = "Predicted Market Share (%)") +
  theme_assignment()
print(p21)

utility_means <- utility_matrix %>%
  select(-respondent_id, -Choice) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 3))) %>%
  pivot_longer(everything(), names_to = "Profile", values_to = "Mean_Utility") %>%
  arrange(desc(Mean_Utility))

cat("\n--- Mean Predicted Utility per Product ---\n")
print(utility_means)

write_csv(market_share,   "market_share_predictions.csv")
write_csv(utility_matrix, "utility_matrix.csv")


# =============================================================================
# PART 2 — STEP 5: PCA — MARKET POSITIONING
# =============================================================================

# I performed PCA on the product attributes dataset to identify the key
# dimensions of consumer perception and position our PB product on a
# perceptual map relative to 24 existing market competitors.

cat("\n=== STEP 5: PCA — MARKET POSITIONING ===\n")

pca_vars <- c("price_100g", "strength_level", "convenience", "authenticity",
              "premium", "perceived_sustainability", "taste_quality")

pca_data <- prod_attr_clean %>% select(product_name, brand, all_of(pca_vars))

cat("PCA input:", nrow(pca_data), "products x", length(pca_vars), "variables\n")

# I standardised all variables before PCA — essential because they are on
# different scales (e.g. price_100g in £ vs perceptual ratings 1–7).
pca_scaled <- pca_data %>% select(all_of(pca_vars)) %>% scale()
rownames(pca_scaled) <- pca_data$product_name

pca_result <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)

cat("\n--- Singular Values ---\n")
cat("(Square roots of eigenvalues of the covariance matrix)\n")
print(round(pca_result$sdev, 4))

pve     <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_pve <- cumsum(pve)

pve_df <- tibble(
  PC          = paste0("PC", 1:length(pve)),
  SingularVal = round(pca_result$sdev, 4),
  Eigenvalue  = round(pca_result$sdev^2, 4),
  PVE         = round(pve * 100, 2),
  Cum_PVE     = round(cum_pve * 100, 2)
)

cat("\n--- PVE Table ---\n")
print(pve_df)
cat(sprintf("\nPC1 + PC2 explain %.1f%% of total variance\n", pve_df$Cum_PVE[2]))

loadings_df <- as.data.frame(pca_result$rotation[, 1:4]) %>%
  rownames_to_column("Variable") %>%
  mutate(Variable = recode(Variable,
                           "price_100g" = "Price/100g", "strength_level" = "Strength",
                           "convenience" = "Convenience", "authenticity" = "Authenticity",
                           "premium" = "Premium", "perceived_sustainability" = "Sustainability",
                           "taste_quality" = "Taste Quality"))

cat("\n--- Loading Matrix (PC1 to PC4) ---\n")
print(loadings_df %>% mutate(across(where(is.numeric), ~ round(.x, 4))))

# ---- FIGURE 22: Scree Plot ----
p22 <- ggplot(pve_df, aes(x = PC, y = PVE, group = 1)) +
  geom_line(colour = "#2E86AB", linewidth = 1.3) +
  geom_point(colour = "#2E86AB", size = 4) +
  geom_col(aes(y = PVE), fill = "#2E86AB", alpha = 0.15) +
  geom_text(aes(label = paste0(PVE, "%")), vjust = -0.6, size = 3.2, fontface = "bold") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Figure 22: PCA Scree Plot — Proportion of Variance Explained",
       subtitle = "Each bar/point shows the % variance captured by each principal component",
       x = "Principal Component", y = "Proportion of Variance Explained (%)") +
  theme_assignment()
print(p22)

# ---- FIGURE 23: Loading Plot ----
# I visualised PC1 and PC2 loadings to interpret what each component represents.
# PC1 = Mainstream (Convenience/Strength/Price) vs Artisan (Authenticity/Sustainability).
# PC2 = Taste Quality / Sensory dimension.

loading_plot_df <- loadings_df %>% select(Variable, PC1, PC2)

p23 <- ggplot(loading_plot_df, aes(x = PC1, y = PC2, label = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "#2E86AB", linewidth = 1.0) +
  geom_text(nudge_x = 0.02, nudge_y = 0.03, size = 3.5, fontface = "bold", colour = "grey20") +
  coord_cartesian(xlim = c(-0.65, 0.65), ylim = c(-0.65, 0.65)) +
  labs(title    = "Figure 23: PCA Loading Plot — PC1 vs PC2",
       subtitle = "Arrows show variable contributions; direction indicates association",
       x = paste0("PC1 (", pve_df$PVE[1], "% variance)"),
       y = paste0("PC2 (", pve_df$PVE[2], "% variance)")) +
  theme_assignment()
print(p23)

# ---- FIGURE 24: Biplot + Perceptual Map ----
# I constructed a biplot overlaying product scores with loading arrows.
# Our PB product is projected onto the space using assumed perceptual values:
# £6.50/100g, strength 7 (dark), convenience 6.5, authenticity 4.0,
# premium 5.0, perceived sustainability 5.5, taste quality 5.5.

scores_df <- as.data.frame(pca_result$x[, 1:2]) %>%
  rownames_to_column("product_name") %>%
  left_join(pca_data %>% select(product_name, brand), by = "product_name")

scale_factor <- max(abs(scores_df[, c("PC1","PC2")])) /
  max(abs(pca_result$rotation[, 1:2])) * 0.6

arrow_df <- loading_plot_df %>%
  mutate(PC1_scaled = PC1 * scale_factor, PC2_scaled = PC2 * scale_factor)

pb_product <- data.frame(price_100g = 6.50, strength_level = 7, convenience = 6.5,
                         authenticity = 4.0, premium = 5.0,
                         perceived_sustainability = 5.5, taste_quality = 5.5)

pca_means <- attr(pca_scaled, "scaled:center")
pca_sds   <- attr(pca_scaled, "scaled:scale")
pb_scaled <- (pb_product - pca_means) / pca_sds
pb_scores <- as.matrix(pb_scaled) %*% pca_result$rotation[, 1:2]

pb_df <- data.frame(product_name = "Our PB Product",
                    PC1 = pb_scores[1, 1], PC2 = pb_scores[1, 2], brand = "PB")

all_scores <- bind_rows(scores_df, pb_df) %>%
  mutate(IsOurs = product_name == "Our PB Product")

brand_colours <- c("Costa" = "#3A7D44", "KENCO" = "#E07B39", "Lavazza" = "#6A5ACD",
                   "Nescafe" = "#F4A261", "Starbucks" = "#2E86AB", "Taylors" = "#1A5276",
                   "Waitrose" = "#8E44AD", "PB" = "#C0392B")

p24 <- ggplot() +
  geom_segment(data = arrow_df,
               aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey50", linewidth = 0.8) +
  geom_text(data = arrow_df,
            aes(x = PC1_scaled * 1.12, y = PC2_scaled * 1.12, label = Variable),
            size = 2.8, colour = "grey30", fontface = "italic") +
  geom_point(data = all_scores,
             aes(x = PC1, y = PC2, colour = brand, size = IsOurs, shape = IsOurs)) +
  geom_text(data = all_scores,
            aes(x = PC1, y = PC2 + 0.12, label = product_name, colour = brand),
            size = 2.5, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  scale_colour_manual(values = brand_colours) +
  scale_size_manual(values  = c("FALSE" = 2.5, "TRUE" = 5.0)) +
  scale_shape_manual(values = c("FALSE" = 16,  "TRUE" = 18)) +
  labs(title    = "Figure 24: Perceptual Map — PCA Biplot (PC1 vs PC2)",
       subtitle = "Product scores + attribute loadings; red diamond = our proposed PB product",
       x = paste0("PC1 (", pve_df$PVE[1], "% variance)"),
       y = paste0("PC2 (", pve_df$PVE[2], "% variance)"), colour = "Brand") +
  guides(size = "none", shape = "none") +
  theme_assignment()
print(p24)

write_csv(pve_df,      "pca_pve.csv")
write_csv(loadings_df, "pca_loadings.csv")
write_csv(scores_df,   "pca_scores.csv")






# load required packages ---------------------------
source("load.R")

# ask user for file input or specify a default
file_name <- readline(prompt = "Enter ASOS station CSV filename (e.g., GSO.csv): ")
if (file_name == "") {
  file_name <- "GSO.csv"  # Default if user does not enter a file name
}

# load data
data <- read.csv(file.path("data", file_name))

# convert 'valid' column to datetime format
data <- data %>%
  mutate(
    valid = ymd_hm(valid), 
    day_month = format(valid, "%m-%d"),  
    year = year(valid)
  )

# convert columns to numeric, replacing "null" with NA
data$tmpf <- as.numeric(ifelse(data$tmpf == "null", NA, data$tmpf))
data$p01i <- as.numeric(ifelse(data$p01i == "null", NA, data$p01i))

# summary of tmpf column
print(summary(data$tmpf))
print(sum(is.na(data$tmpf)))

# aggregate daily data
daily_data <- data %>%
  group_by(day_month, year) %>%
  summarise(
    min_tmpf = min(tmpf, na.rm = TRUE),
    max_tmpf = max(tmpf, na.rm = TRUE),
    mean_tmpf = mean(tmpf, na.rm = TRUE),
    mean_p01i = ifelse(all(is.na(p01i) | p01i == 0), NA, sum(p01i[p01i > 0], na.rm = TRUE)),
    .groups = "drop"
  )

# function to check trends
check_trend <- function(day_data, value_column) {
  if (nrow(day_data) < 2 || all(is.na(day_data[[value_column]]))) {
    return(tibble(slope = NA, p_value = NA))
  }
  
  model <- lm(reformulate("year", value_column), data = day_data)
  slope <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  
  return(tibble(slope = slope, p_value = p_value))
}

# apply trend analysis
trend_results <- bind_rows(
  daily_data %>%
    group_by(day_month) %>%
    do(check_trend(., "min_tmpf")) %>%
    mutate(metric = "min"),
  
  daily_data %>%
    group_by(day_month) %>%
    do(check_trend(., "max_tmpf")) %>%
    mutate(metric = "max"),
  
  daily_data %>%
    group_by(day_month) %>%
    do(check_trend(., "mean_tmpf")) %>%
    mutate(metric = "mean")
) %>%
  ungroup()

# print and save results
print("Trend results:")
print(head(trend_results))

significant_trends <- trend_results %>%
  filter(p_value < 0.05)

print(significant_trends)

write.csv(significant_trends, paste0("significant_trends_", file_name, ".csv"), row.names = FALSE)
write.csv(trend_results, paste0("trend_results_", file_name, ".csv"), row.names = FALSE)

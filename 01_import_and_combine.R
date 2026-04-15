# Cyclistic Bike-Share Case Study
# Phase: Prepare + Process
# Data source: Divvy trip data (public), provided by Motivate International Inc. / Lyft
# License: Divvy Data License Agreement
# Analysis period: March 2025 – February 2026 (12 months)
# Goal: Combine monthly files → clean → add required derived columns (ride_length, day_of_week)

library(tidyverse)
library(lubridate)
library(janitor)
library(vroom)

# ── 1. Find and list all monthly CSV files ───────────────────────────────────
csv_files <- list.files(
  path       = "data_raw/",
  pattern    = "\\d{6}-divvy-tripdata\\.csv",
  full.names = TRUE
)

# ── 2. Read and combine all 12 monthly files into one data frame ─────────────
all_trips <- vroom(
  csv_files,
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    started_at = as_datetime(started_at),
    ended_at   = as_datetime(ended_at)
  )

# Quick checks after combining
glimpse(all_trips)
nrow(all_trips)
table(all_trips$member_casual, useNA = "always")

# Capture original row count before cleaning
nrow_before <- nrow(all_trips)

# ── 3. Add required derived columns + basic cleaning ─────────────────────────
all_trips <- all_trips |>
  mutate(
    ride_length_sec = as.numeric(difftime(ended_at, started_at, units = "secs")),
    ride_length_min = ride_length_sec / 60,
    day_of_week_num = wday(started_at),
    day_of_week     = wday(started_at, label = TRUE, abbr = FALSE)
  ) |>
  filter(
    ride_length_sec > 60,
    ride_length_sec < 24 * 60 * 60
  )

# ── Quick checks after cleaning ──────────────────────────────────────────────
glimpse(all_trips)

# ── Show cleaning impact ─────────────────────────────────────────────────────
message("Rides before cleaning: ", format(nrow_before, big.mark = ","))
message("Rides after cleaning:  ", format(nrow(all_trips), big.mark = ","))
message("Removed: ", format(nrow_before - nrow(all_trips), big.mark = ","),
        " (", round(100 * (nrow_before - nrow(all_trips)) / nrow_before, 1), "%)")

message("Negative ride lengths after cleaning: ", sum(all_trips$ride_length_sec < 0))

# ── 4. Save the cleaned dataset ──────────────────────────────────────────────
if (!dir.exists("data_processed")) {
  dir.create("data_processed")
}

saveRDS(all_trips, "data_processed/all_trips_cleaned.rds")

message("✅ Cleaned dataset saved successfully!")
message("   File: data_processed/all_trips_cleaned.rds")
message("   Total rides saved: ", format(nrow(all_trips), big.mark = ","))

# ── 5. Analyze: Member vs Casual Usage Differences ───────────────────────────

cat("\n=== ANALYZE: MEMBER VS CASUAL DIFFERENCES ===\n")

# Basic calculations requested in the guide
cat("\n--- Basic Calculations (Mean, Max, Mode) ---\n")

mean_ride <- mean(all_trips$ride_length_min, na.rm = TRUE)
message("Mean ride length (overall): ", round(mean_ride, 2), " minutes")

max_ride <- max(all_trips$ride_length_min, na.rm = TRUE)
message("Maximum ride length (overall): ", round(max_ride, 2), " minutes")

# Full ranking of days by number of rides
cat("\n--- Rides by Day of Week (Most to Least) ---\n")

day_ranking <- all_trips |>
  count(day_of_week, name = "total_rides") |>
  arrange(desc(total_rides))

print(day_ranking)

message("Most frequent day (Mode): ", day_ranking$day_of_week[1], 
        " with ", format(day_ranking$total_rides[1], big.mark = ","), " rides")

# Key comparisons by user type
cat("\n--- Key Comparisons by User Type ---\n")

ride_summary <- all_trips |>
  group_by(member_casual) |>
  summarise(
    total_rides     = n(),
    avg_ride_min    = mean(ride_length_min, na.rm = TRUE),
    median_ride_min = median(ride_length_min, na.rm = TRUE),
    max_ride_min    = max(ride_length_min, na.rm = TRUE)
  )

print(ride_summary)

day_summary <- all_trips |>
  count(member_casual, day_of_week, name = "total_rides") |>
  arrange(member_casual, desc(total_rides))

print(day_summary)

weekend_summary <- all_trips |>
  mutate(is_weekend = if_else(day_of_week_num %in% c(1, 7), "Weekend", "Weekday")) |>
  group_by(member_casual, is_weekend) |>
  summarise(
    avg_ride_min = mean(ride_length_min, na.rm = TRUE),
    total_rides  = n(),
    .groups = "drop"
  )

print(weekend_summary)

# ── Export summary files for further analysis ────────────────────────────────
write_csv(ride_summary, "data_processed/ride_summary.csv")
write_csv(day_summary,  "data_processed/rides_by_day.csv")

message("\n✅ Analysis summaries exported to data_processed/ folder")


# ── 6. Share: Sophisticated Visualizations for Executives ─────────────────────

library(ggplot2)

# Professional theme
theme_cyclistic <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "grey30", hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# 1. Average Ride Length by User Type
p1 <- ggplot(ride_summary, aes(x = member_casual, y = avg_ride_min, fill = member_casual)) +
  geom_col(width = 0.65, alpha = 0.95) +
  geom_text(aes(label = round(avg_ride_min, 1)), vjust = -0.5, size = 5.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("casual" = "#FF7F0E", "member" = "#1F77B4")) +
  labs(
    title = "Casual Riders Take Significantly Longer Rides",
    subtitle = "Average ride duration in minutes",
    x = NULL,
    y = "Average Ride Length (minutes)",
    fill = "User Type"
  ) +
  theme_cyclistic

print(p1)   # ← Important: print each plot

# 2. Number of Rides by Day of Week
p2 <- ggplot(day_summary, aes(x = day_of_week, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge", alpha = 0.9) +
  facet_wrap(~ member_casual, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("casual" = "#FF7F0E", "member" = "#1F77B4")) +
  labs(
    title = "Number of Rides by Day of Week",
    subtitle = "Members ride more consistently, casual riders peak on weekends",
    x = "Day of Week",
    y = "Total Number of Rides",
    fill = "User Type"
  ) +
  theme_cyclistic +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# 3. Weekend vs Weekday Average Ride Length
p3 <- ggplot(weekend_summary, aes(x = is_weekend, y = avg_ride_min, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c("casual" = "#FF7F0E", "member" = "#1F77B4")) +
  labs(
    title = "Weekend vs Weekday Ride Behavior",
    subtitle = "Casual riders ride much longer on weekends",
    x = NULL,
    y = "Average Ride Length (minutes)",
    fill = "User Type"
  ) +
  theme_cyclistic

print(p3)

# 4. Total Rides by User Type
p4 <- ggplot(ride_summary, aes(x = member_casual, y = total_rides, fill = member_casual)) +
  geom_col(width = 0.65, alpha = 0.95) +
  geom_text(aes(label = format(total_rides, big.mark = ",")), 
            vjust = -0.5, size = 5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("casual" = "#FF7F0E", "member" = "#1F77B4")) +
  labs(
    title = "Total Number of Rides",
    subtitle = "Members complete far more rides than casual users",
    x = NULL,
    y = "Total Rides",
    fill = "User Type"
  ) +
  theme_cyclistic

print(p4)

message("\n✅ 4 polished visualizations created for executive presentation")


# ── 7. Act: Top 3 Recommendations ────────────────────────────────────────────

cat("\n=== TOP 3 RECOMMENDATIONS FOR CONVERTING CASUAL RIDERS TO MEMBERS ===\n\n")

cat("1. Target Weekend Leisure Riders with Membership Incentives\n")
cat("   Casual riders take significantly longer rides on weekends (22.7 min vs 18.2 min on weekdays).\n")
cat("   Offer weekend-specific membership perks (e.g., free extended rides on Sat/Sun or discounted weekend passes)\n")
cat("   to convert leisure users who already love the service.\n\n")

cat("2. Highlight Time Savings and Consistency for Commuters\n")
cat("   Members ride more consistently during the week and have shorter average rides (12.2 min).\n")
cat("   Market the membership as a reliable, time-saving commuting tool with priority bike access\n")
cat("   or guaranteed availability during peak hours.\n\n")

cat("3. Create a 'Try Before You Buy' Conversion Campaign\n")
cat("   With over 1.9 million casual rides, there's a huge pool of users already familiar with Cyclistic.\n")
cat("   Offer a 30-day trial membership at a discounted rate or 'ride 10 times and get the 11th free'\n")
cat("   to lower the barrier for casual users to become members.\n")


# Save the summary objects so the Rmd can load them
saveRDS(ride_summary,     "data_processed/ride_summary.rds")
saveRDS(day_summary,      "data_processed/day_summary.rds")
saveRDS(weekend_summary,  "data_processed/weekend_summary.rds")
saveRDS(day_ranking,      "data_processed/day_ranking.rds")

saveRDS(p1, "data_processed/plot_p1.rds")
saveRDS(p2, "data_processed/plot_p2.rds")
saveRDS(p3, "data_processed/plot_p3.rds")
saveRDS(p4, "data_processed/plot_p4.rds")

message("✅ Summary objects and plots saved for R Markdown report")


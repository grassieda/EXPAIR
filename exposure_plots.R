
rm(list = ls(all.names = TRUE)) # clear working directory

library(ggplot2)
library(tidyverse)
library(zoo)
library(dplyr)

age_group <- "81-120"
season <- "heating"
day_type <- "weekend"
BaseWD <- "U:/Projects & research/INHABIT/Code Sharing folder/EXPAIR v5/"
figspath <- paste0(BaseWD, "figures/")
infile <- paste0(BaseWD,"outputdata/exposure_run_",age_group,"_",season,"_",day_type,"_dt_10_.csv")

# window size: 8 hours = 48 rows (10‑min sampling)
rolling_duration <- 8
k <- (60*rolling_duration) / 10   # window size (8 hours)

##########################

df <- read.csv(infile)
df <- df %>% select(-X)
df_summary <- df %>% 
  rowwise() %>% 
  mutate( mean = mean(c_across(where(is.numeric)), na.rm=TRUE), 
          median = median(c_across(where(is.numeric)), na.rm=TRUE), 
          min = min(c_across(where(is.numeric)), na.rm=TRUE), 
          #max = max(c_across(where(is.numeric)), na.rm=TRUE) 
          ) %>% 
  ungroup()

df_stats <- df_summary %>% select(min, median, mean)
df_stats <- cbind(Time = df$Time, df_stats)
df_stats$Time <- factor(df_stats$Time, levels = df_stats$Time)

df_roll <- df_stats %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ {
      x <- .x
      n <- length(x)
      
      # pad on BOTH ends for true circularity
      x_pad <- c(x[(n - (k - 2)):n], x, x[1:(k - 1)])
      
      # centered rolling window
      roll <- rollapply(
        x_pad,
        width = k,
        FUN = mean,
        align = "center",
        fill = NA
      )
      
      # extract the middle n values (fully circular)
      roll[(k):(k + n - 1)]
    },
    .names = "{.col}_roll"
  ))


# original data → long
df_long <- df_stats %>%
  select(Time, min, median, mean) %>%
  pivot_longer(
    cols = c(min, median, mean),
    names_to = "statistic",
    values_to = "value"
  ) %>%
  mutate(type = "original")

# rolling data → long
df_roll_long <- df_roll %>%
  select(Time, ends_with("_roll")) %>%
  pivot_longer(
    cols = ends_with("_roll"),
    names_to = "statistic",
    values_to = "value"
  ) %>%
  mutate(
    statistic = sub("_roll$", "", statistic),  # match names to original
    type = paste(rolling_duration,"hr rolling")
  )

df_plot <- bind_rows(df_long, df_roll_long)
plot_title <- paste("Exposure ", age_group, "years,", day_type, season, "season")
# create a safe filename (remove spaces and punctuation) 
file_name <- paste0(gsub("[^A-Za-z0-9]+", "_", plot_title,"_nomax"), ".png")
full_path <- file.path(figspath, file_name)

three_hour_ticks <- c("00:00","03:00","06:00","09:00", "12:00","15:00","18:00","21:00")
p <- ggplot(df_plot, aes(x = Time, y = value,
                    colour = statistic, linetype = type,
                    group = interaction(statistic, type)))+
  geom_line() +
  scale_x_discrete(breaks = three_hour_ticks) +
  scale_y_continuous(limits = c(0, 25), expand = c(0,0)) +
  scale_linetype_manual(values = c( "original" = "solid", "8 hr rolling" = "dashed" ))+
  labs(y = "concentration (ug/m3)",
       title = plot_title) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(full_path, plot = p, width = 8, height = 5, dpi = 300)

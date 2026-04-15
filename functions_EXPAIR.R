## Library of functions used in EXPAIR code
main <- function(population, pollutant, act_data, conc_inputdata, plot=TRUE, write_csv=TRUE, extra="_"){
  popgroup_act<-population
  timestep_act<-length(act_data)/(24*60)
  avail_menvs_act<-names(act_data)[-1]
  avail_seasons   <- extract_unique_field(conc_inputdata, "season")
  avail_day_types <- extract_unique_field(conc_inputdata, "day_type")
  avail_menvs     <- extract_unique_field(conc_inputdata, "micro_env")
  nc <- length(avail_menvs)
  
  # Check same list of menvs in activity and concentration data
  menvs_not_in_act_data<-setdiff(avail_menvs, avail_menvs_act)
  menvs_not_in_conc_data<-setdiff(avail_menvs_act, avail_menvs)
  if (length(menvs_not_in_act_data) > 0 || length(menvs_not_in_conc_data) > 0){
    stop(paste("Error: The following micro-envs are not given in conc data: ", menvs_not_in_conc_data,
               "and the following micro-envs are not given in activity data: ", menvs_not_in_act_data))
  }
  # Check menvs from (a) conc, (b) activities data are consistent for each season-day pairing
  for (season in avail_seasons){
    for(day_type in avail_day_types){
      subset_conc_inputdata <- Filter(function(x) {
        is.list(x) && "season" %in% names(x) && "day_type" %in% names(x) && x$season == season && x$day_type == day_type
      }, conc_inputdata)
      
      subset_menvs<-unique(unlist(lapply(subset_conc_inputdata, function(x) x$micro_env)))
      missing_menvs <- setdiff(avail_menvs, subset_menvs)
      if (length(missing_menvs) > 0) {
        stop(paste("Error: For ", season, day_type,"there is no microenvironment data for: ", paste(missing_menvs, collapse = ", ")))
      }
      #dgedit the following section is no longer required due to Not-app being redundant
  # Create single mean, sd dfs for all menv concentrations based on outdoor concentration timesteps
      outdoor_conc_list <- Filter(function(x) x$micro_env == "Out", subset_conc_inputdata)
      outdoor_conc_df <- outdoor_conc_list[[1]]$conc_data
      means_df <- data.frame(time=outdoor_conc_df$hour,Out=outdoor_conc_df$mean) 
      sd_df <- data.frame(time=means_df$time, out = 0)

  # time step consistency between activity and concentrations
      timestep_conc <-length(means_df$time)
      dt<-(24*60)/timestep_conc
      times <- as.character(means_df$time)
      if (timestep_act < timestep_conc){
        daily_profiles <- interpolate_df(act_data, timestep_conc)
      } else if (timestep_act > timestep_conc){
        daily_profiles <- condense_df(act_data, timestep_conc)
        # Convert minutes to HH:MM format
        daily_profiles <- daily_profiles %>%
          mutate(HHMM = sprintf("%02d:%02d", as.numeric(Time) %/% 3600, (as.numeric(Time) %% 3600) %/% 60)) %>%  # Format hours & minutes
          select(-Time) %>%
          rename(Time=HHMM)
        daily_profiles <- move_column(daily_profiles, "Time", 1)
        daily_profiles <- as.data.frame(daily_profiles)
      } else daily_profiles <- act_data
        
  # Loop over microenvironments to to ensure (a) no multiple/missing records, (b) timestep consistency 
      for (menv in avail_menvs){
        if (menv!="Out"){
          filtered_conc_inputdata <- Filter(function(x) x$micro_env == menv, subset_conc_inputdata)
          n_single <- length(filtered_conc_inputdata)
          if (n_single == 0) { 
            stop(paste("No results found for: ", menv, " for the ", season, day_type, " case."))
          } else if (n_single > 1) { 
          stop(paste("Multiple results found for: ", menv, " for the ", season, day_type, " case."))
          }
          single_conc_inputdata <- filtered_conc_inputdata[[1]]$conc_data
          single_conc_inputdata <- setNames(single_conc_inputdata, c("time","mean","sd"))
          timestep_menv_conc <-length(single_conc_inputdata$time)
          if (timestep_menv_conc != timestep_conc){
            stop(paste(menv, " concentrations have different timesteps from outdoor concentrations"))
          }          
  # (b) read in mean, sd concentration for each menv and put in a single df
          #print(str(means_df))
          #print(str(sd_df))
          means_df <-data.frame(means_df,single_conc_inputdata$mean,check.names = FALSE)
          names(means_df)[ncol(means_df)] <- menv
          sd_df <-data.frame(sd_df,single_conc_inputdata$sd,check.names = FALSE)
          names(sd_df)[ncol(sd_df)] <- menv
        }
      }
      #print(str(avail_menvs))
      #print(str(daily_profiles))
      #print(str(times))
      #print(str(means_df))
      #print(str(sd_df))
      names(daily_profiles) <- gsub("\\.", "-", names(daily_profiles))
      #print(str(daily_profiles))
      df_samples <- create_sampled_df(avail_menvs, daily_profiles, times, means_df, sd_df)
      df_samples$Time <- times
      df_samples <- move_column(df_samples, "Time", 1)
      print(str(df_samples))
      if (write_csv==TRUE){
        write.csv(df_samples, paste0(outpath,"exposure_run_",population,"_",season,"_",day_type,"_dt_",dt,extra,".csv"), row.names=TRUE)
      }
      
      row_summary <- apply(df_samples[,!names(df_samples) %in% "Time"], 1, function(x) {
        c(
          Min = min(x, na.rm = TRUE),
          Q1 = quantile(x, 0.25, na.rm = TRUE),  # First quartile (25%)
          Median = median(x, na.rm = TRUE),      # Median (50%)
          Q3 = quantile(x, 0.75, na.rm = TRUE),  # Third quartile (75%)
          Max = max(x, na.rm = TRUE),
          Mean = mean(x, na.rm=TRUE)
        )
      })
      row_summary_df <- as.data.frame(t(row_summary))
      row_summary_df$Time <- times
      row_summary_df <- move_column(row_summary_df, "Time", 1)
      if (write_csv == TRUE){
        write.csv(row_summary_df, paste0(outpath,"summary_run_",population,"_",season,"_",day_type,"_dt_",dt, extra,".csv"), row.names=TRUE)
      }
      
      # Convert dataframe to long format
      row_summary_df$ID <- row.names(row_summary_df)
      
      #df_exposure2 <- df_exposure %>%
      #  mutate(Time = as.POSIXct(Time, format="%H:%M:%S"))  # Convert to actual time format
      
      # Apply pivot_longer while preserving Time format
      #summary_long <- df_exposure2 %>% select(-ID) %>%
      #  pivot_longer(cols = -Time, names_to = "Statistic", values_to = "Value") %>%
      #  mutate(Time = format(Time, "%H:%M:%S"))  # Reapply time format
      
      summary_long <- row_summary_df %>% select(-ID) %>%
        pivot_longer(cols = -Time, names_to = "Statistic", values_to = "Value")
      
      if (plot==TRUE){
        bxplot_title <- paste0("Probabilistic exposure (",population,"_",season,"_",day_type, ")",extra)
        bxplot <- plot_boxplots_probabilistic(summary_long, bxplot_title, nth=3, dt=dt) #Plot every 3 hours
        print(bxplot)
        ggsave(paste0(figpath, "boxplot_",population,"_",season,"_",day_type,"_dt_", dt, extra,".png"), bxplot, width = 8, height = 6, dpi = 600)
      }
    }
  }
  return(df_samples)
}

extract_unique_field <- function(data_list, field) {
  unique(vapply(data_list, function(x) {
    if (is.list(x) && field %in% names(x)) x[[field]] else NA_character_
  }, FUN.VALUE = character(1)))
}

pick_column_per_row <- function(df) {
  apply(df[,-1], 1, function(row) {
    selected_col <- sample(names(df)[-1], size = 1, prob = row)
    return(selected_col)
  })
}

pick_column_per_row_override <- function(df, person="random") {
  ## Similar function to "pick_column" but overrides specific times based on type of person
  ## need to test this as currently doesn't work.
  override_rows <- integer(0)  # Ensure this is a valid empty set
  override_column <- NULL      # Default to NULL unless changed
  
  if (person == "child") {
    override_rows <- select_idx(8.5, 15) ## Children at school from 08:30 - 15:00
    override_column <- "ME_3"
  } else if (person == "adult_office") {
    override_rows <- select_idx(9, 17.5)
    override_column <- "ME_3"
  }
  
  apply(df, 1, function(row, .row) {
    if (.row %in% override_rows) {  # Use .row instead of idx
      return(override_column)  # Assign fixed column for specified rows
    } else {
      selected_col <- sample(names(df)[-1], size = 1, prob = row[-1])  # Use probabilities for column selection
      return(selected_col)
    }
  }, .row = seq_len(nrow(df)))
}

select_idx <- function(hour_start, hour_end){
  idx_start <- (hour_start - 4)*6
  idx_end <- (hour_end - 4)*6
  override_rows <- seq(idx_start, idx_end)
  return(override_rows)
}

create_selected_df <- function(df, time_column, dfC) {
  selected_columns <- pick_column_per_row(df)
  # Create a new dataframe with selected column values
  selected_values <- mapply(function(row, col) dfC[row, col], row = seq_len(nrow(dfC)), col = selected_columns)
  new_df <- data.frame(Time = time_column, microenvironment = selected_columns, concentration = selected_values)
  # Create a new dataframe with just the selected column names and time column
  #new_df <- data.frame(Time = time_column, Selected_Column = selected_columns)
  return(new_df)
}

rollingmean <- function(df, n=48){
  #df$max_8hr_mean <- rollapply(df$value, width = 8, FUN = mean, align = "right", fill = NA)
  stats::filter(df, rep(1 / n, n), sides = 2) ## centre alignment
}

sample_lognormal_old <- function(means, sds, n_samples) {
  # Convert means and standard deviations to log-space parameters
  log_mu <- log(means^2 / sqrt(sds^2 + means^2))
  log_sigma <- sqrt(log(1 + (sds^2 / means^2)))
  
  # Generate samples for each mean/sd pair (rows) across multiple samples (columns)
  samples <- t(sapply(seq_along(means), function(i) {
    rlnorm(n_samples, meanlog = log_mu[i], sdlog = log_sigma[i])
  }))
  sampled_df <- as.data.frame(samples)
  return(sampled_df)  # Rows are distributions, columns are individual samples
}

sample_lognormal <- function(mu, sigma, n){
  ## Convert mu & sigma to log space
  meanlog <- log(mu^2 / sqrt(sigma^2 + mu^2))
  sdlog <- sqrt(log(1 + (sigma^2 / mu^2)))
  ## Sample n times
  samples <- rlnorm(n, meanlog, sdlog)
}

create_sampled_df <- function(microenvs, df_dp, times, means_df, sd_df, nc=100){
  ## Function that returns one dataframe of nrows (dependent on timesteps) and ncols=100 (population)
  ## where each element is sampled from the lognormal distribution of concentrations
  ## from each microenvironment, and number of samples from each menv at any one time
  ## corresponds to the proportion of population there.
  
  df <- data.frame(matrix(ncol=nc, nrow=length(times)))
  for (j in seq_along(times)){
    row_sample <- numeric()
    for (i in seq_along(microenvs)[-1]){
      microenv <- microenvs[i]
      p <- round(df_dp[[microenv]][j]*100)
      mu <- means_df[[microenv]][j]
      sigma <- sd_df[[microenv]][j]
      samples <- sample_lognormal(mu, sigma, p)
      row_sample <- c(row_sample, samples) ## should end up with ~100 elements at the end
    }
    # Ensure row_sample has exactly 100 elements
    if (length(row_sample) < nc) {
      row_sample <- c(row_sample, rep(NA, nc - length(row_sample)))
    }
    if (length(row_sample) > nc){
      row_sample <- head(row_sample, nc)
    }
    #print(length(row_sample))
    df[j, ] <- row_sample
  }
  return(as.data.frame(df))
}

interpolate_df <- function(df, target_length) {
  ## function to interpolate the activity profiles dataframe into length of timesteps
  ## the categorical time variable will be repeated in blocks to reach correct length
  repeat_factor <- ceiling(target_length / nrow(df))  # Determine how many times each category repeats
  interp_cols <- lapply(df, function(col) {
    if (is.numeric(col)) {
      approx(seq_len(nrow(df)), col, xout = seq(1, nrow(df), length.out = target_length))$y
    } else {
      rep(col, each = repeat_factor, length.out = target_length)  # Repeat each value in blocks
    }
  })
  return(as.data.frame(interp_cols))
}


condense_df <- function(df, target_length){
  # Condense dataframe by averaging groups
  df_condensed <- df %>%
    mutate(group = cut(seq_along(Time), breaks = target_length, labels = FALSE)) %>%
    group_by(group) %>%
    summarize(across(everything(), mean, na.rm = TRUE)) %>%
    select(-group)  # Remove temporary grouping column
  return(df_condensed)
}

move_column <- function(df, col_name, new_pos) {
  col_order <- setdiff(names(df), col_name)  # Exclude the column to move
  col_order <- append(col_order, col_name, after = new_pos - 1)  # Insert at new position
  return(df[, col_order])
}

sweep_files <- function(folder_path, keyword) {
  all_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  # Filter files that contain the keyword in their name
  matching_files <- all_files[grepl(keyword, basename(all_files), ignore.case = TRUE)]
  #data_list <- lapply(matching_files, read.csv)
  data_list <- lapply(matching_files, function(file) {
    df <- read.csv(file)
    df[, -1]  # Remove the first column
  })
  extract_name <- function(filename) {
    name <- basename(filename)
    pattern <- paste0("run_", "(.*?)", "_dt")
    match <- regmatches(name, regexpr(pattern, name))
    sub("run_", "", sub("_dt", "", match))
  }
  names(data_list) <- sapply(matching_files, extract_name)

  return(data_list)
}

############# PLOTTING FUNCTIONS ################
plot_exposure <- function(df, title, generic=FALSE){
  if (generic == FALSE){
    plt <- ggplot(df, aes(x = Time, y = concentration, color=microenvironment, shape=microenvironment)) +
      geom_point(size=2) +
      scale_color_manual(values = c("Eating/cooking" = "red", "Home_other" = "blue", "Not_home" = "green",
                                    "Personal_care" = "orange", "Sleeping"="black", "Watching_TV/lounging"="magenta")) +  # Customize colors
      scale_shape_manual(values = c("Eating/cooking" = 16, "Home_other" = 17, "Not_home" = 18, 
                                    "Personal_care" = 15, "Sleeping" = 3, "Watching_TV/lounging" = 7)) +  # Assigning different point styles
      
      scale_x_discrete(limits = df$Time, breaks = df$Time[seq(1, length(df$Time), by = 18)]) +
      guides(color = guide_legend("Microenvironment"), shape = guide_legend("Microenvironment")) +  # Merge color and shape into one legend
      labs(title = title, x = "Time", y = "Concentrations (ug/m3)") +
      theme_minimal()
  }
  else if (generic==TRUE){
    plt <- ggplot(df, aes(x = Time, y = concentration, color=microenvironment, shape=microenvironment)) +
      geom_point(size=2) +
      scale_color_manual(values = c("ME_1" = "red", "ME_2" = "blue", "ME_3" = "green",
                                    "ME_4" = "orange", "ME_5"="black", "ME_6"="magenta")) +  # Customize colors
      scale_shape_manual(values = c("ME_1" = 16, "ME_2" = 17, "ME_3" = 18, 
                                    "ME_4" = 15, "ME_5" = 3, "ME_6" = 7)) +  # Assigning different point styles
      
      scale_x_discrete(limits = df$Time, breaks = df$Time[seq(1, length(df$Time), by = 18)]) +
      guides(color = guide_legend("Microenvironment"), shape = guide_legend("Microenvironment")) +  # Merge color and shape into one legend
      labs(title = title, x = "Time", y = "Concentrations (ug/m3)") +
      theme_minimal()
  }
  return(plt)
}

plot_boxplots_probabilistic <- function(df, title, nth, dt){
  #df$Time <- factor(df$Time, levels = unique(df$Time))  # Ensure correct order
  #print(nrow(df))
  #print(str(df))
  nth_lab <- (nth * 60)/as.numeric(dt)
  
  plt <- ggplot(df, aes(x = factor(Time, levels=unique(Time)), y = Value, group = Time)) +
    geom_boxplot() +
    scale_x_discrete(
      breaks = unique(df$Time)[seq(1, length(unique(df$Time)), by = nth_lab)]  # Selects every 18th label
    ) +
    labs(title = title, x = "Time", y = "Concentration (ug/m3)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels
  return(plt)
}

plot_boxplots_probabilistic2 <- function(df, title, nth, dt){
  #df$Time <- factor(df$Time, levels = unique(df$Time))  # Ensure correct order
  #print(nrow(df))
  #print(str(df))
  nth_lab <- (nth * 60)/as.numeric(dt)
  
  plt <- ggplot(df, aes(x = factor(Time, levels=unique(Time)), y = Value, group = Time)) +
    geom_boxplot() +
    scale_x_discrete(
      breaks = unique(df$Time)[seq(1, length(unique(df$Time)), by = nth_lab)]  # Selects every 18th label
    ) +
    labs(title = title, x = "Time", y = "Concentration (ug/m3)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels
  return(plt)
}

plot_boxplots_popgroups <- function(data_list, figpath, stat_columns = c("Min", "Q1.25.", "Median", "Q3.75.", "Max", "Mean")) {
  # Prepare data: extract statistics from each dataset
  box_data <- lapply(seq_along(data_list), function(i) {
    df <- data_list[[i]][, stat_columns, drop = FALSE]
    # Convert each row of statistics into a vector of values
    stats <- as.numeric(unlist(df[1, stat_columns]))
    data.frame(Value = stats, Dataset = names(data_list)[i])
  })
  
  # Combine all into one data frame
  combined_data <- do.call(rbind, box_data)
  
  # Create boxplot: one per dataset
  boxplot(Value ~ Dataset, data = combined_data,
          main = "Population groups",
          xlab = "",
          ylab = "Concentration (ug/m3)",
          col = "lightblue",
          las = 2,
          ylim = c(0,40),
          outline = TRUE)
  #ggsave(paste0(figpath, "boxplot_allgroups.png"), boxplot_combined, width = 8, height = 6, dpi = 600)
}


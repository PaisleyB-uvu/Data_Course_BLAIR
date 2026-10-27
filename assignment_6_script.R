> dat <- read_csv("../../Data/BioLog_Plate_Data.csv")
Rows: 1152 Columns: 8                                    
── Column specification ─────────────────────────────────
Delimiter: ","
chr (3): Sample ID, Well, Substrate
dbl (5): Rep, Dilution, Hr_24, Hr_48, Hr_144

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> tidy_data <- dat %>%
  + pivot_longer(
    +     cols = starts_with("Hr_"),
    +     names_to = "Time",
    +     values_to = "Absorbance"
    + ) %>%
  + mutate(
    +     Time = as.numeric(str_remove(Time, "Hr_"))
    + ) %>%
  + mutate(
    +     Type = case_when(
      +         str_detect(`Sample ID`, "(?i)soil")  ~ "Soil",
      +         str_detect(`Sample ID`, "(?i)water") ~ "Water",
      +         TRUE                                 ~ "Unknown"
      +     )
    + )
> plot_data <- tidy_data %>%
  +     filter(Dilution == 0.1)
> ggplot(plot_data, aes(x = Time, y = Absorbance, color = Type, group = `Sample ID`)) +
  + geom_line(linewidth = 0.7, alpha = 0.8) + 
  + facet_wrap(~ Substrate, ncol = 6) + 
  + theme_minimal() +
  + labs(
    +     title = "Just dilution 0.1",
    +     x = "Time",
    +     y = "Absorbance",
    +     color = "Type"
    + ) +
  + theme(
    +     strip.text = element_text(size = 8),
    +     plot.title = element_text(size = 12, face = "plain"),
    +     legend.position = "right",
    +     panel.grid.minor = element_blank()
    + )
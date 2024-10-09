library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)

# Read data from the specified Excel file, sheet, and range without column headers
file_path <- "C:/Users/Peter Zorn/Desktop/religion_vs_education/Sonderauswertung_Religionszugehoerigkeit_Gemeinden.xlsx"
sheet_name <- "Religion"
data_range <- "A7:F10792"

# Use readxl to read the data and tidyverse to manipulate it
df <- read_excel(file_path, sheet = sheet_name, range = data_range, col_names = FALSE) %>%
    as_tibble() %>%
    rename(
        ARS = 1,
        name = 2,
        level = 3,
        catholic_share = 4,
        protestant_share = 5,
        other_share = 6
    ) %>%
    mutate(
        AGS = paste0(substr(ARS, 1, 5), substr(ARS, 10, 12)),
        highest_share = pmax(catholic_share, protestant_share, other_share),
        highest_religion = case_when(
            highest_share == catholic_share ~ "Catholic",
            highest_share == protestant_share ~ "Protestant",
            highest_share == other_share ~ "Other"
        )
    )

# Read data from the specified Excel file, sheet, and filter on column2 equal to "Land"
education_file_path <- "C:/Users/Peter Zorn/Desktop/religion_vs_education/Regionaltabelle_Bildung_Erwerbstaetigkeit.xlsx"
education_sheet_name <- "Höchster Schulabschluss"
education_range <- "A7:AA12445"

education_df <- read_excel(education_file_path, sheet = education_sheet_name, range = education_range, col_names = FALSE) %>%
    as_tibble() %>%
    rename(
        ARS = 1,
        name = 2,
        level = 3,
        total = 4,
        Hauptschule = 13,
        Realschule = 19,
        Abitur = 22
    ) %>%
    select(ARS, name, level, total, Hauptschule, Realschule, Abitur) %>%
    filter(level == "Gemeinde") %>%
    mutate(
        total = as.numeric(total),
        Hauptschule = as.numeric(Hauptschule),
        Realschule = as.numeric(Realschule),
        Abitur = as.numeric(Abitur)
    ) %>%
    mutate(
        Hauptschule_share = Hauptschule / total,
        Realschule_share = Realschule / total,
        Abitur_share = Abitur / total
    ) %>%
    rowwise() %>%
    mutate(
        highest_ed_share = max(c(Hauptschule_share, Realschule_share, Abitur_share)),
        highest_education = case_when(
            highest_ed_share == Hauptschule_share ~ "Hauptschule",
            highest_ed_share == Realschule_share ~ "Realschule",
            highest_ed_share == Abitur_share ~ "Abitur"
        )
    ) %>%
    ungroup()

# Merge the religion and education dataframes on ARS
merged_df <- df %>%
    inner_join(education_df, by = "ARS")

# Display the dataframe
print(merged_df)

# Load a shapefile of Germany's states
germany_shapefile <- st_read("C:/Users/Peter Zorn/Desktop/religion_vs_education/VG5000_GEM.shp", quiet = TRUE)

# Merge the shapefile with the dataframe
germany_map <- germany_shapefile %>%
    left_join(merged_df, by = c("AGS"))

# Plot the map with religion
ggplot(data = germany_map) +
    geom_sf(aes(fill = highest_religion)) +
    scale_fill_manual(values = c("Catholic" = "blue", "Protestant" = "red", "Other" = "green")) +
    labs(title = "Dominant Religion by Municipality in Germany", fill = "Religion") +
    theme_minimal()

# Export the religion map as a JPG file
ggsave("dominant_religion_map.jpg", plot = last_plot(), device = "jpg", path = "C:/Users/Peter Zorn/Desktop/religion_vs_education/")

# Plot the map with education
ggplot(data = germany_map) +
    geom_sf(aes(fill = highest_education)) +
    scale_fill_manual(values = c("Hauptschule" = "blue", "Realschule" = "red", "Abitur" = "green")) +
    labs(title = "Dominant Education by Municipality in Germany", fill = "Education") +
    theme_minimal()

# Export the education map as a JPG file
ggsave("dominant_education_map.jpg", plot = last_plot(), device = "jpg", path = "C:/Users/Peter Zorn/Desktop/religion_vs_education/")

# Calculate the correlation between the share of Protestants and the share of Abitur
correlation <- cor(merged_df$protestant_share, merged_df$Abitur_share, use = "complete.obs")

# Print the correlation result
print(paste("Correlation between share of Protestants and share of Abitur:", correlation))

# Calculate the correlation between the share of Catholics and the share of Abitur
catholic_correlation <- cor(merged_df$catholic_share, merged_df$Abitur_share, use = "complete.obs")

# Print the correlation result
print(paste("Correlation between share of Catholics and share of Abitur:", catholic_correlation))

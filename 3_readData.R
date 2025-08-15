# READING IN DATA ####
# Path to transcript data ----
path <- "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/data"
output <- "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/"

# Reading all transcripts into one data frame ----
pattern <- "*.csv"
file_list <- list.files(path, pattern, full.names=TRUE)
df <- readr::read_csv(file_list, id="file_name")

# DATA PREP ####
# Column names ----
names(df)

# Dropping columns ----
# Note: Dropped Ancestry3 since the column had no values.
df <- df[,!names(df) %in% c("Ancestry3", "Time", "file_name")]

# Formatting date as date variable ----
df$datefmt <- as.Date(df$Date, format="%m/%d/%y")

# Extracting year-month ----
df$monthyear <- format(as.Date(df$datefmt), "%Y-%m")

# FILTER FOR JAMAICA ####
jamaica_df <- dplyr::filter(df, JamaicaYN=='Y' & Person=='Interviewee')

jamaica_df <- jamaica_df %>%
  mutate(
    Time_Bin = case_when(
      JamaicaTime == "-99" ~ "Unknown",
      JamaicaTime %in% c("1930s", "1940s") ~ "1930s-1940s",
      JamaicaTime %in% c("1950s", "1950s-1960s", "1960s", "1968") ~ "1950s-1960s",
      JamaicaTime %in% c("1970s", "1970s-1980s", "1980s") ~ "1970s-1980s",
      JamaicaTime %in% c("1980s-1990s") ~ "1980s-1990s",
      JamaicaTime %in% c("1990s", "1992", "1990s-2000s", "2000s", "2009") ~ "1990s-2000s",
      JamaicaTime %in% c("2010s", "2018", "2020", "2021", "2023") ~ "2010s-2020s",
      JamaicaTime == "Pre-COVID" ~ "Pre-COVID",
      JamaicaTime == "Unknown" ~ "Unknown",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(Line_Number = row_number())

# WIDE TO LONG ####
# Subset and pivot character variables ----
char_long <- jamaica_df %>% 
  distinct(ID, Spatial1, Spatial2, Spatial3, Spatial4, Spatial5,Spatial6, Spatial7, Spatial8, Spatial9, Spatial10, Ancestry1, Ancestry2, Description, monthyear) %>%
  pivot_longer(cols=-ID,names_to="variable", values_to="values") %>%
  na.omit(char_long)

# Subset and pivot numeric variables ----
num_long <- jamaica_df %>%
  distinct(ID, TemporalMin, TemporalMax) %>%
  pivot_longer(cols=-ID, names_to="variable", values_to="values")

# Jamaica Time wide to long ----
jamaica_long <- select(jamaica_df, c('ID', 'JamaicaTime', 'Time_Bin'))

# TOTAL COUNTS (DENOMINATORS) ####
total_ids <- length(unique(char_long$ID))
total_excerpts <- length(jamaica_long$ID)

# LIST OF INTERVIEW IDS ####
interview_ids <- sort(unique(as.character(jamaica_df$ID)))
time_bins <- sort(unique(jamaica_df$Time_Bin))

# DESCRIPTIONS ####
captions_df <- jamaica_df %>%
  distinct(ID, Description) %>%
  mutate(ID = as.character(ID))


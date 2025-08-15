# SPATIAL DATA ####
# Query on spatial data ----
spatial <- char_long[grep("^Spatial",char_long$variable),]
# Spatial data frequencies ----
spatial_freq <- table(spatial$values) 
spatial_freq <- as.data.frame(spatial_freq)
names(spatial_freq) <- c("Location", "N")
# Spatial data percents ----
spatial_freq$Percent <- (spatial_freq$N / total_ids) 
spatial_freq <- spatial_freq %>%
  arrange(desc(Percent))
# Spatial data table output ----
spatial_output <- dplyr::filter(spatial_freq, N>1 | str_detect(Location, "Jamaica")) %>%
  gt() %>% 
  cols_align(
    align="center",
    columns=everything()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "white",
      style = "solid",
      weight = px(1.5)),
    locations = cells_body()) %>%
  tab_header(title="Spatial Metadata",
             subtitle="All locations referenced in interviews pertaining to Jamaica, Queens, NY.") %>%
  fmt_percent(Percent) %>%
  cols_merge_n_pct(col_n=N, col_pct=Percent) %>%
  tab_footnote(footnote=md("*Count and Percentage shows the proportion of interviews where the location was mentioned. Example: Manhattan, NY appears in 13% of all interviews.*")) %>%
  tab_footnote(footnote="The following locations were each mentioned once:") %>%
  tab_footnote(footnote=md("**Bronx locations:** Hunts Point")) %>%
  tab_footnote(footnote=md("**Brooklyn locations:** Bedford-Stuyvesant")) %>%
  tab_footnote(footnote=md("**Manhattan locations:** Chelsea, Yorkville")) %>%
  tab_footnote(footnote=md("**Queens locations:** Bayside, Douglaston, Flushing Meadows Corona Park, Forest Park, Fresh Meadows, Hillcrest, Hollis, Jackson Heights, Kew Gardens Hills, Ozone Park, Queens Village, Queensborough Community College, Richmond Hill, Ridgewood")) %>%
  tab_footnote(footnote=md("**Long Island locations:** Commack, Roslyn")) %>%
  tab_footnote(footnote=md("**Other locations in the U.S.:** Los Angeles, Rhode Island, Southern California, Southern Florida")) %>%
  tab_footnote(footnote=md("**Locations outside the U.S.:** Czechoslovakia, Guyana, Istanbul")) %>%
  #tab_style(style=cell_text(size="small"), locations=cells_body()) %>%
  tab_style(style=list(cell_fill(color="#fbc500"), cell_text(color="black")), locations=cells_body(rows=grepl("^Jamaica", Location))) %>%
  tab_options(table.width = pct(100), 
              heading.title.font.size = "medium",
              heading.subtitle.font.size = "small",
              heading.background.color = "#1a64c2",
              column_labels.font.size = 'small',
              column_labels.font.weight = 'bold',
              column_labels.background.color = "lightgray",
              table.font.size = 'small',
              footnotes.font.size = "small",
              data_row.padding = px(5),
              ihtml.use_compact_mode=TRUE)%>%
  opt_table_font(font=list(google_font(name="Google Sans Code"))) %>%
  opt_table_outline(style="none")
spatial_output
# Save spatial data output ----
gtsave(spatial_output, "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/spatial.html")

# ANCESTRY DATA ####
# Query on ancestry data ----
anc_data <- char_long[grep("^Ancestry",char_long$variable),]
# Ancestry data frequencies ----
anc_freq <- table(anc_data$values)
anc_freq <- as.data.frame(anc_freq)
names(anc_freq) <- c("Ancestry", "N")
# Ancestry data percents ----
anc_freq$Percent <- (anc_freq$N / total_ids) 
anc_freq <- anc_freq %>%
  arrange(desc(Percent))
# Ancestry data table output ----
ancestry_output <- anc_freq %>%
  gt() %>%
  cols_align(
    align="center",
    columns=everything()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "white",
      style = "solid",
      weight = px(1.5)),
    locations = cells_body()) %>%
  tab_header(title="Ancestries Mentioned by Interviewees") %>%
  fmt_percent(Percent) %>%
  cols_merge_n_pct(col_n=N, col_pct=Percent) %>%
  tab_footnote(footnote=md("*Count and Percentage shows the proportion of interviews in which the ancestry was mentioned. Example: German ancestry is mentioned in 13% of all interviews.*")) %>%
  tab_options(table.width = pct(100), 
              heading.title.font.size = "medium",
              heading.background.color = "#1a64c2",
              column_labels.font.size = 'small',
              column_labels.font.weight = 'bold',
              column_labels.background.color = "lightgray",
              table.font.size = 'small',
              footnotes.font.size = "small", 
              data_row.padding = px(5),
              ihtml.use_compact_mode=TRUE) %>%
  opt_table_font(font=list(google_font(name="Google Sans Code"))) %>%
  opt_table_outline(style="none")
ancestry_output
# Save ancestry data output ----
gtsave(ancestry_output, "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/ancestry.html")

# GENERAL TEMPORAL DATA ####
# Query on temporal min and temporal max data ----
tempmin_data <- num_long[grep("TemporalMin",num_long$variable),]
tempmax_data <- num_long[grep("TemporalMax",num_long$variable),]
# General temporal data frequencies ----
tempmin_freq <- table(tempmin_data$values)
tempmin_freq <- as.data.frame(tempmin_freq)
names(tempmin_freq) <- c("Temporal Start", "N")
tempmax_freq <- table(tempmax_data$values)
tempmax_freq <- as.data.frame(tempmax_freq)
names(tempmax_freq) <- c("Temporal End", "N")
# General temporal data percents ----
tempmin_freq$Percent <- (tempmin_freq$N / total_ids) 
tempmin_freq <- tempmin_freq %>%
  arrange(desc(Percent))
tempmax_freq$Percent <- (tempmax_freq$N / total_ids) 
tempmax_freq <- tempmax_freq %>%
  arrange(desc(Percent))
# General temporal data table output ----
# Temporal min ====
tempmin_output <- tempmin_freq %>%
  gt() %>%
  cols_align(
    align="center",
    columns=everything()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "white",
      style = "solid",
      weight = px(1.5)),
    locations = cells_body()) %>%
  tab_header(title="Earliest Recalled Year") %>%
  fmt_percent(Percent) %>%
  cols_merge_n_pct(col_n=N, col_pct=Percent) %>%
  tab_footnote(footnote=md("*Count and percentage represent the proportion of interviews with a specific start year. Example: 38% of the interviews include memories dating back to 1950.*")) %>%
  tab_options(table.width = pct(100), 
              heading.title.font.size = "medium",
              heading.background.color = "#1a64c2",
              column_labels.font.size = 'small',
              column_labels.font.weight = 'bold',
              column_labels.background.color = "lightgray",
              table.font.size = 'small',
              footnotes.font.size = "small",
              data_row.padding = px(5),
              ihtml.use_compact_mode=TRUE) %>% 
  opt_table_font(font=list(google_font(name="Google Sans Code"))) %>%
  opt_table_outline(style="none")
tempmin_output
# Temporal max ====
tempmax_output <- tempmax_freq %>%
  gt() %>%
  cols_align(
    align="center",
    columns=everything()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "white",
      style = "solid",
      weight = px(1.5)),
    locations = cells_body()) %>%
  tab_header(title="Latest Recalled Year") %>%
  fmt_percent(Percent) %>%
  cols_merge_n_pct(col_n=N, col_pct=Percent) %>%
  tab_footnote(footnote=md("*Count and percentage represent the proportion of interviews with a specific start year. Example: 32% of the interviews include memories extending up to 2021.*")) %>%
  tab_options(table.width = pct(100), 
              heading.title.font.size = "medium",
              heading.background.color = "#1a64c2",
              column_labels.font.size = 'small',
              column_labels.font.weight = 'bold',
              column_labels.background.color = "lightgray",
              table.font.size = 'small',
              footnotes.font.size = "small",
              data_row.padding = px(5),
              ihtml.use_compact_mode=TRUE) %>% 
  opt_table_font(font=list(google_font(name="Google Sans Code"))) %>%
  opt_table_outline(style="none")
tempmax_output
# Save general temporal data output ----
gtsave(tempmin_output, "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/tempmin.html")
gtsave(tempmax_output, "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/tempmax.html")

# JAMAICA TEMPORAL DATA - V1 (EXCERPTS) ####
# Jamaica time frequencies ----
jamaicatime_freq <- table(jamaica_long$Time_Bin)
jamaicatime_freq <- as.data.frame(jamaicatime_freq)
names(jamaicatime_freq) <- c("Time Period", "N")
# Jamaica time calculate percents ----
jamaicatime_freq$Percent <- (jamaicatime_freq$N / total_excerpts)
jamaicatime_freq <- jamaicatime_freq %>%
  arrange(desc(Percent))
# Jamaica time data table output ----
jamaicatime_output <- jamaicatime_freq %>%
  gt() %>%
  cols_align(
    align="center",
    columns=everything()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "white",
      style = "solid",
      weight = px(1.5)),
    locations = cells_body()) %>%
  tab_header(title="Temporal Contexts of Interviewee Dialogue Segments Set in Jamaica, Queens") %>%
  fmt_percent(Percent) %>%
  cols_merge_n_pct(col_n=N, col_pct=Percent) %>%
  tab_footnote(footnote=md("*Count and percentage represent the proportion of Jamaica dialogue segments from interviewees occurring within a specific time period. For example, 47% of the segments reflect memories from the 1950s–1960s. The most recent memory dates back to 2023.*")) %>%
  tab_options(table.width = pct(100), 
              heading.title.font.size = "medium",
              heading.background.color = "#1a64c2",
              column_labels.font.size = 'small',
              column_labels.font.weight = 'bold',
              column_labels.background.color = "lightgray",
              table.font.size = 'small',
              footnotes.font.size = "small",
              data_row.padding = px(5)) %>% 
  opt_table_font(font=list(google_font(name="Google Sans Code"))) %>%
  opt_table_outline(style="none")
jamaicatime_output
# Save Jamaica temporal data output v1 ----
gtsave(jamaicatime_output, "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/jamaicatempv1.html")

# JAMAICA TEMPORAL DATA - V2 (INTERVIEWS) ####
# Jamaica time frequencies ----
jamaicatemp_freq <- jamaica_df %>%
  distinct(ID, Time_Bin) %>%  # ensures each id is only counted once per time
  count(Time_Bin, sort = TRUE)
names(jamaicatemp_freq) <- c("Time Period", "N")
# Jamaica time calculate percents ----
jamaicatemp_freq$Percent <- (jamaicatemp_freq$N / total_ids) 
# Jamaica time data table output ----
jamaicatemp_output <- jamaicatemp_freq %>%
  gt() %>%
  cols_align(
    align="center",
    columns=everything()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "white",
      style = "solid",
      weight = px(1.5)),
    locations = cells_body()) %>%
  tab_header(title="Temporal Distribution of Interviews Featuring Memories Set in Jamaica, Queens") %>%
  fmt_percent(Percent) %>%
  cols_merge_n_pct(col_n=N, col_pct=Percent) %>%
  tab_footnote(footnote=md("*Count and percentage represent the proportion of interviews conducted with references to Jamaica, Queens, during each time period. For example, 69% of the interviews included memories from the 2010s–2020s.*")) %>%
  tab_options(table.width = pct(100), 
              heading.title.font.size = "medium",
              heading.background.color = "#1a64c2",
              column_labels.font.size = 'small',
              column_labels.font.weight = 'bold',
              column_labels.background.color = "lightgray",
              table.font.size = 'small',
              footnotes.font.size = "small",
              data_row.padding = px(5)) %>% 
  opt_table_font(font=list(google_font(name="Google Sans Code"))) %>%
  opt_table_outline(style="none")
jamaicatemp_output
# Save Jamaica temporal data output v2 ----
gtsave(jamaicatemp_output, "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/jamaicatempv2.html")

# INTERVIEW DATE DATA ####
# Query on interview date data ----
conduct_data <- char_long[grep("monthyear",char_long$variable),]
# Interview date data frequencies ----
conduct_freq <- table(conduct_data$values)
conduct_freq <- as.data.frame(conduct_freq)
names(conduct_freq) <- c("Interview Conducted", "N")
# Interview date calculate percents ----
conduct_freq$Percent <- (conduct_freq$N / total_ids) 
conduct_freq <- conduct_freq %>%
  arrange(desc(Percent))
# Interview date data table output ----
conduct_output <- conduct_freq %>%
  gt() %>%
  cols_align(
    align="center",
    columns=everything()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "white",
      style = "solid",
      weight = px(1.5)),
    locations = cells_body()) %>%
  tab_header(title="Date the Interview was Conducted") %>%
  fmt_percent(Percent) %>%
  cols_merge_n_pct(col_n=N, col_pct=Percent) %>%
  tab_footnote(footnote=md("*Count and percentage represent the proportion of interviews conducted during specific time periods. For example, 25% of the interviews were conducted in December 2020.*")) %>%
  tab_options(table.width = pct(100), 
              heading.title.font.size = "medium",
              heading.background.color = "#1a64c2",
              column_labels.background.color = "lightgray",
              column_labels.font.size = 'small',
              column_labels.font.weight = 'bold',
              table.font.size = 'small',
              footnotes.font.size = "small", 
              data_row.padding = px(5)) %>%
  opt_table_font(font=list(google_font(name="Google Sans Code"))) %>%
  opt_table_outline(style="none")
conduct_output
# Save interview date data output ----
gtsave(conduct_output, "/Users/randipparhar/Documents/RANDIP/capstone/analysis_website/website_project/output/conduct.html")

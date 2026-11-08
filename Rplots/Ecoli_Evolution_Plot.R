# library
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(patchwork)
# Add the document with the Agar and the MIC
#Ecoli_agar (sheet =1) Ecoli MICs (sheet = 2)
Plates<-read_xlsx("C:/Users/ngonzalez/OneDrive - Massey University/Thesis/Chapter 3 - Evolutionary Study/Ecoli_Passes.xlsx", sheet =1)
MICs<-read_xlsx("C:/Users/ngonzalez/OneDrive - Massey University/Thesis/Chapter 3 - Evolutionary Study/Ecoli_Passes.xlsx", sheet =2)

#Replace "-" with NA and convert everything to numeric except Day
MICs_clean <- MICs %>%
  mutate(across(-Day, as.character)) %>%             # make all isolates character, numbers, - and NA, but this is momentary
  mutate(across(-Day, ~na_if(.x, "-"))) %>%          # replace "-" with NA (to do this we needed everything to be a character)
  mutate(across(-Day, ~na_if(.x, "NA"))) %>%         # replace "NA" string with NA (to do this we needed everything to be a character)
  mutate(across(-Day, as.numeric))                   # convert everything to numeric (The numbers will go back to numeric but not the NAs)

# Reshape to long format
MICs_long <- MICs_clean %>%
  pivot_longer(
    cols = starts_with("Isolate"),
    names_to = "Isolate",
    values_to = "Value"
  )

# Drop rows with missing Value
MICs_long <- MICs_long %>%
  filter(!is.na(Value), !is.na(Isolate))

# basic scatterplot
p1 <- ggplot(MICs_long, aes(x = Day, y = Value, color = Isolate)) + 
  geom_point(na.rm = TRUE, size = 5, alpha = 0.5, position = position_jitter(width = 0.2, height = 0)) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Isolate 1" = "orange",
    "Isolate 2" = "darkred",
    "Isolate 3" = "blue",
    "Isolate 4" = "darkgreen",
    "Isolate 5" = "pink",
    "Isolate 6" = "lightblue"
  )) +
  labs(
    title = "A) MIC evolution of isolates per growth point",
    x = "Days",
    y = "MIC (µg/ml)"
  )

#Now we are going to take care of the time line: 
#For that we need to add intensity to each of the xMIC values. We'll select the numeric variable (string library)
Plates<- Plates %>%
  mutate(MIC_mult = as.numeric(str_extract(Concentration, "\\d+")))  # gets 2, 4, 8, 16...
 
#Now we need to associate the colours that we chose before for the scatterplot to the timeline
base_colors <- c(
  "Isolate 1" = "darkorange",
  "Isolate 2" = "darkred",
  "Isolate 3" = "blue",
  "Isolate 4" = "darkgreen",
  "Isolate 5" = "pink",
  "Isolate 6" = "lightblue"
)

#
Plates <- Plates %>%
  mutate(FillKey = paste(Isolate, MIC_mult, sep = "_"))

#Make colours
fill_colors <- unlist(lapply(names(base_colors), function(iso) {
  mults <- sort(unique(Plates$MIC_mult[Plates$Isolate == iso]))
  cols <- scales::col_numeric(
    palette = c("white", base_colors[iso]),
    domain = range(mults)
  )(mults)
  names(cols) <- paste(iso, mults, sep = "_")
  cols
}))

# Lets join the axis for both plots
min_day <- min(MICs_long$Day, Plates$Start, na.rm = TRUE)
max_day <- max(MICs_long$Day, Plates$End, na.rm = TRUE)

# Make a Gantt-style plot
p2 <- ggplot(Plates, aes(y = Isolate)) +
  geom_rect(aes(xmin = Start, xmax = End,
                ymin = as.numeric(factor(Isolate)) - 0.4,
                ymax = as.numeric(factor(Isolate)) + 0.4,
                fill = FillKey),
            color = "black") +
  geom_text(aes(x = (Start + End)/2, y = Isolate, label = Concentration),
            color = "black", size = 3) +
  scale_fill_manual(values = fill_colors, guide = "none") +
  theme_minimal() +
  labs(
    title = "B) Passes of isolates on agar",
    x = "Days",
    y = "Isolates"
  )
#Plot them together (patchwork)

# Update scatter plot to use the same x-axis
p1 <- p1 + scale_x_continuous(limits = c(min_day, max_day),
                              breaks = seq(min_day, max_day, by = 1))  # show every day)

# Update timeline plot to use the same x-axis (optional: already done)
p2 <- p2 + scale_x_continuous(limits = c(min_day, max_day),
                              breaks = seq(min_day, max_day, by = 1))  # show every day)

p1/p2


##NOW LETS HAVE AN INDIVIDUAL FIGURES


# Get the list of isolates (from MICs_long, since that's your master list of names)
isolate_list <- unique(MICs_long$Isolate)

# Store all combined plots in a list
isolate_plots <- list()

y_min <- 0
y_max <- 1

for (iso in isolate_list) {
  
  # Filter each dataset to just this isolate
  mic_sub <- MICs_long %>% filter(Isolate == iso)
  plates_sub <- Plates %>% filter(Isolate == iso)
  
  # --- Scatter plot for this isolate ---
  p1_iso <- ggplot(mic_sub, aes(x = Day, y = Value, color = Isolate)) +
    geom_point(na.rm = TRUE, size = 5, alpha = 0.5) +
    theme_minimal()+
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 12)) +
    scale_color_manual(values = base_colors, guide = "none") +
    scale_x_continuous(limits = c(min_day, max_day),
                       breaks = seq(min_day, max_day, by = 1)) +
    scale_y_continuous(limits = c(y_min, y_max),
                       breaks = seq(y_min, y_max, by = 0.25)) +  # adjust interval to taste
    labs(
      title = paste0("A) MIC evolution - ", iso),
      x = "Days",
      y = "MIC (µg/ml)"
    )
  
  # --- Gantt-style plot for this isolate ---
  box_height <- 0.3  # half-height; try 0.2 for thinner, 0.45 for near-touching rows
  
  p2_iso <- ggplot(plates_sub, aes(y = Isolate)) +
    geom_rect(aes(xmin = Start, xmax = End,
                  ymin = as.numeric(factor(Isolate)) - box_height,
                  ymax = as.numeric(factor(Isolate)) + box_height,
                  fill = FillKey),
              color = "black") +
    geom_text(aes(x = (Start + End)/2, y = Isolate, label = Concentration),
              color = "black", size = 3) +
    scale_fill_manual(values = fill_colors, guide = "none") +
    theme_minimal() +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 12)) +
    scale_x_continuous(limits = c(min_day, max_day),
                       breaks = seq(min_day, max_day, by = 1)) +
   labs(
    #title = paste0("B) Passes - ", iso),
      x = "Days",
    y = ""
    )
  
  # Combine scatter + gantt for this isolate +making B smaller
  isolate_plots[[iso]] <- p1_iso / p2_iso + plot_layout(heights = c(3, 0.5))
}

# View one isolate's combined plot, e.g. Isolate 1
isolate_plots[["Isolate 1"]]

# Or save each isolate as its own file
for (iso in names(isolate_plots)) {
  ggsave(
    filename = paste0("MIC_timeline_", gsub(" ", "_", iso), ".png"),
    plot = isolate_plots[[iso]],
    width = 8.3, height = 3.9, dpi = 300
  )
}

##save them in pdf

for (iso in names(isolate_plots)) {
  ggsave(
    filename = paste0("MIC_timeline_", gsub(" ", "_", iso), ".pdf"),
    plot = isolate_plots[[iso]],
    width = 8.3, height = 3.9, device = "pdf"
  )
}

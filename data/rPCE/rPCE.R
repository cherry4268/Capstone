rPCE <- read.csv("Capstone/data/rPCE/rPCE.csv")

rPCE <- rPCE %>%
  pivot_longer(-State, names_to = "Year", values_to = "Price") %>%
  mutate(Year = str_extract(Date, "\\d{4}"))

rPCE$Date <- NULL

rPCE$Year <- as.numeric(rPCE$Year)
rPCE$Price <- as.numeric(rPCE$Price)

# define regions 
northeast <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", 
               "Connecticut", "New York", "New Jersey", "Pennsylvania")
midwest <- c("Ohio", "Indiana", "Michigan", "Illinois", "Wisconsin", 
             "Minnesota", "Iowa", "Missouri", "North Dakota", "South Dakota", 
             "Nebraska", "Kansas")
south <- c("Delaware", "Maryland", "Virginia", "West Virginia", "North Carolina",
           "South Carolina", "Georgia", "Florida", "Kentucky", "Tennessee", 
           "Alabama", "Mississippi", "Arkansas", "Louisiana", "Oklahoma", "Texas")
west <- c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", 
          "Utah", "Nevada", "Washington", "Oregon", "California", "Alaska", "Hawaii")

# add region to data
rPCE <- rPCE %>%
  mutate(Region = case_when(
    State %in% northeast ~ "Northeast",
    State %in% midwest ~ "Midwest",
    State %in% south ~ "South",
    State %in% west ~ "West",
    TRUE ~ "Other"
  ))

write.csv(rPCE, "rPCE.csv", row.names=FALSE)

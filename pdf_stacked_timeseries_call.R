# call pdf plots

library(targets)
library(tidyverse)

tar_load(transects)
tar_load(attributes_pfix)
tar_load(rs_pfix)
tar_load(dtw_pfix)

# str(transects$Year)
# str(dtw_pfix$Year)
# str(rs_pfix$Year)
glimpse(rs_pfix)
#
# str(transects$Cover)
# str(transects$Shrub)
# str(transects$Grass)
#
# str(rs_pfix$NDVI_SUR)
# str(rs_pfix$PPT)
# str(dtw_pfix$DTW)



rs_pfix <- rs_pfix %>% mutate(Year = as.numeric(Year))
transects <- transects %>% mutate(Year = as.numeric(Year))
dtw_pfix <- dtw_pfix %>% mutate(Year = as.numeric(Year))

rs_pfix <- rs_pfix %>% mutate(PPT = PPT * 0.0393701)  # Convert mm to inches

# options(error = recover)

source("code/R/gg_timeseries_plots6b.R")
# Define the necessary variables
AttributesPID <- attributes_pfix %>% filter(reinv == "r", Parcel != "BIS082")
PIDs <- unique(AttributesPID$Parcel) # List of unique parcel IDs
current_year <- 2025
pdf_file <- "Parcel_TimeSeries_Customized06b_2025.pdf"  # Output PDF file name

# Call the function to create the PDF
create_ts_plots_pdf_gg_discrete(
  PIDs = PIDs,
  pdf_file = pdf_file,
  attributes = attributes_pfix,  # Replace with your attributes dataframe
  transects = transects,         # Replace with your transects dataframe
  dtw_data = dtw_pfix,           # Replace with your depth-to-water dataframe
  rs_data = rs_pfix,             # Replace with your remote sensing dataframe
  current_year = current_year
)

# create_ts_plots_pdf_gg_discrete(
#   PIDs       = "BLK099",  # or a single known PID
#   pdf_file   = "Test_Single.pdf",
#   attributes = attributes_pfix,
#   transects  = transects,
#   dtw_data   = dtw_pfix,
#   rs_data    = rs_pfix,
#   current_year = 2024
# )

#
#
# glimpse(dtw_pfix)
# glimpse(rs_pfix)
# glimpse(transects)
# glimpse(attributes_pfix)

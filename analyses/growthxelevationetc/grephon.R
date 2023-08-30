##======================================================================================================
##  Grephon
##  plotting data from papers
## Datum:           24.08.2023
## Autor:           Britany
#======================================================================================================

# Housekeeping ------------------------------------------------------------

rm(list=ls(all=TRUE))

# Set the path to PheonoFlex
directory_path <- "~/Documents/ubc/TemporalEcologyLab/"

# Set Working Directory
setwd(directory_path)

# Load packages
library(readxl)
library(ggplot2)

# Load data ---------------------------------------------------------------
meta <- read_excel("grephon/data/classicalrefs.xlsx", sheet = "datascraped")
meta$growth_value[which(meta$growth_units=="cm/yr-1")] <- meta$growth_value[which(meta$growth_units=="cm/yr-1")]*10
meta$growth_errorvalue <- as.numeric(meta$growth_errorvalue)
meta$growth_errorvalue[which(is.na(meta$growth_errorvalue)==TRUE)] <- 0
lat <- subset(meta, predictor_type == "latitude")
elev <- subset(meta, predictor_type == "elevation")

# Plotting ----------------------------------------------------------------
plot <- ggplot(data = meta, aes(x = predictor_value, y = growth_value, 
                                ymin = growth_value - growth_errorvalue, 
                                ymax = growth_value + growth_errorvalue,
                                colour = dataset_id)) +
  geom_point() + 
  geom_errorbar(width = 0.2) +
  geom_smooth(method="lm") +
  facet_wrap(vars(predictor_type), scales = "free")
print(plot)
ggsave("grephonR/res/grephon_plot.pdf", plot, dpi = 500, width = 15, height = 9)


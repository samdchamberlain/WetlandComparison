#' Script to process soils data across sites
#'
#' Sites: Peat 20 yr old, Alluvium 4 yr old. More to follow
#'
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom dplyr "%>%"

#load data.frames for pH and iron
pH_data <- read_csv("data/pH.csv")
Fe_data <- read_csv("data/Fe_data.csv")

#clean data tables to remove empty columns and rows
pH_data <- pH_data[,colSums(is.na(pH_data))<nrow(pH_data)] #columns
pH_data <- na.omit(pH_data) #rows

Fe_data <- Fe_data[,colSums(is.na(Fe_data))<nrow(Fe_data)] #columns
Fe_data <- na.omit(Fe_data) #rows

#merge data sources to single dataframe, clean up remaining dataframe
soils_df <- merge(pH_data, Fe_data, by="Number") %>%
  select(Number, `Depth (cm).x`, Tract.x, Point.x, Area.x, pH, `Fe(II)_mg.g`,
         Fe_all_mg.g, `Fe(III)_mg.g`) %>%
  rename(Depth = `Depth (cm).x`, FeII_mg.g = `Fe(II)_mg.g`, FeIII_mg.g = `Fe(III)_mg.g`)

#create site variable based on 'Area' site names
soils_df$site <- ifelse(soils_df$Area.x == "East End", "Alluvium - 3 yr old",
                        "Peat - 19 yr old")

#What percent of iron is in oxidized form?
soils_df$percent_ox <- (soils_df$FeIII_mg.g/soils_df$Fe_all_mg.g)*100
soils_df$percent_ox <- ifelse(soils_df$FeIII_mg.g < 0, 0, soils_df$percent_ox) #if FeIII is less than zero set to zero

#New surface v. deep column represents more accurately that we are comparing a recent muck layer
# to a deep older soil layer. This deep layer at the peat site is actually ~0.7m below surface
soils_df$horizon <- ifelse(soils_df$Depth == "0-15", "surface", "deep")

#Also, need to fix West Pond point 1 where 15-30 was also 'surface' litter
soils_df$horizon <- ifelse(soils_df$Area.x == "West Pond" & soils_df$Point.x == "P1",
                           "surface", soils_df$horizon)

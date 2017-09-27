#' Script to process soils data across sites
#' Sites: Peat 20 yr old, Alluvium 4 yr old. More to follow
#'
#' @import dplyr
#' @importFrom dplyr "%>%"

#load soils data
soils_df <- read.csv("data/soils.csv")

#create site variable based on 'Area' site names
soils_df$site <- ifelse(soils_df$Area == "East End", "Peat-Alluvium",
                        ifelse(soils_df$Area == "West Pond", "Old Peat",
                               "Young Peat"))

#What percent of iron is in oxidized form?
soils_df$percent_ox <- (soils_df$FeIII_mg.g/soils_df$FeT_mg.g)*100
soils_df$percent_ox <- ifelse(soils_df$FeT_mg.g <= 1, NA, soils_df$percent_ox) #if no measurable Fe set to zero

#New surface v. deep column represents more accurately that we are comparing a recent muck layer
# to a deep older soil layer. This deep layer at the peat site is actually ~0.7m below surface
soils_df$horizon <- ifelse(soils_df$Depth == "0-15", "Accreted", "Parent")

#Also, need to fix West Pond point 1 where 15-30 was also 'surface' litter
soils_df$horizon <- ifelse(soils_df$Area == "West Pond" & soils_df$Point == "P1",
                           "Accreted", soils_df$horizon)

#re-order factors to group peat sites in legend
soils_df$site <- factor(soils_df$site, levels=c("Peat-Alluvium", "Old Peat", "Young Peat"))

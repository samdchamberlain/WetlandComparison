#' Script to process soils data across sites
#' Sites: Peat 20 yr old, Alluvium 4 yr old. More to follow
#'
#' @import dplyr
#' @importFrom dplyr "%>%"

#load soils data
load("data/soils.Rdata")

#create site variable based on 'Area' site names
soils$site <- ifelse(soils$Area == "East End", "Peat-Alluvium",
                        ifelse(soils$Area == "West Pond", "Old Peat",
                               "Young Peat"))

#What percent of iron is in oxidized form?
soils$percent_ox <- (soils$FeIII_mg.g/soils$FeT_mg.g)*100
soils$percent_ox <- ifelse(soils$FeT_mg.g <= 1, NA, soils$percent_ox) #if no measurable Fe set to zero

#New surface v. deep column represents more accurately that we are comparing a recent muck layer
# to a deep older soil layer. This deep layer at the peat site is actually ~0.7m below surface
soils$horizon <- ifelse(soils$Depth == "0-15", "Accreted", "Parent")

#Also, need to fix West Pond point 1 where 15-30 was also 'surface' litter
soils$horizon <- ifelse(soils$Area == "West Pond" & soils$Point == "P1",
                           "Accreted", soils$horizon)

#re-order factors to group peat sites in legend
soils$site <- factor(soils$site, levels=c("Peat-Alluvium", "Old Peat", "Young Peat"))

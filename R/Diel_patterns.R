#' Script to calculate diel flux patterns across three core wetland sites
#'
#' Sites: Peat 7 yr old, Peat 20 yr old, Alluvium 4 yr old
#'
#' @import dplyr
#' @importFrom dplyr "%>%"
#' @importFrom lubridate month

## Process each core site into diel values then we will merge them into a single file for visualization
# diel fluxes from 6yr old peat site
diel_peat6 <- peat6_all %>%
  subset(DOY < 301 & DOY > 99 & year > 2010) %>% #use growing season only with full data years
  group_by(year, time, site) %>%
  summarise(CH4 = mean(wm, na.rm=T),
            sdCH4 = sd(wm, na.rm=T),
            GPP = mean(wc, na.rm=T),
            sdGPP = sd(wc, na.rm=T),
            ET = mean(wq, na.rm=T),
            sdET = sd(wq, na.rm=T),
            Ta = mean(TA.y, na.rm=T),
            sdTa = sd(TA.y, na.rm=T),
            Tw = mean(TW_TULE_10cm, na.rm=T),
            sdTw = sd(TW_TULE_10cm, na.rm=T),
            PAR = mean(PAR, na.rm=T),
            sdPAR = sd(PAR, na.rm=T))

#diel fluxes for 19yr old peat site
diel_peat19 <- peat19_all %>%
  subset(DOY.x < 301 & DOY.x > 99) %>% #use growing season only
  group_by(year, time, site) %>%
  summarise(CH4 = mean(wm, na.rm=T),
            sdCH4 = sd(wm, na.rm=T),
            GPP = mean(wc, na.rm=T),
            sdGPP = sd(wc, na.rm=T),
            ET = mean(wq, na.rm=T),
            sdET = sd(wq, na.rm=T),
            Ta = mean(TA.y, na.rm=T),
            sdTa = sd(TA.y, na.rm=T),
            Tw = mean(TW_8cm, na.rm=T),
            sdTw = sd(TW_8cm, na.rm=T),
            PAR = mean(PAR, na.rm=T),
            sdPAR = sd(PAR, na.rm=T))

#diel fluxes for 4yr old alluvium site
diel_alluvium <- alluvium_all %>%
  subset(DOY.x < 301 & DOY.x > 99) %>% #use growing season only
  group_by(year, time, site) %>%
  summarise(CH4 = mean(wm, na.rm=T),
            sdCH4 = sd(wm, na.rm=T),
            GPP = mean(wc, na.rm=T),
            sdGPP = sd(wc, na.rm=T),
            ET = mean(wq, na.rm=T),
            sdET = sd(wq, na.rm=T),
            Ta = mean(TA.y, na.rm=T),
            sdTa = sd(TA.y, na.rm=T),
            Tw = mean(TW_10cm, na.rm=T),
            sdTw = sd(TW_10cm, na.rm=T),
            PAR = mean(PAR, na.rm=T),
            sdPAR = sd(PAR, na.rm=T))

#merge core site files
diel_all <- rbind(diel_peat6, diel_peat19); diel_all <- rbind(diel_all, diel_alluvium);

rm(diel_peat6); rm(diel_peat19); rm(diel_alluvium) #remove redundant site files

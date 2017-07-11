#' Script to load, merge, and process daily to annual budgets.
#' Code varies for each site as input met variables vary across sites
#' Site: Alluvium
#'
#' @import dplyr
#' @importFrom dplyr "%>%"
#' @importFrom lubridate month
#' @importFrom zoo na.approx

# load datasets
load("data/alluvium_eddy.Rda") #eddy fluxes
load("data/alluvium_met.Rda") #met variables

# merge met and eddy dataframes by shared time variable
alluvium_all <- merge(alluvium_eddy, alluvium_met, by="decday")
alluvium_all$datetime <- as.POSIXct((alluvium_all$Mdate  - 719529)*86400, origin = "1970-01-01", tz = "UTC")
alluvium_all$month <- month(alluvium_all$datetime)
alluvium_all$yr.m <- paste(alluvium_all$year, alluvium_all$month, sep="_")
alluvium_all$dday <- floor(alluvium_all$decday)

# create a long-term water temperature measure, as the probes have changed over time
# when we don't have a 10cm water temp measurement, add 8cm depth in place
alluvium_all$TW_10cm <- ifelse(is.na(alluvium_all$TW_10cm), alluvium_all$TW_8cm, alluvium_all$TW_10cm)
alluvium_all$TW_10cm <- na.approx(alluvium_all$TW_10cm, na.rm=F) # linear interpolate holes

#Simple average of daily fluxes for pre-gapfilled values
daily <- alluvium_all %>%
  group_by(dday) %>%
  summarise(mNEE = mean(wc_gf),         #NEE (umol m-2 s-1)
            c_obs = sum(!is.na(wc)),    #num of non-filled observations (applies all _obs)
            mCH4 = mean(wm_gf),         #CH4 flux (nmol m-2 s-1)
            m_obs = sum(!is.na(wm)),
            GPP = mean(gpp_ANNnight),   #partitioned GPP (umol m-2 s-1)
            ER = mean(er_ANNnight),     #partitioned respiration (umol m-2 s-1)
            ET = mean(wq_gf),           #evapotranspiration (umol m-2 s-1)
            q_obs = sum(!is.na(wq)),
            H = mean(H_gf),                    #sensible heat flux (W m-2)
            mGCC = mean(GCC, na.rm=T),         #green index
            PAR = mean(PAR, na.rm=T),          #photosynthetic active radiation (umol m-2 s-1)
            Tair = mean(TA.y, na.rm=T),        #air temp (deg C)
            Tw = mean(TW_10cm, na.rm=T),       #water temp (deg C)
            Ts = mean(TS_8cm, na.rm=T),        #soil temp (deg C)
            mVPD = mean(VPD, na.rm=T),         #VPD (kPa)
            PA = mean(PA.y, na.rm=T),          #atm pressure (kPa)
            u. = mean(ustar, na.rm=T),         #friction velocity (m/s)
            Rain = sum(PRECIP, na.rm=T),       #rain (mm)
            WTD = mean(WT, na.rm=T),           #water table (m from surface)
            Cond = mean(Cond, na.rm=T),        #conductivity (mS)
            month = round(median(month)),
            year = round(median(year))) %>%
  filter(year < 2017) #only pre-2017 data

#Time and unit conversions
daily$datetime <- as.POSIXct(daily$dday*86400, origin="2013-01-01")
daily$mgCH4 <- (daily$mCH4*12.01*3600*24)/1000000 #mg C m-2 d-1
daily$gCO2 <- (daily$mNEE*12.01*3600*24)/1000000  #g C m-2 d-1
daily$gER <- (daily$ER*12.01*3600*24)/1000000     #g C m-2 d-1
daily$gGEP <- (daily$GPP*12.01*3600*24)/1000000   #g C m-2 d-1

#Annual fluxes from daily fluxes
yearly <- daily %>%
  group_by(year) %>%
  summarise(tCH4 = mean(mgCH4)*365/1000, #gC m-2 yr-1 (same for all fluxes below)
            tNEE = mean(gCO2)*365,
            tER = mean(gER)*365,
            tGEP = mean(gGEP)*365,
            Tair = mean(Tair, na.rm=T),
            GCC = mean(mGCC, na.rm=T),
            Days = sum(!is.na(mgCH4))) %>%
  filter(year < 2017 & Days >= 365) #only keep annual budgets for full years

#Ecosystem C balance and GHG (CO2eq) balance
yearly$Cbalance <- yearly$tNEE + yearly$tCH4 #C m-2 yr-1
yearly$GHGbalance <- (yearly$tNEE*44/12) + (25*yearly$tCH4*16/12) #CO2-eq m-2 yr-1

#resave with specific names, and add site name variable
alluvium_yearly <- yearly
alluvium_daily <- daily
alluvium_yearly$site <- "Alluvium - 3 yr old"
alluvium_daily$site <- "Alluvium - 3 yr old"
alluvium_all$site <- "Alluvium - 3 yr old"

#remove other dataframes
rm(yearly); rm(daily); rm(alluvium_eddy); rm(alluvium_met)






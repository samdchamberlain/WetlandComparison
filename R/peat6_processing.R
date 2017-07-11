#' Script to load, merge, and process daily to annual budgets.
#' Code varies for each site as input met variables vary across sites
#' Site: Peat 6 yr old
#'
#' @import dplyr
#' @importFrom dplyr "%>%"
#' @importFrom lubridate month


# load datasets
load("data/peat6_eddy.Rda") #eddy fluxes
load("data/peat6_met.Rda") #met variables

# merge met and eddy dataframes by shared time variable
peat6_all <- merge(peat6_eddy, peat6_met, by="decday")
peat6_all$datetime <- as.POSIXct((peat6_all$Mdate  - 719529)*86400, origin = "1970-01-01", tz = "UTC")
peat6_all$month <- month(peat6_all$datetime)
peat6_all$yr.m <- paste(peat6_all$year, peat6_all$month, sep="_")
peat6_all$dday <- floor(peat6_all$decday)

#Simple average of daily fluxes for gapfilled values
daily <- peat6_all %>%
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
            mGCC = mean(GCCsmooth, na.rm=T),   #green index
            PAR = mean(PAR, na.rm=T),          #photosynthetic active radiation (umol m-2 s-1)
            Tair = mean(TA.y, na.rm=T),        #air temp (deg C)
            Tw = mean(TW_WATER_10cm, na.rm=T), #water temp (deg C)
            Ts = mean(TS_TULE_8cm, na.rm=T),   #soil temp (deg C)
            mVPD = mean(VPD, na.rm=T),         #VPD (kPa)
            PA = mean(PA.y, na.rm=T),          #atm pressure (kPa)
            u. = mean(ustar, na.rm=T),         #friction velocity (m/s)
            Rain = sum(PRECIP, na.rm=T),       #rain (mm)
            WTD = mean(WT, na.rm=T),           #water table (m from surface)
            Cond = mean(Cond, na.rm=T),        #conductivity (mS)
            month = round(median(month)),
            year = round(median(year))) %>%
  filter(year < 2017) #only pre-2017

#Time and unit conversions
daily$datetime <- as.POSIXct(daily$dday*86400, origin="2010-01-01")
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

#resave with specific names, and add a site name variable
peat6_yearly <- yearly
peat6_daily <- daily
peat6_yearly$site <- "Peat - 6 yr old"
peat6_daily$site <- "Peat - 6 yr old"
peat6_all$site <- "Peat - 6 yr old"

#remove other dataframes
rm(yearly); rm(daily); rm(peat6_eddy); rm(peat6_met)

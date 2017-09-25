#' Script to load, merge, and process daily to annual budgets.
#' Code varies for each site as input met variables vary across sites
#' Site: Alluvium
#'
#' @import dplyr
#' @importFrom dplyr "%>%"
#' @importFrom zoo na.approx

# load eddy flux and met dataset
load("data/alluvium_all.Rdata")

# create a long-term water temperature measure, as the probes have changed over time
# when we don't have a 10cm water temp measurement, add 8cm depth in place
alluvium_all$TW_10cm <- ifelse(is.na(alluvium_all$TW_10cm), alluvium_all$TW_8cm, alluvium_all$TW_10cm)
alluvium_all$TW_10cm <- na.approx(alluvium_all$TW_10cm, na.rm=F) # linear interpolate holes

#Simple average of daily fluxes for pre-gapfilled values
daily <- alluvium_all %>%
  group_by(dday, site) %>%
  summarise(mNEE = mean(wc_gf),         #NEE (umol m-2 s-1)
            vNEE = mean(wc_gf_var),     #NEE variance (umol m-2 s-1)^2
            c_obs = sum(!is.na(wc)),    #num of non-filled observations (applies all _obs)
            mCH4 = mean(wm_gf),         #CH4 flux (nmol m-2 s-1)
            vCH4 = mean(wm_gf_var),     #CH4 variance (nmol m-2 s-1)^2
            m_obs = sum(!is.na(wm)),
            GPP = mean(gpp_ANNnight),   #partitioned GPP (umol m-2 s-1)
            ER = mean(er_ANNnight),     #partitioned respiration (umol m-2 s-1)
            vER = mean(er_ANN_var),     #partitioned respiration variance (umol m-2 s-1)^2
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
            t_obs = length(wc_gf),      # total observations in the day
            month = round(median(month)),
            year = round(median(year))) %>%
  filter(year > 2013 & t_obs == 48) #cut out pre-wetland measures, and incomplete days at edge of time series

#Time and unit conversions
daily$datetime <- as.POSIXct(daily$dday*86400, origin="2013-01-01")
daily$mgCH4 <- (daily$mCH4*12.01*3600*24)/1000000 #mg C m-2 d-1
daily$gCO2 <- (daily$mNEE*12.01*3600*24)/1000000  #g C m-2 d-1
daily$gER <- (daily$ER*12.01*3600*24)/1000000     #g C m-2 d-1
daily$gGEP <- (daily$GPP*12.01*3600*24)/1000000   #g C m-2 d-1

#Annual fluxes from daily fluxes
yearly <- daily %>%
  group_by(year, site) %>%
  summarise(tCH4 = mean(mgCH4)*365/1000, #gC m-2 yr-1 (same for all fluxes below)
            ciCH4 = 1.96*(sqrt(mean(vCH4)*(12.01*3600*24*365/(10^9))^2)/sqrt(20)), #95% confidence interval from 20 ANNs cumulative
            tNEE = mean(gCO2)*365,
            ciNEE = 1.96*(sqrt(mean(vNEE)*(12.01*3600*24*365/(10^6))^2)/sqrt(20)), #95% confidence interval from 20 ANNs cumulative
            tER = mean(gER)*365,
            ciER = 1.96*(sqrt(mean(vER)*(12.01*3600*24*365/(10^6))^2)/sqrt(20)), #95% confidence interval from 20 ANNs cumulative
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

#remove other dataframes
rm(yearly); rm(daily);






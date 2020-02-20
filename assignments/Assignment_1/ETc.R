### Calculate crop evapotranspiration

# ETc: Crop evapotranspiration [mm d−1],
# Kc: Crop coefficient [–].
#ETo: Reference evapotranspiration [mm d−1],

# Δ: Slope vapor pressure curve [kPa °C−1],
# Rn: Net radiation at the crop surface [MJ m−2 d−1],
# G: Soil heat flux density [MJ m−2 d−1],
# T: Air temperature at 2 m height [°C],
# es: Saturation vapor pressure [kPa],
# ea: Actual vapor pressure [kPa],
# es − ea: Saturation vapour pressure deficit [kPa],
# u2: Wind speed at 2 m height [m s−1], and
# γ: Psychometric constant [kPa °C−1].

# ETc = Kc*ETo
# ETo = (0.408*Δ*(Rn-G)+γ*(900/T+273)*u2*(es-ea))/(Δ+γ(1+0.34*u2))

ETc <- function(u2, Kc, temp = 20, es = 2.3, ea = 1.1, P = 100, Rn = 6, G = 0) {
  # error checking
  temp = ifelse((temp < -50 | temp > 60), return("Unexpected value for temperature"), temp)
  Kc = ifelse((Kc < 0), NA, Kc)
  u2 = ifelse((u2 < 0), NA, u2)
  
  # intermediate calculations
  delta <- (4098*0.6108*2.718^(17.27*temp/(temp+237.3)))/(temp+237.3)^2
  gamma <- 0.000665*P
  vapor_P_deficit <- es-ea
  # one <- delta*0.408*(Rn-G)/(delta+gamma*(1+0.34*u2))
  # two <- (gamma*(900/(temp+273))*u2*vapor_P_deficit)/(delta+gamma*(1+0.34*u2))
  # ETo2 <- one + two
  
  # calculate reference ET
  ETo <- (0.408*delta*(Rn-G)+gamma*(900/(temp+273))*u2*vapor_P_deficit)/(delta+gamma*(1+0.34*u2))
  
  #calculate ETc
  ETc <- ETo*Kc
  return(ETc)
}

# Test function
# ETc(2, 1.2)


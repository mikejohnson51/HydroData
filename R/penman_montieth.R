#' penman_montieth
#'
#' Computes evapotranspiration
#'
#' @param    Tair    (deg C) air temperature
#' @param    vpd     (Pa)    vapour pressure deficit
#' @param    Rnet    (j/m2/day)      net radiation
#' @param    gs      (mm/s)  surface conductance
#' @param    ga      (mm/s)  aerodynamic conductance
#' @param    CP      1010.0 (J/kg*K) specific heat of air
#' @param    Pair    air pressure (default 101325 (Pa)
#' @param    dayl    number of hours in the day (default 24)
#' @author   Mike Johnson
#' @return   Evapotranspiration (mm/day)


pm = function(Tair = NULL, vpd = NULL, Rnet = NULL, gs = .5, ga = NULL, CP=1010, Pair=101325, dayl=24) {

    ######################## Internal Variables:####################
    #                                                              #
    #       rho     (kg/m3)     density of air                     #
    #       lhvap   (J/kg)      latent heat of vapourization H20   #
    #       s       (Pa/degC)   slope of sat vpd vs T curve        #
    #       rs      (s/m)       surface resistance                 #
    #       ra      (s/m)       aerodynamic resistance             #
    #                                                              #
    ################################################################

  # Convert Rnet to daytime value in j/m2/s
  Rnet = Rnet / (60*60*dayl)

  # Convert conductance to resistance and change units
  rs = 1000.0/gs
  ra = 1000.0/ga

  # Assign tk (Kelvins)
  tk = Tair + 273.15

  # Density of air (rho) as a fn. of air temp.
  rho = Pair / (287.058 * tk)
    #1.292 * ( 0.00428 * tk )

  # Latent heat of vapourization as a fn. of Tair.
  lhvap = 2.5023e6 - 2430.54 * tk

  # Temperature offsets for slope estimates
  dt = 0.2
  t1 = tk + dt
  t2 = tk - dt

  # Saturation vapour pressures at t1 and t2(Pa)
  pvs1 = 610.7 * exp(17.38 * t1 / ( 239.0 + t1))
  pvs2 = 610.7 * exp(17.38 * t2 / ( 239.0 + t2))

  # Slope of pvs vs T curve at Tair (Pa/deg C)
  s = ( pvs1 -   pvs2 ) / ( t1 -  t2 )

  # Calculate gamma
  gamma = CP * Pair / ( lhvap )

  # Evaporation in W/m2
  et = ((s*Rnet) + (rho*CP*vpd/ra)) / (gamma*(1.0 + rs/ra) +s)


  # mH20/s = W/m2 * 1kgH20/lhvap J * 1m3H20/1000kGH20
  ewater = ( et/ ( lhvap * 1000 ))

  # mmH20/day
  ewater.day = ewater * 60*60 * dayl* 1000

  # return from your function
  return(ewater.day)

}



compute_ga = function(zm = 10, z0 = .123, zd = .67, v = NULL, vegH = .12) {

  # zm height of wind measurement (standard height @ airport 10m)
  # z0 roughness height
  # zd zero-plane displacment
  # v wind velocity (m/sec)
  # vegH = vegitation height (default = .12 m)


  v = v*277778 # Convert km/hr (WU units) to mm/sec

  z0 = z0 * vegH
  zd = zd * vegH

  val = ((zm - zd)/z0)

  c = v / (6.25 * log(val)^2) # Returns mm/sec

  return(c)

}


get.vpd = function (tmin = NULL, tmax = NULL, rhmin = NULL, rhmax = NULL) {
  esmn <- .6108 * exp((17.27 * tmin) / (tmin + 237.3))
  esmx <- .6108 * exp((17.27 * tmax) / (tmax + 237.3))
  es = (esmn + esmx) / 2

  ea <- ((esmn * rhmax/100) + (esmx * rhmin/100)) / 2

  vpd = es - ea    # in kilo-pascal
  vpd = vpd * 1000 # Convert to Pa

  return(vpd)
}


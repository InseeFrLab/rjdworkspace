ts_r2jd <- function(s){
  freq <- frequency(s)
  start <- start(s)
  jd_freq <- .jcall("ec/tstoolkit/timeseries/simplets/TsFrequency", "Lec/tstoolkit/timeseries/simplets/TsFrequency;", "valueOf", as.integer(freq))
  jd_period <- .jnew("ec/tstoolkit/timeseries/simplets/TsPeriod", jd_freq,
                     as.integer(start[1]), as.integer(start[2] - 1))
  ts <- .jnew("ec/tstoolkit/timeseries/simplets/TsData", jd_period, as.double(s), FALSE)
  return(ts)
}

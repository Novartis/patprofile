.handle_asr <- function(SL, nsl){
  if("ASR" %in% nsl){
    asr <- paste("<b> Age/Sex/Race: </b>", SL$ASR,  '&nbsp  &nbsp')
  } else{
    agecols <- c("AGE", "AGEDRV")
    racecols <- c("RACE", "RACEOR")
    sexcols <- c("SEX", "SEDX")
    if(any(agecols %in% nsl)){
      age <- SL[, intersect(agecols, nsl)][[1]]
      age <- paste0("<b> Age: </b>", age)
    } else{
      age <- NULL
    }
    if(any(sexcols %in% nsl)){
      sex <- SL[, intersect(sexcols, nsl)][[1]]
      sex <- paste0("<b> Sex: </b>", sex)
    } else{
      sex <- NULL
    }
    if(any(racecols %in% nsl)){
      race <- SL[, intersect(racecols, nsl)][[1]]
      race <- paste0("<b> Race: </b>", race)
    } else{
      race <- NULL
    }
    asr <- paste(age, sex, race, '&nbsp  &nbsp')
  }
  return(asr)
}
.get_summary_text <- function(SL){
  nsl <- names(SL)
  # CCS
  if("CCS" %in% nsl){
    ccs <- paste("<b> Country/Centre/Patient: </b>", SL$CCS, '&nbsp  &nbsp')
  } else{
    countrycols <- c("COUNTRY", "COUNTRYL")
    if(any(countrycols %in% nsl)){
      country <- SL[, intersect(countrycols, nsl)][[1]]
      country <- paste0("<b> Country: </b>", country)
    } else{
      country <- NULL
    }
    ccs <- paste(country, '&nbsp  &nbsp')
  }
  asr <- .handle_asr(SL, nsl)
  if("ARM" %in% nsl){
    trt <- paste("<b> Arm: </b>", SL$ARM, '&nbsp  &nbsp')
  } else if("TRT01A" %in% nsl){
    trt <- paste("<b> Arm: </b>", SL$TRT01A, '&nbsp  &nbsp')
  } else{
    trt <- ""
  }

  patient_txt <- paste("<span style='font-size: 16px; text-align: center;'>",
                       ccs, asr, trt)
  return(patient_txt)
}

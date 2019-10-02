display_date <- function (x) {
  toupper (format (as.Date (x, origin='1960-01-01'), '%d%b%Y'))
}
#' Get AECM data
#'
#' Create a dataset from ADSL, ADAE and ADCM for a unique subject
#'
#' @param uid A \code{character}. A subject ID available in all three datasets
#' @param ADSL A \code{data.frame}. The Subject Level data.
#' @param ADAE A \code{data.frame}. The Adverse Event data.
#' @param ADCM A \code{data.frame}. The Concomitent Medication data.
#'
#' @details
#' The AE+CM module displays concomitant medications and adverse events for
#' the patient over the study period. Events can be colored by severity, grade
#' or action taken.
#'
#' @importFrom dplyr filter select mutate starts_with desc one_of
#' @seealso plot_aecm
#'
get_aecm_data <- function(uid, ADSL, ADAE, ADCM){
  ggADSL <- filter(ADSL, USUBJID == uid)
  ggADSL <- ggADSL[1,] #Some studies have multiple USUBJID entries
  if("RANDDT" %in% names(ggADSL)){
    min_x <- as.Date(ggADSL$RANDDT, origin = '1960-01-01') - 30
  } else if("RANDDTM" %in% names(ggADSL)){
    min_x <- as.Date(ggADSL$RANDDTM/86400, origin = '1960-01-01') - 30
  } else{
    min_x <- as.Date(ggADSL$TRTSDT, origin = '1960-01-01') - 30
  }

  if("LSTALDT" %in% names(ggADSL)){
    max_x <- max(as.Date(ggADSL$LSTALDT, origin = '1960-01-01'),
                 as.Date(ggADSL$TRTEDT, origin = '1960-01-01')) + 1
  } else{
    max_x <- max(as.Date(ggADSL$TRTEDT, origin = '1960-01-01')) + 1
  }
  trt_start <- as.Date(ggADSL$TRTSDT, origin = '1960-01-01')
  trt_end <-   as.Date(ggADSL$TRTEDT, origin = '1960-01-01')

  selcolsAE <- c("USUBJID", "AEDECOD", "AEBODSYS", "ASTDT", "AENDT", "AEREL",
                 "AESTDTC", "AEENDTC", "AEOUT", "ASTDY", "AESER", "AESEV",
                 "AEACN", "AETOXGRN")
  ggADAE <- select(ADAE, one_of(selcolsAE))

  ggADAE <- ggADAE %>%
    filter(USUBJID == uid) %>%
    mutate(
      start = as.Date(ASTDT, origin = '1960-01-01'),
      startt = display_date(ASTDT),
      end =  as.Date(AENDT, origin = '1960-01-01') +1,
      endt = display_date (AENDT),
      aenum = rank (AEDECOD),
      type = 'AE',
      name = AEDECOD,
      colour_ser = ifelse(AESER == 'Y', 'Yes', 'No'),
      colour_action = AEACN,
      text = paste0("<b> Start: </b>", startt, "<br/>",
                    "<b> End: </b>", endt, "<br/>",
                    "<b> AE: </b>", AEDECOD, "<br/>",
                    "<b> Body system: </b>", AEBODSYS, "<br/>",
                    "<b> Serious: </b>", AESER, "<br/>",
                    "<b> Related: </b>", AEREL, "<br/>",
                    "<b> Action taken: </b>", AEACN, "<br/>",
                    "<b> Outcome: </b>", AEOUT, "<br/>"))
  if("AESEV" %in% names(ggADAE)){
    ggADAE <- ggADAE %>% mutate(
      colour_sev = AESEV,
      text = paste0(text, "<b> Severity: </b>", AESEV, "<br/>"))
  }
  if("AETOXGRN" %in% names(ggADAE)){
    ggADAE <- ggADAE %>% mutate(
      colour_grade = paste('Grade', AETOXGRN),
      text = paste0(text, "<b> Grade: </b>", AETOXGRN, "<br/>"))
  }

  ggADAE <- ggADAE %>%
    select(type, name, start, end, text, starts_with("colour_"))

  selcolsCM <- c("USUBJID", "ASTDT", "AENDT", "CMDECOD", "CMSEQ", "CMINDC",
                "CMDOSE", "CMDOSU", "CMSTDTC", "CMENDTC", "APHASE", "CMROUTE")
  ggADCM <- select(ADCM, one_of(selcolsCM))
  ggADCM <- ggADCM %>%
    filter(USUBJID == uid) %>%
    mutate(
      start = as.Date(ASTDT, origin='1960-01-01' ),
      startt = display_date(ASTDT),
      end =  as.Date(AENDT, origin='1960-01-01' ) +1,
      endt = display_date(AENDT),
      type = 'CM',
      name = CMDECOD,
      colour_sev = "CM",
      colour_ser = "CM",
      colour_action = "CM",
      colour_grade = "CM",
      text = paste0("<b> CM Term: </b>", CMDECOD, "<br/>",
                    "<b> Start: </b>", startt, "<br/>",
                    "<b> End: </b>", endt, "<br/>",
                    "<b> Start Char: </b>", CMSTDTC, "<br/>",
                    "<b> End Char: </b>", CMENDTC, "<br/>",
                    "<b> Dose/Route: </b>", CMDOSE, CMDOSU, '/', CMROUTE, "<br/>",
                    "<b> Indication: </b>", CMINDC, "<br/>")) %>%
    filter(end >= min_x | is.na (end)) %>%
    select(type, name, start, end, starts_with("colour_"), text)

  # Dynamically generate text based on available information
  if("APHASE" %in% names(ggADCM)){
    ggADCM <- ggADCM %>%
      mutate(text = paste0(text, "<b> Phase: </b>", APHASE, "<br/>"))
  }


  ggADCM$end[is.na(ggADCM$end)] <- max_x
  ggADCM$start[ggADCM$start < min_x] <- min_x
  ggAECM <- bind_rows(ggADAE, ggADCM) %>% arrange(desc(start))

  # get first instance of each name:
  f_order <- ggAECM %>%
    group_by(name) %>%
    summarise('first' = min (start)) %>%
    arrange(desc(first)) %>%
    .$name
  ggAECM$name <- factor(ggAECM$name, levels = f_order)
  n_aecm <- length(unique(ggAECM$name))

  aecm_ref <- data_frame('start' = c(trt_start), 'end' = c(trt_end))


  ret <- list(aecm = ggAECM, aecm_ref = aecm_ref, nrow = n_aecm,
              min_x = min_x, max_x = max_x, trt_start = trt_start, trt_end = trt_end)
  return(ret)
}

#' Get lab values
#'
#' Get lab values for lab overview plot
#'
#' @param uid A \code{character}. A unique subject ID available in \code{ADLB}.
#' @param ADLB A \code{data.frame}. The lab values
#'
#' @importFrom dplyr arrange
get_lab_values <- function(uid, ADLB){

  # selcols <- c("USUBJID", "PARAMCD", "ATOXGR", "BASE", "VISIT", "AVISIT",
  #              "VISITNUM", "AVAL", "ADT", "TRTSDT", "PARAM", "ADY", "PARCAT1",
  #              "ANRIND", "ANRLO", "ANRHI", "BNRIND")
  #
  # ppLB <- select(ADLB, one_of(selcols))
  ppLB <- ADLB %>%
    filter(USUBJID == uid, !is.na (AVAL), AVISIT != 'UNS') %>%
    filter(!AVISIT %in% c('', "Worst post baseline (CTC)", "EOT")) %>%
    filter(!grepl("maximum", x = AVISIT, ignore.case = T)) %>%
    mutate(date = as.Date(ADT, origin = '1960-01-01')) %>%
    arrange(ADY, PARAMCD)

  if("ATOXGR" %in% colnames(ppLB)){
    ppLB <- ppLB %>%
      filter(!is.na(ATOXGR), ATOXGR != "") %>%
      mutate(ATOXGR = factor(abs(as.numeric(ATOXGR))))
  }
  if("LBNRIND" %in% colnames(ppLB)){
    ppLB <- ppLB %>%
      mutate(direction = ifelse(ANRIND == "HIGH", 'high', ifelse(ANRIND == "LOW", 'low', 'square')))
    ppLB$ANRIND <- factor(ppLB$ANRIND, levels = c('LOW', 'NORMAL', 'HIGH'))
  }

  ppLB$AVISIT2 <- as.factor(ppLB$AVISIT)
  l1 <- unique(ppLB$AVISIT)
  levels(ppLB$AVISIT2) <- l1

  return (ppLB)
}


#' Get subject info
#'
#' Get subject level info for a participant
#'
#' @return A \code{list} with subject level information for \code{uid}.
get_uid_info <- function(uid, ADSL){
  info <- list()
  idADSL <- filter(ADSL, USUBJID == uid)

  info$pADSL <- idADSL
  info$rand <- as.Date(idADSL$RANDDT, origin = '1960-01-01')

  info$trt_start  <- as.Date(idADSL$TRTSDT, origin = '1960-01-01')
  info$trt_startt <- display_date(idADSL$TRTSDT)
  info$trt_end  <- as.Date(idADSL$TRTEDT, origin = '1960-01-01')
  info$trt_endt <- display_date(idADSL$TRTEDT)

  info$last_alive <- as.Date(idADSL$LSTALDT, origin = '1960-01-01')
  info$dth <- as.Date(idADSL$DTHDT, origin = '1960-01-01')

  if (idADSL$SAFFL == 'Y'){
    if (! is.na (info$dth)) {
      info$last_plot_date <- info$dth + 5
    } else if ((info$last_alive - info$trt_end < 60) & (info$last_alive >= info$trt_end)){
      info$last_plot_date <- info$last_alive
    } else{
      info$last_plot_date <- info$trt_end + 10
    }
  }
  info$dfref <- data_frame('start' = c(info$trt_start), 'end'= c(info$trt_end))
  info$SAFFL <- idADSL$SAFFL
  return(info)
}


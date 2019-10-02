tick_vals <- function(start, end, ref_dt, last_plot_date = end){
  t_time <- end - start
  increment <- ifelse(t_time < 300, 15,
                      ifelse(t_time < 600, 30, 60)
  )
  ticks <- seq(start, end, by = increment)
  tick_days <- ticks - ref_dt + 1*(ticks >= ref_dt)

  scale_x <- scale_x_date(limits = c(start, last_plot_date),
                          breaks = ticks,
                          labels = tick_days)
  return(list('ticks'= ticks, 'tick_days'=tick_days, 'scale_x' = scale_x))
}


#' @importFrom ggplot2 ggplot aes geom_segment geom_rect element_blank element_line element_text
#' @importFrom ggplot2 scale_x_date scale_colour_manual theme theme_bw
#' @importFrom scales date_format
#'
plot_aecm <- function(aecm_list, aecolour = "severity"){
  aecm <- aecm_list$aecm
  aecolour <- tolower(aecolour)
  if(aecolour == 'severity') aecm$colour = aecm$colour_sev
  if(aecolour == 'grade')    aecm$colour = aecm$colour_grade
  if(aecolour == 'action')   aecm$colour = aecm$colour_action
  if(aecolour == 'severity') {
    cs <- scale_colour_manual(values = c('CM' = "#9318db", "AE1"= "#39b9ef", "AE2" = "#078bc4"),
                              name = 'Event',
                              labels = c("CM"="CM", "AE1"= "AE", "AE2" = "SAE"))
  } else if (aecolour == 'grade') {
    cs <-  scale_colour_manual(values = c('CM' = "#9318db",
                                         "Grade 0" = "#eff3ff",
                                         "Grade 1" = "#bdd7e7",
                                         "Grade 2" = "#6baed6",
                                         "Grade 3" = "#3182bd",
                                         "Grade 4" = "#08519c"),
                             name = 'Event' )
  } else if (aecolour == 'action') {
    cs <- scale_colour_manual(values = c('CM' = "#9318db",
                                        "DOSE NOT CHANGED" = "#66c2a5",
                                        "DRUG INTERRUPTED" = "#fc8d62",
                                        "DRUG WITHDRAWN" = "#8da0cb",
                                        "DOSE REDUCED" = "#e78ac3",
                                        "UNKNOWN" = "#a6d854"),
                              name = 'Event' )
  }

  plot <- ggplot(aecm) +
    scale_x_date(limits = c(as.Date(aecm_list$min_x), as.Date(aecm_list$max_x)), labels = scales::date_format("%d%b%Y")) +
    geom_segment(aes(x = start, y = name, yend = name, xend = end, colour = colour), size=3) +
    geom_rect(data = aecm_list$aecm_ref, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = c("green")) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), panel.border = element_blank(),
      axis.line.x = element_line(color = "black", size = 1), axis.line.y = element_line(color = "black", size = 1),
      axis.title.x = element_blank(), axis.title.y = element_blank()) +
    cs
  return(plot)
}


#' @importFrom ggplot2 ggplot_gtable ggplot_build geom_line
#' @importFrom ggplot2 xlab ylab scale_colour_discrete
#' @importFrom grid grid.draw grid.newpage
plot_EXLB <- function(uid, ADLB, ADSL, ADEX, ADPC, paramcd = c('ALP', 'ALT', 'AST', 'BILI')){

  ggADSL <- filter(ADSL, USUBJID == uid)
  trt_start  <- as.Date(ggADSL$TRTSDT,  origin = '1960-01-01')
  trt_end    <- as.Date(ggADSL$TRTEDT,  origin = '1960-01-01')
  last_alive <- as.Date(ggADSL$LSTALDT, origin = '1960-01-01')
  dth        <- as.Date(ggADSL$DTHDT,   origin = '1960-01-01')

  uid_info <- get_uid_info(uid, ADSL)
  ex_plot <- plot_EX(uid, info = uid_info, ADEX = ADEX, ADPC = ADPC)

  ticks <- tick_vals(uid_info$trt_start - 30,  uid_info$last_plot_date, uid_info$trt_start )
  scale_x <- scale_x_date(limits = c(uid_info$trt_start - 30, uid_info$last_plot_date),
                          breaks = ticks$ticks,
                          labels = scales::date_format("%d%b%y"),
                          position = 'top')

  ggADLB <- ADLB %>%
    filter(USUBJID == uid, PARAMCD %in% paramcd, !is.na(AVAL)) %>%
    select(USUBJID, PARAMCD, ATOXGR, BASE, VISIT, VISITNUM, AVAL, ADT, TRTSDT, PARAM, NORMAVAL) %>%
    mutate (date = as.Date (ADT, origin = '1960-01-01'))

  plot_LB <- ggplot(ggADLB) +
    geom_line(size = 1.5, aes (x = date, y = NORMAVAL, col = PARAMCD)) +
    geom_rect(data = uid_info$dfref, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              alpha = 0.1, fill= c("green")) +
    scale_x + scale_colour_discrete(name = 'Lab Parameter:') +
    ylab("Value / ULN") +
    theme_bw() +
    theme(legend.justification = c(0, 1), legend.position = c(0.01, 0.99)) +
    theme(axis.title.x = element_blank())

  gb1 <- ggplot_build(plot_LB)
  gb2 <- ggplot_build(ex_plot)
  n1 <- length(gb1$panel$ranges[[1]]$y.labels)
  n2 <- length(gb2$panel$ranges[[1]]$y.labels)
  n2
  gA <- ggplot_gtable(gb1)
  gB <- ggplot_gtable(gb2)
  g <- gtable:::rbind_gtable(gA, gB, "last")

  panels <- g$layout[grepl("panel", g$layout$name), ]
  ulist <- c(ggplot2::unit(70, units = "null"),
             ggplot2::unit(20,units = "null" ))
  lapply(1:2, function(i) {g$heights[unique(panels$t)][i] <- unit(ulist[i], "null")})# change 5 to other int

  grid.newpage()
  grid.draw(g)
}


#' Plot lab visits
#' @importFrom ggplot2 aes_string geom_point scale_fill_manual scale_shape_manual facet_grid
#' @importFrom RColorBrewer brewer.pal
plot_lab_visits <- function(lab_data, color_by = "LBNRIND"){

  cols <- c("#225EA8", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C")

  if(color_by == "ATOXGR"){
    aescol <- aesfill <- "ATOXGR"
    sc <- scale_colour_manual(values = c(cols, 'grey'))
    sf <- scale_fill_manual(values = c(cols, 'grey'))
  } else if(tolower(color_by) == "ntbcrit"){ #Specific to QAW should
    ntbcrit_cols <- c("H" = "#FC4E2A", "N" = "#225EA8", "black")
    aescol <- aesfill <- "NTBCRIT"
    sc <- scale_colour_manual(values = ntbcrit_cols)
    sf <- scale_fill_manual(values = ntbcrit_cols)
  } else if(tolower(color_by) == "none"){
    aescol <- aesfill <- NULL
    sc <- sf <- NULL
  } else{
    aescol <- aesfill <- color_by
    sc <- sf <- NULL
  }

  aesshape <- NULL
  if("ANRIND" %in% colnames(lab_data)){
    aesshape <- "ANRIND"
  }
  
  if("LBNRIND" %in% colnames(lab_data)){
    aesshape <- "LBNRIND"
  }
  

  #browser()
  p <- ggplot(data = lab_data,
              aes_string(x = "AVISIT2", y = "PARAM", col = aescol, fill = aesfill, shape = aesshape))

  p <- p + geom_point(size = 3) + sc + sf +
    scale_shape_manual(values = c('LOW' = 25, 'NORMAL' = 22, 'HIGH' = 24)) +
    theme_bw(base_size=15) +
    theme(
      panel.grid.major = element_blank(), panel.border = element_blank(),
      axis.line.x = element_line(color = "black", size = 1),
      axis.line.y = element_line(color = "black", size = 1),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90))

  if("PARCAT1" %in% colnames(lab_data)){
    p <- p + facet_grid(PARCAT1~., scales = "free", space = "free")
  }
  return(p)
}

#' Plot Lab params
#'
#' Plot lab parameters
#'
#' @importFrom ggplot2 ggplot geom_hline geom_text
plot_lab_param <- function(uid, params, ADLB){
  selcols <- c("USUBJID", "PARAMCD", "BASE", "VISIT", "VISITNUM", "AVAL", "ADT",
               "ANRLO", "ANRHI", "TRTSDT", "TRTEDT", "PARAM", "ADY", "ONTRTFL")

  selADLB <- select(ADLB, one_of(selcols))
  dfLB <- selADLB %>%
    filter(USUBJID == uid, PARAMCD %in% params, !is.na(AVAL)) %>%
    mutate(date = as.Date(ADT, origin = '1960-01-01'))

  dfLB <- dfLB %>%
    group_by(PARAMCD) %>%
    mutate(range_l = min(as.numeric(ANRLO), na.rm=TRUE),
           range_h = max(as.numeric(ANRHI), na.rm=TRUE))


  miny <- 0.97 * (min(min(dfLB$AVAL, na.rm=TRUE), min(dfLB$range_l, na.rm=TRUE)))
  maxy <- 1.03 * (max(max(dfLB$AVAL, na.rm=TRUE), min(dfLB$range_h, na.rm=TRUE)))

  if("TRTSDT" %in% names(dfLB)){
    minx <- as.Date(min(dfLB$ADT, dfLB$TRTSDT - 30),  origin = '1960-01-01')
  } else{
    minx <- as.Date(min(dfLB$ADT),  origin = '1960-01-01')
  }
  if("TRTEDT" %in% names(dfLB)){
    maxx <- as.Date(max(dfLB$ADT, dfLB$TRTEDT + 30),  origin = '1960-01-01')
  } else{
    maxx <- as.Date(max(dfLB$ADT),  origin = '1960-01-01')
  }
  # minx <- as.Date(min(min(dfLB$ADT, dfLB$TRTSDT - 30)),  origin = '1960-01-01')
  # maxx <- as.Date(max(max(dfLB$ADT, dfLB$TRTEDT + 30)),  origin = '1960-01-01')
  ylab <- unique(dfLB$PARAM)

  scale_x <- scale_x_date(limits = c(minx, maxx),
                          labels = scales::date_format("%d%b%y"),
                          position = 'bottom')
  labplot <- ggplot(dfLB, aes(x = date, y = AVAL)) +
    geom_line(colour = 'darkgreen', size = 1) +
    geom_point(colour = ' darkgreen') +
    facet_grid(PARAM~., scales='free') +
    theme_bw() + xlab('Date') +
    geom_hline(aes(yintercept = range_h), colour= 'red', linetype = 'dashed') +
    geom_hline(aes(yintercept = range_l), colour= 'red', linetype = 'dashed') +
    geom_text(aes(minx, range_h, label = 'UNL', vjust = + 1, colour = 'red')) +
    geom_text(aes(minx, range_l, label = 'LNL', vjust = - 1, colour = 'red')) +
    theme(legend.position = "none")
  # output df for hover
  return(list('plot' = labplot, 'df' = dfLB))
}




#' Plot EX
#' @importFrom ggplot2 scale_y_continuous geom_step sec_axis
#' @importFrom dplyr bind_rows
plot_EX <- function(uid, info, ADEX, ADPC, tick_scale = TRUE){
  dfEX <- ADEX %>%
    filter(USUBJID == uid, !is.na(ASTDT), EXTRT == "LEE011/PLACEBO") %>%
    select(USUBJID, ASTDT, AENDT, EXTRT, EXDOSE) %>%
    mutate(date= as.Date(ASTDT, origin='1960-01-01'),
           end = as.Date(AENDT, origin='1960-01-01'),
           EXDOSE = EXDOSE) %>%
    arrange (date)

  dfPK <- ADPC %>%
    filter(USUBJID == uid, PARAM == "LEE011 plasma concentrations (ng/ml)", !is.na(AVAL)) %>%
    select(USUBJID, PARAMCD, AVISIT, AVISITN, AVAL, ADT,
           TRTSDT, TRTEDT, PARAM, ADY, ONTRTFL, ATPT, SMPNO, EXDOSE, PKFL) %>%
    mutate(date = as.Date(ADT, origin = '1960-01-01'),
           SMPNO = as.numeric(SMPNO),
           ONTRTFL = ifelse(ONTRTFL == 'Y', 'Yes', 'No'))

  last_date <- dfEX[nrow(dfEX), ] %>% mutate (date = end)
  dfEX <- bind_rows(dfEX, last_date)

  ticks <- tick_vals(info$trt_start - 30, info$last_plot_date, info$trt_start)
  scale_x <- scale_x_date(limits = c(info$trt_start - 30, info$last_plot_date),
                          breaks = ticks$ticks,
                          labels = ticks$tick_days)

  ex_plot <- ggplot(dfEX, aes(x = date, y = EXDOSE)) +
    geom_step(size = 1) + geom_point(data = dfPK, aes(x = date, y = AVAL, colour = ATPT)) +
    theme_bw() +
    xlab('Study Day') + ylab('Dose') +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Concentration")) +
    theme(legend.position="bottom")

  if(tick_scale){
    ex_plot <- ex_plot + scale_x
  }
  return(ex_plot)
}

#' Plot EXEG
#'
#' Plot Exposure & ECG
#' @importFrom ggplot2 ggplot_gtable ggplot_build unit
#' @importFrom grid grid.draw grid.newpage
plot_EXEG <- function(uid, param, ADSL, ADEX, ADPC, ADEG){
  ggADSL <- filter(ADSL, USUBJID == uid)

  trt_start  <- as.Date(ggADSL$TRTSDT, origin = '1960-01-01')
  trt_end    <- as.Date(ggADSL$TRTEDT, origin = '1960-01-01')
  last_alive <- as.Date(ggADSL$LSTALDT, origin = '1960-01-01')
  dth <- as.Date(ggADSL$DTHDT, origin = '1960-01-01')

  # Plots
  uid_info <- get_uid_info(uid, ADSL)
  ex_plot <- plot_EX(uid, info = uid_info, ADEX = ADEX, ADPC = ADPC)
  ecg_plot <- plot_ECG(uid, param, ADEG)[['plot']]

  # Combining plots
  gb1 <- ggplot_build(ecg_plot)
  gb2 <- ggplot_build(ex_plot)
  n1 <- length(gb1$panel$ranges[[1]]$y.labels)
  n2 <- length(gb2$panel$ranges[[1]]$y.labels)
  gA <- ggplot_gtable(gb1)
  gB <- ggplot_gtable(gb2)
  g <- gtable:::rbind_gtable(gA, gB, "last")
  panels <- g$layout[grepl("panel", g$layout$name), ]
  ulist <- c(ggplot2::unit(70, units = "null"),
             ggplot2::unit(20,units = "null" ))
  lapply(1:2, function(i) {g$heights[unique(panels$t)][i] <- unit(ulist[i], "null")})# change 5 to other int

  grid.newpage()
  grid.draw(g)
}


#' Plot ECG
#'
#' Plot ECG
#'
plot_ECG <- function(uid, paramcd, ADEG){
  dfEG <- ADEG %>%
    filter(USUBJID == uid, PARAMCD == paramcd, !is.na(AVAL)) %>%
    select(USUBJID, PARAMCD, AVISIT, AVISITN, AVAL, ADT,
           TRTSDT, TRTEDT, PARAM, ADY, ONTRTFL, ATPT, BASE, DTYPE, ADTM) %>%
    mutate(date = as.Date(ADT, origin = '1960-01-01'),
           ONTRTFL = ifelse(ONTRTFL == 'Y', 'Yes', 'No'))

  parameter <- unique(dfEG$PARAM)
  trtsdt <- as.Date(unique(dfEG$TRTSDT), origin='1960-01-01')

  miny <- 0.97 * min(dfEG$AVAL, na.rm=TRUE)
  maxy <- 1.03 * max(dfEG$AVAL, na.rm=TRUE)
  minx <- as.Date(min(min(dfEG$ADT, dfEG$TRTSDT - 30)), origin = '1960-01-01')
  maxx <- as.Date(max(max(dfEG$ADT, dfEG$TRTEDT + 30)), origin = '1960-01-01')
  ylab <- unique(dfEG$PARAM)

  scale_x <- scale_x_date(limits = c(minx, maxx),
                          labels = scales::date_format("%d%b%y"),
                          position = 'bottom')
  ecgplot <- ggplot(dfEG, aes(x = date, y = AVAL, colour = ATPT)) +
    geom_line(colour = 'black', size=0.2) + geom_point() +
    theme_bw() + xlab('Date') + ylab(ylab) +
    scale_x + scale_y_continuous(limits = c(miny, maxy)) +
    theme(legend.position = "bottom")

  # The data.frame is returned for hover info
  return(list('plot' = ecgplot, 'df' = dfEG))
}

#' Plot PK
plot_PK <- function (uid, param, ADPC) {
    
  dfPK <- ADPC %>%
    filter(USUBJID == uid, PARAM == param, !is.na(AVAL)) %>%
    select(USUBJID, PARAMCD, AVISIT, AVISITN, AVAL, ADT,
           TRTSDT, TRTEDT, PARAM, ADY, ATPT, SMPNO, PKFL) %>%
    mutate(date = as.Date(ADT, origin = '1960-01-01'),
           SMPNO = as.numeric(SMPNO))#,
           #ONTRTFL = ifelse(ONTRTFL == 'Y', 'Yes', 'No'))

  parameter <- unique(dfPK$PARAM)
  trtsdt <- as.Date(unique(dfPK$TRTSDT), origin='1960-01-01')
  miny <- 0.97 * min(dfPK$AVAL, na.rm=TRUE)
  maxy <- 1.03 * max(dfPK$AVAL, na.rm=TRUE)

  pkplot <- ggplot(dfPK, aes(x = SMPNO, y = AVAL, linetype = AVISIT, colour = AVISIT)) +
    geom_line(colour = 'black', size=0.2) + geom_point(aes(shape = PKFL)) +
    theme_bw() +
    xlab('Analysis Timepoint') + ylab('Concentration') +
    scale_y_continuous(limits = c(miny, maxy)) +
    theme(legend.position = "bottom")

  # The data.frame is returned for hover info
  return (list('plot'= pkplot, 'df'= dfPK))
}


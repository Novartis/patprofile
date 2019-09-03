# This script add the AVA library path
# This is needed to run the app on the Shiny Server Pro
if(file.exists('/CHBS/apps/busdev_apps/AVA/RLib/AVA.R')){
  source('/CHBS/apps/busdev_apps/AVA/RLib/AVA.R', local = T)
} else{
  source("http://ava-web.statwb.eu.novartis.net/cfg/installer.R")
}

library(shiny)
library(ava)
library(dplyr, warn.conflicts = F)
library(ggplot2)
library(scales)
library(grid)
library(htmlwidgets)
library(plotly, warn.conflicts = F)
library(pprofile)


# AVA options
options(appName = "Patient profile")
options(author = "Renan Sauteraud")
options(email = "renan.sauteraud@gmail.com")

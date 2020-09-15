# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Global
# programmer:   Zhe Liu
# Date:         2020-07-21
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


if(Sys.getenv('SHINY_PORT') == "") {
  options(shiny.maxRequestSize=1000*1024^2)
}

options(scipen = 200,
        stringsAsFactors = FALSE)

##---- loading the required packages ----
suppressPackageStartupMessages({
  require(zip)
  require(openxlsx)
  require(readxl)
  require(writexl)
  require(RcppRoll)
  require(plyr)
  require(stringi)
  require(feather)
  require(RODBC)
  require(MASS)
  require(car)
  require(data.table)
  require(plotly)
  require(tidyverse)
  require(kknn)
  require(lubridate)
  require(janitor)
  require(digest)
  require(tables)
  require(DT)
  require(shiny)
  require(shinydashboard)
  require(shinydashboardPlus)
  require(shinyjs)
  require(shinyFiles)
})


##---- source function ----

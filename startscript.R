require(shiny)
require(shinydashboard)
require(tidyverse)
require(lubridate)
require(DT)


port <- Sys.getenv('PORT') 

shiny::runApp('SHINY_APP',  host="0.0.0.0", port=as.numeric(Sys.getenv("4443")))

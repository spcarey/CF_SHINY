require(shiny)
require(shinydashboard)
require(plotly)
require(tidyverse)
require(lubridate)
require(DT)


port <- Sys.getenv('PORT') 

shiny::runApp('R_TEST_APP', host = '0.0.0.0', port = as.numeric(port))

if (!require("knitr")) install.packages("knitr")
library(knitr)
if (!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown)
if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("DT")) install.packages("DT")
library(DT)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("purrr")) install.packages("purrr")
library(purrr)
if (!require("readr")) install.packages("readr")
library(readr)
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)
if (!require("writexl")) install.packages("writexl")
library(writexl)
if (!require("readxl")) install.packages("readxl")
library(readxl)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("stringr")) install.packages("stringr")
library(stringr)
if (!require("rsconnect")) install.packages("rsconnect")
library(rsconnect)
if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("DT")) install.packages("DT")
library(DT)
if (!require("here")) install.packages("here")
library(here)
if (!require("leaflet")) install.packages("leaflet")
library(leaflet)
if (!require("sf")) install.packages("sf")
library(sf)
if (!require("R6")) install.packages("R6")
library(R6)
if (!require("leaflet.extras")) install.packages("leaflet.extras")
library(leaflet.extras)
if (!require("shinyWidgets")) install.packages("shinyWidgets")
library(shinyWidgets)
if (!require("shinydashboard")) install.packages("shinydashboard")
  library(shinydashboard)
if (!require("htmlwidgets")) install.packages("htmlwidgets")
library(htmlwidgets)
if (!require("plotly")) install.packages("plotly")
library(plotly)
if (!require("tools")) install.packages("tools")
library(tools)



# fecha_inicio <- make_date(year = 2024, month = 10, day = 10)



ruta_proyecto <-  normalizePath(getwd())
ruta_proyecto <- here()
Sys.setenv(TZ = "America/Montevideo")




















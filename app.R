library(shiny)
library(dplyr)
library(magrittr)
library(DT)
library(shinydashboard)
library(shinyjs)

source("ui.R")
source("server.R")




shinyApp(ui, server)
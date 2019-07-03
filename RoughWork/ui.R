## --------features to complete 
## TABS FOR PATIETS AND COHORT BASED ANALYSIS
## BUTTON TO NORMALISE AND ANALYSE THE DATA


library(shiny)
library(shinyWidgets)
library(shinydashboard)

### UI

### --- 1 Structure layout
ui <- DashboardPage(
  skin = "green",
  dashboardHeader(title = "I2EHR"),
  dashboardSidebar(
    
    ### --- 2 Sidebar items 
    sidebarMenu(
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("id-card")),
      menuItem("Patient Analysis",
               tabName="Patient_Tab",
               icon=icon("id-card")),
      
      menuItem("Cohort Analysis", 
               icon = icon("poll"), 
               tabName = "Cohort_Tab"))),
  
  ### 3 Body items 
  dashboardBody(
    tags$head(
      # Font set-up
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "custom.css")),
    
    tabItems(
      tabItem(tabName = "overview", 
              box(title = "Welcome to the Interactive Integrated 
                  Electronic Health Record (I2EHR)", 
                  width=8, 
                  status = "success", 
                  solidHeader = TRUE),
              box(plotOutput("PCA_2D_normalised"))
              )
      )
    ) 
  ) # close ui

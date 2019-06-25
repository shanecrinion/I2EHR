##features to complete 
## TABS FOR PATIETS AND COHORT BASED ANALYSIS
## BUTTON TO NORMALISE AND ANALYSE THE DATA



### 2. UI

### --- 2.1 Structure layout
ui <- DashboardPage(
  skin = "green",
  dashboardHeader(title = "I2EHR"),
  dashboardSidebar(
    
    ### --- 2.2 Sidebar items 
    sidebarMenu(
      menuItem("Overview", 
               tabName = "overview", 
               icon = icon("id-card")),
      menuItem("Patient Analysis",
               tabName="Patient",
               icon=icon("id-card")),
      
      menuItem("Cohort Analysis", 
               icon = icon("poll"), 
               tabName = "Cohort"))),
  
)
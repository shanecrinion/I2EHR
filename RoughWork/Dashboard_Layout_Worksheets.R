

### Worksheet 2 ###

## ui.R ##


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName="welcome", icon=icon("info")),
    menuItem("Patient", tabName = "dashboard", icon = icon("id-card")),
    menuItem("Cohort", icon = icon("poll"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            box(
              title = "Welcome to the Interactive Integrated Electronic Health Record (I2EHR)", 
              source("nui-galway.jpg"),
              width=4,
              footer="for more information please e-mail shanecrinion@gmail.com")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)


# Put them together into a dashboardPage
ui <- 
  dashboardPage(
    skin="green",
  dashboardHeader(title = "I2EHR"),
  sidebar,
  body
)


server <- function(input, output){
  
}

shinyApp(ui, server)


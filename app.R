#En la siguietne version inclir las pociones de drive_ls,
#por ejemplo dar opciones de incluir archivos con cierto patron o archivos de un solo tipo con multiple checkbox, etc


library(shiny)
library(googledrive)
library(googlesheets)
library(dplyr)
library(shinydashboard)
library(shinycssloaders)



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Isaac", tabName = "isaac", icon = icon("dashboard")),
    menuItem("Arian", icon = icon("th"), tabName = "arian")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "isaac",
            fluidPage(
              titlePanel(" demo"),
              fluidRow(
                column(4, wellPanel(
                  textInput("n", label = h3("Text input"), value = "Enter text..."),
                  checkboxInput('subfolders', 'Inlcuir subcarpetas', TRUE),
                  actionButton("submit", "Submit"),
                  actionButton("reset", "Clear"), 
                  actionButton("go", "Go"),
                  
                  downloadButton("downloadData", "Download")
                )),
                column(8,
                       tableOutput("text")
                ),
                column(12,
                       withSpinner(tableOutput("text1"))
                )
              )
            )
            
    ),
    
    #aqui va tu parte, Arian
    tabItem(tabName = "arian",
            h2("ss")
    )
  )
)



ui <- 
  # Put them together into a dashboardPage
  dashboardPage(
    dashboardHeader(title = "Simple tabs"),
    sidebar,
    body
  )


server <- function(input, output) {
  
  
  #inventory$input
  
  running=reactiveValues(
    input=NULL
  )
  observed=reactiveValues(
    input=NULL
  )
  
  observeEvent(input$submit,{    
    observed$input <- unique(rbind(observed$input,input$n))
  })
  
  output$text <- renderText({
    print(observed$input)
  })
  
  
  observeEvent(input$reset, {
    observed$input <- NULL
    running$input <- NULL
  })  
  
  
  
  hola <- eventReactive(input$go,{
    if (is.null(observed$input)) stop("dummy error")
    
    running$input<-"Cest fini"
  })
  
  
  
  perro<- eventReactive(input$go,{
    
   
    
    drive_auth()
    
    
    listing <- function(folder) {
      inv_fold=drive_ls(folder, recursive = FALSE)%>%
        select(-drive_resource)
      return(inv_fold)
    }
    
    inventory<-bind_rows(lapply(observed$input, listing))
    
  })
  
  
  
  output$text1<-renderText({
    
    hola()
    perro()
    print(running$input)
  })
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("inventario", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(perro(), file)
    }
  )
  
}

shinyApp(ui <- ui, server <- server)
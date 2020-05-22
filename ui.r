library(shiny)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)


custom_db <- c("Ansteph Genome","Ansteph Protein")
 dashboardPagePlus(skin = "black",
    header = dashboardHeaderPlus(title = "An. stephensi Genome Resource Hub",titleWidth = 450,
                                 enable_rightsidebar = FALSE, rightSidebarIcon = "gears",uiOutput("logoutbtn")
    ),#Header Close
        
    sidebar = dashboardSidebar(sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("dashboard"),selected = TRUE),
      menuItem("Download", tabName = "download", icon = icon("th")),
      menuItem("Tools", tabName = "tool", icon = icon("th")),
      menuSubItem("Blast",tabName = "shinyblast",icon = shiny::icon("angle-double-right"), selected = NULL),
      menuSubItem("JBrowse",tabName = "jbrowse", icon = shiny::icon("angle-double-right"), selected = NULL),
      menuSubItem("SequenceServer-Blast",tabName = "seqserver",icon = shiny::icon("angle-double-right"), selected = NULL),
      menuItem("About Us", tabName = "aboutus", icon = icon("th")),
      menuItem("Enquiry", tabName = "chat", icon = icon("th")))),#Sidebar Closs

    
    body <- dashboardBody(shinyjs::useShinyjs(),
       tabItems(
         tabItem(tabName = "dashboard",
      box(
        solidHeader = FALSE,
        title = "Looking For",background = NULL, width = 12,status = "danger",
        footer = fluidRow(
          column(width = 6,
                 descriptionBlock(
                  number = HTML("<a href='http://3.93.125.130'>JBrowse+Search</a>"),
                  number_color = "yellow",number_icon = "fas fa-align-justify",
                  header = HTML("<a href='http://3.93.125.130'>Biologist Version</a>"),
                  text = "Easy and Simple",
                  right_border = TRUE,
                  margin_bottom = FALSE)
              ),
          column(width = 6,
            descriptionBlock(
              number = HTML("<a href='http://3.93.125.130'>All the Capabalities</a>"), 
              number_color = "black", 
              number_icon = "fa fa-arrows-alt",
              header = HTML("<a href='http://3.93.125.130'>Data Analyst Version</a>"), 
              text = "Complex & Huge", 
              right_border = TRUE,
              margin_bottom = TRUE
            )#block2
          )#column2
        )#fluidrow
      ),#box
          box(width = 12,fluidRow(
            column(width = 4,
                   selectInput(inputId = 'searchby',label = "Search By",choices = c("Gene Name","Gene Function","Start Site","Pfam","InterPro","GO"))),#column1
            column(width = 4,
                      textInput("querysearch","Query", placeholder = "hydroxyalse, t1.221, PF228800")),#column2
            column(width = 4,title = "Search Database", status = "primary",collapsible = TRUE,HTML("<br>"),actionButton("submit","SUBMIT"))#column3
            )),#box
      box(width = 12,fluidRow(
        column(width = 12,DT::dataTableOutput("queryresult"))
        
      ))),#box2
      tabItem(tabName = "shinyblast",
              fluidRow(infoBox("Step 1","Insert Query"),
                infoBox("Step 2","Select Program - blastn or blastp"),
                infoBox("Step 3","BLAST")
              ),
              fluidRow(box(title = NULL, status = "info", solidHeader = FALSE, width = 12,
                    collapsible = TRUE,textAreaInput('query', 'Input sequence:', value = "", placeholder = "", height="200px"))),
              fluidRow(box(width = 3,status = "info",selectInput("db", "Database:", choices=c(custom_db))),
                      (box(width = 3,status = "danger",selectInput("program", "Program:", choices=c("blastn","blastp","tblastn","blastx")))),
                      (box(width = 3,status = "warning",selectInput("eval", "e-value:", choices=c(1,0.001,1e-4,1e-5,1e-10)))),
                      (box(width = 3,status = "success",HTML("</br>"),actionButton("blast", "BLAST"),HTML("</br>"),HTML("</br>")))),
              #  textInput("text","Label",value="")
              fluidRow(box(width = 12,status = "success",title = "Blast Result",DT::dataTableOutput("blastResults"))),
              fluidRow(box(width = 12,status = "info",title = "Detailed Result",DT::dataTableOutput("clicked"))),
              fluidRow(box(width = 12,status = "warning",title = "Show Alignment",verbatimTextOutput("alignment"))),
              
              
              #this snippet generates a progress indicator for long BLASTs
              # div(class = "busy",
              #     p("Calculation in progress.."),
              #     img(src="https://i.stack.imgur.com/8puiO.gif", height = 100, width = 100,align = "center")
              # ), #Basic results output
       ),
      
      
      tabItem(tabName = "tool",shinyjs::useShinyjs(),
             box(width = 12,title = "Sequence Search",footer = "This will help to get sequence from Genome",status = "danger",solidHeader = TRUE,
                 collapsible = TRUE,
                 fluidRow(
                box(width = 3,selectInput("chr",label = "Select Chromosome",choices = c("chr2","chr3","chrX"))),
                box(width = 3,numericInput("start","START",value = 1,min = 1)),
                box(width = 3,numericInput("end",label = "END",value = 2,min = 2)),
                box(width = 3,HTML("</br>"),actionButton("getseq",label = "Fetch"))
                ),
              fluidRow(box(width = 12,title = "Feteched Sequence",status = "success",textOutput("chrseq"))),
              fluidRow(box(width = 12,title="Complementary Sequence",status = "success",textOutput("chrseqcomp"))),
              fluidRow(box(width = 12,title="Reverse Complementry Sequence",status = "success",textOutput("chrseqrevcom"))),
              
              )),
      tabItem(tabName = "chat",
              
              includeCSS("shinychat.css"),includeScript("sendOnEnter.js"),
              
              fluidRow(box(width = 8,title="Chat Room",uiOutput("chat")),
                       box(width = 4,title="To help you please enter your name", textInput("user", "Your User Name:", value=""),
                           tags$hr(), h5("Connected Users"),# Create a spot for a dynamic UI containing the list of users.
                           uiOutput("userList"))),
              fluidRow(box(width = 6,title="Type Text Here",textInput("entry", "")),
                       box(width = 6,title="To Send your message click Send button",HTML("</br>"),actionButton("send", "Send")))
              
              # div(
              #   # Setup custom Bootstrap elements here to define a new layout
              #   class = "container-fluid", 
              #   div(class = "row-fluid",
              #       # Set the page title
              #       tags$head(tags$title("ShinyChat")),
              #       
              #       # Create the header
              #       div(class="span6", style="padding: 10px 0px;",
              #           h1("ShinyChat"), 
              #           h4("Hipper than IRC...")
              #       ), div(class="span6", id="play-nice",
              #              "IP Addresses are logged... be a decent human being."
              #       ))),
              # div(
                # class = "row-fluid", 
                # mainPanel(
                  # Create a spot for a dynamic UI containing the chat contents.
                  # uiOutput("chat"),
                  # fluidRow(box(width = 12,title="Reverse Complementry Sequence",status = "success",textInput("entry", ""))),
                  # fluidRow(box(width = 12,title="Reverse Complementry Sequence",status = "success",actionButton("send", "Send"))),
                  # Create the bottom bar to allow users to chat.
                  # fluidRow(
                  #   div(class="span10",
                  #       
                  #   ),
                #     div(class="span2 center",
                #         actionButton("send", "Send")
                #     )
                #   )
                # ),
                # The right sidebar
                # sidebarPanel(
                #   # Let the user define his/her own ID
                #   textInput("user", "Your User ID:", value=""),
                #   tags$hr(),
                #   h5("Connected Users"),
                #   # Create a spot for a dynamic UI containing the list of users.
                #   uiOutput("userList"),
                #   tags$hr(),
                #   helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.<p>Source code available <a href =\"https://github.com/trestletech/ShinyChat\">on GitHub</a>."))
                # )
              ),
      tabItem(tabName = "jbrowse",
              fluidRow(column(12,  htmlOutput("jb_frame")))),
      tabItem(tabName = "seqserver",
              fluidRow(column(12,  htmlOutput("seqserver_frame")))),
      tabItem(tabName = "download",
              fluidRow(column(12,  htmlOutput("download_frame"))))

      )),#dashboard 
    
    dashboardFooter(
      left_text = "Database is maintained By IBAB",
      right_text = "Insititue of Bioinformatics & Applied Biotechnolgy, 2020"
    ),#footer
    
    rightsidebar = rightSidebar(
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Tab 1",
        icon = "desktop",
        active = TRUE,
        sliderInput(
          "obs",
          "Number of observations:",
          min = 0, max = 1000, value = 500
        )
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Tab 2",
        textInput("caption", "Caption", "Data Summary")
      ),
      rightSidebarTabContent(
        id = 3,
        icon = "paint-brush",
        title = "Tab 3",
        numericInput("obs", "Observations:", 10, min = 1, max = 100)
      )
    ),
    title = "IBAB"
  )

#“blue”, “black”, “purple”, “green”, “red”, “yellow”
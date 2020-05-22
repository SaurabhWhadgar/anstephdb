library(dplyr)
library(DT)
require(XML)
library(plyr)
library(dplyr)
library(sodium)
library(Biostrings)
library(stringr)




vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to Shiny Chat!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

server = function(input, output,session) {
  loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                   wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                       style = "text-align: center;",
                       actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                       shinyjs::hidden(
                         div(id = "nomatch",
                             tags$p("Oops! Incorrect username or password!",
                                    style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                    class = "text-center"))),
                       br(),
                       br(),
                       tags$code("Username: myuser  Password: mypass"),
                       br(),
                       tags$code("Username: myuser1  Password: mypass1")
                     ))
  )
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItem(tabName ="download", class = "active",
              fluidRow(
                box(width = 12, dataTableOutput('results'))
              ))
    }
    else {
      loginpage
    }
  })
  
  credentials = data.frame(
    username_id = c("myuser", "myuser1"),
    passod   = sapply(c("mypass", "mypass1"),password_store),
    permission  = c("basic", "advanced"), 
    stringsAsFactors = F
  )
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$something <- renderText({
    if (USER$login == TRUE ) {
      renderText({"You have selected this"})
    }
    else {
      loginpage
    }
  })
  
  #custom_db <- c("Ansteph Genome Version 1","Ansteph Genome Version 2")
  tbl <- read.csv("./from_python", header=FALSE, sep="\t")
  colnames(tbl) <- c("Chromosome","JBrowse/FASTA","Start","End","GeneInfo","Pfam","GO","Sequence")
  
  queryres <- eventReactive(input$submit,{
    if(input$searchby == "Gene Function" || input$searchby=='Gene Name'){
          tbl[grep(input$querysearch,tbl$GeneInfo,ignore.case = T),1:5]}
   else if(input$searchby == 'Pfam'|| input$searchby=='InterPro'){
    tbl[grep(input$querysearch,tbl$Pfam),1:5]}
  })
  
  output$queryresult <- DT::renderDataTable(queryres(),server = TRUE,filter = 'top',options = list(scrollX = TRUE),escape=FALSE)
  
  custom_db <- custom_db <- c("Ansteph Genome","Ansteph Protein")
  custom_db_path <- c("/mnt/250gb/tigs/anstephdb/jbrowse2/data/refseq/anstep_genomev2.fasta",
                      "/mnt/250gb/shiny-server/apps/db/anstepV2_augproteins.fasta")
  
  blastresults <- eventReactive(input$blast, {
    
    #gather input and set up temp file
    query <- input$query
    tmp <- tempfile(fileext = ".fa")
    
    #if else chooses the right database
    #if (input$db == "Ansteph Genome Version 1"){
    #  db <- c("../db/asteph.fa")
    #  remote <- c("")
    #}
    if(input$db == "Ansteph Genome"){
      db <- custom_db_path[1]
     # remote <- c("../db/anstep_genomev2.fasta")
    }
    if(input$db == "Ansteph Protein"){
      db <- custom_db_path[2]
      # remote <- c("../db/anstep_genomev2.fasta")
    }
    
    
    # }ifelse {
    #   db <- c("nr")
    #   #add remote option for nr since we don't have a local copy
    #   remote <- c("-remote")
    # }
    
    #this makes sure the fasta is formatted properly
    if (startsWith(query, ">")){
      writeLines(query, tmp)
    } else {
      writeLines(paste0(">Query\n",query), tmp)
    }
    
    #calls the blast
      data <- system(paste0("/mnt/250gb/blast/ncbi-blast-2.9/ncbi-blast-2.9.0+/bin/",input$program," -query ",tmp," -db ",db," -evalue ",input$eval," -outfmt 5 -max_hsps 1 -max_target_seqs 10 "), intern = T)
    xmlParse(data)
  }, ignoreNULL= T)
  
  #Now to parse the results...
  parsedresults <- reactive({
    if (is.null(blastresults())){}
    else {
      xmltop = xmlRoot(blastresults())
      
      #the first chunk is for multi-fastas
      results <- xpathApply(blastresults(), '//Iteration',function(row){
        query_ID <- getNodeSet(row, 'Iteration_query-def') %>% sapply(., xmlValue)
        hit_IDs <- getNodeSet(row, 'Iteration_hits//Hit//Hit_def') %>% sapply(., xmlValue)
        hit_length <- getNodeSet(row, 'Iteration_hits//Hit//Hit_len') %>% sapply(., xmlValue)
        
        hit_from <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hit-from') %>% sapply(., xmlValue)
        hit_to <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hit-to') %>% sapply(., xmlValue)
        
        bitscore <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_bit-score') %>% sapply(., xmlValue)
        eval <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_evalue') %>% sapply(., xmlValue)
        if(input$db=="Ansteph Genome Version 1"){
          jblink = "<a href=http://3.93.125.130/jbrowse/?loc="
          jbtrack="DNA%2CGENE_CANVAS"
        }else{
          jblink = "<a href=http://3.93.125.130/tigs/anstephdb/jbrowse2/?loc="
          jbtrack="DNA%2CUCI_MAKER"
        }
        jbrowse <- paste0(jblink,hit_IDs,":",as.numeric(hit_from)-1000,"..",as.numeric(hit_to)+1000,
                          "&tracks=",jbtrack,"&highlight=",hit_IDs,":",hit_from,"..",hit_to,">","JBrowse","</a>")
        
        #&tracks=DNA%2CGENE_CANVAS&highlight=
        # <a href='",mydata$url,"'>",mydata$url,"</a>"
        
        cbind(query_ID,hit_IDs,hit_length,hit_from,hit_to,bitscore,eval,jbrowse)
      })
      #this ensures that NAs get added for no hits
      results <-  rbind.fill(lapply(results,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
    }
  })
  
  #makes the datatable
  output$blastResults <- DT::renderDataTable({
    if (is.null(blastresults())){
    } else {
      parsedresults()
    }
  },escape=FALSE,server = TRUE)
  
  #this chunk gets the alignemnt information from a clicked row
  output$clicked <- DT::renderDataTable({
    if(is.null(input$blastResults_rows_selected)){}
    else{
      xmltop = xmlRoot(blastresults())
      clicked = input$blastResults_rows_selected
      tableout<- data.frame(parsedresults()[clicked,])
      
      tableout <- t(tableout)
      names(tableout) <- c("")
      rownames(tableout) <- c("Query ID","Hit ID", "Length", "From", "To", "Bit Score", "e-value","JBrowse")
      colnames(tableout) <- NULL
      data.frame(tableout)
    }
  },escape=FALSE,rownames =T,colnames =F)
  
  #this chunk makes the alignments for clicked rows
  output$alignment <- renderText({
    if(is.null(input$blastResults_rows_selected)){}
    else{
      xmltop = xmlRoot(blastresults())
      
      clicked = input$blastResults_rows_selected
      
      #loop over the xml to get the alignments
      align <- xpathApply(blastresults(), '//Iteration',function(row){
        top <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_qseq') %>% sapply(., xmlValue)
        mid <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_midline') %>% sapply(., xmlValue)
        bottom <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hseq') %>% sapply(., xmlValue)
        rbind(top,mid,bottom)
      })
      
      #split the alignments every 40 carachters to get a "wrapped look"
      alignx <- do.call("cbind", align)
      splits <- strsplit(gsub("(.{40})", "\\1,", alignx[1:3,clicked]),",")
      
      #paste them together with returns '\n' on the breaks
      split_out <- lapply(1:length(splits[[1]]),function(i){
        rbind(paste0("Q-",splits[[1]][i],"\n"),paste0("M-",splits[[2]][i],"\n"),paste0("H-",splits[[3]][i],"\n"))
      })
      unlist(split_out)
    }
  })
  
  fetchedseq <- eventReactive(input$getseq,{
    system(paste0("samtools faidx ../db/anstep_genomev2.fasta ",input$chr,":",input$start,"-",input$end),
           intern = TRUE)
         #  ignore.stdout = TRUE,
          # ignore.stderr = FALSE,
         #  wait = TRUE,
         #  input = NULL, 
         #  show.output.on.console = TRUE)
  })
  
  output$chrseq <- renderText({
    fetchedseq()[2]
    })
  
  output$chrseqcomp<- renderText({
    chartr("ATGC","TACG", fetchedseq()[2])
  })
  
  output$chrseqrevcom<- renderText({
    chartr("ATGC","TACG",reverse(fetchedseq()[2]))
  })
  
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE
  
  # When a session is ended, remove the user and note that they left the room. 
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
    })
  })
  
  # Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Note the change in the chat log
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-change",
                                                    paste0("\"", sessionVars$username, "\""),
                                                    " -> ",
                                                    paste0("\"", input$user, "\""))))
        
        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user", 
                    value=sessionVars$username)    
  })
  
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })
  
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
  
  
  output$jb_frame <- renderUI({
    my_test1 <- tags$iframe(src="http://3.93.125.130/tigs/anstephdb/jbrowse2/?loc=chr2%3A37471337..54414233&tracks=DNA&highlight=",height=600, width="100%",frameBorder="0")
    #print(my_test)
    my_test1
  })
  
  output$seqserver_frame <- renderUI({
    my_test2 <- tags$iframe(src="http://3.93.125.130/subapp/", height=600, width="100%",frameBorder="0")
   # print(my_test)
   my_test2
  })
  
    output$download_frame <- renderUI({
    my_test2 <- tags$iframe(src=" http://3.93.125.130/tigs/anstephdb/page2.html", height=600, width="100%",frameBorder="0")
   # print(my_test)
   my_test2
  })

 
}






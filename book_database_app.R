# Load packages ---------------------------------------------------------------------------------------------------


# Deploy app to shinyapps.io --------------------------------------------------------------------------------------

# rsconnect::setAccountInfo(name='',
#                           token='',
#                           secret='')

# START OF UI -----------------------------------------------------------------------------------------------------

ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$style(".book1 {color:#097969; font-size:27px}"),
    tags$style(".book2 {color:#00FF00; font-size:27px}"),
    
    bootstrapPage("",
      navbarPage(title = div(style="margin-top: 0%; margin-right:-5%; padding-right: 10px; display: inline-block",
                             "Book database", icon("book-open", "fa-1.5x",lib = "font-awesome")),
                 #Home panel####
                 tabPanel("Home",
                          #titlePanel(windowTitle = "My book db"),
                          mainPanel(style = "margin-bottom:50px; margin-top:75px ",
                                    id = "home", width = 12,
                                    br(),
                                    fluidRow(column(width = 8, offset = 2, align = "center",
                                                    uiOutput("home_books"))),
                                    br(),
                                    br(),
                                    titlePanel(h1("Your latest additions", align = "center")),
                                    #br(),
                                    lapply(1:10, function(i) {
                                      fluidRow(
                                        style='padding:10px;',
                                        column(width = 6, offset = 3,
                                               style='border: 2px solid black; border-radius: 5px',
                                               uiOutput(paste0("latest_",i)))
                                      )
                                    })
                          )
                          ),
                 
                 
                 #View books panel####
                 tabPanel("View books",
                          titlePanel(title = "",
                                     windowTitle = "my b db"),
                          mainPanel(style = "margin-bottom:50px; margin-top:85px; margin-left: 0%",
                                  width = 12,
                                  DT::DTOutput(outputId = 'allbooks'))),
                 
                 
                 #Add book panel ####
                 tabPanel("Add book",
                          mainPanel(style = "margin-bottom:50px; margin-top:75px; margin-left: 0%;",
                                    id = "add_book_form",
                                    h1("Fill in the form and press 'add book'"),
                                    fluidRow(
                                      column(6, 
                                             textInput(inputId = 'title', label = 'Title:',
                                                  value = '')),
                                      column(6, 
                                             textInput(inputId = 'og_title', label = 'Original title:',
                                                  value = ''))),
                                    checkboxInput(inputId = "same_title", 
                                                  label = "If the title and the original title are equal, check this box"),
                                    fluidRow(
                                      column(6,
                                             selectizeInput(inputId = "author", label = "Author",
                                                            choices = c(""), selected = "",
                                                            options=list(create=TRUE))),
                                      column(6,
                                             selectizeInput(inputId = 'language', label = "Language",
                                                         choices = c("","Spanish", "English", "Italian", "French", 
                                                                     "German", "Galician", "Russian"),
                                                         selected = "",
                                                         options = list(create=TRUE)))),
                                    helpText('If there are 2+ authors, separate using the & symbol:'),
                                    fluidRow(
                                      column(6,
                                             selectizeInput(inputId = "editorial", label = "Publisher:",
                                                            choices = c(""), selected = "",
                                                            options=list(create=TRUE))),
                                      column(6,
                                             textInput(inputId = 'isbn', label = 'ISBN:',
                                                       value = ''))),
                                    fluidRow(
                                      column(6,
                                             radioButtons(inputId = 'owner', label = 'Property of:',
                                                          choices = c('Común','Javier', 'Pablo'), 
                                                          selected = "Común"
                                                          )),
                                      column(6,
                                             selectizeInput(inputId = 'location', label = 'Where is it?',
                                                         choices = c(''), selected = '',
                                                         options=list(create=TRUE)))),
                                    
                                    #type and genre
                                    fluidRow(
                                      column(6,
                                             radioButtons(inputId = 'type', label = 'Type of book:',
                                                          choices = c('Reading book', 'Textbook'),
                                                          selected = NULL)),
                                      column(6,
                                             selectizeInput(inputId = 'genre', label = 'Genre:?',
                                                            choices = c(''),selected = '',
                                                            options=list(create=TRUE)))),
                                        
                                    #brief summary
                                    textAreaInput(inputId = "summary", 
                                                   "Describe the book in one snetence:", rows = 3),
                                    
                                    #preview of added book
                                    helpText('Below you will find a summary of all categories filled.'),
                                    tableOutput('added_book_table'),
                                    tags$hr(), #simple horizontal line
                                    
                                    #add book
                                    textInput('password', 'Password:', value = ''),
                                    div(style = "display:inline-block;",
                                        actionButton(style = "background-color:	#1E90FF",
                                                     inputId = 'add_book', label = 'Add book')),
                                    
                                    #message that the book has been added or not
                                    div(style = "display:inline-block; margin-left: 20px;",
                                        textOutput('added_book_message')))),
                 
                 #Stats panel####
                 tabPanel(title = div("Stats!", icon("bar-chart-o")),
                          titlePanel("Stat That!"),
                          mainPanel(style = "margin-bottom:50px; margin-top:75px",
                                    width = 12,
                                    fluidRow(
                                      style = 'font-weight: bold; font-size:300%; text-align: center', 
                                      textOutput(outputId = "stats_text1")
                                    ),
                                    br(),
                                    fluidRow(
                                      #left column
                                      column(3, offset = 2, 
                                             fluidRow(
                                               style ='padding:1px; background-color: #000080; border-radius: 25px',
                                               h4("Whose are the books?", align = "center", 
                                                  style = 'font-weight: bold; color: #FFFFFF'),
                                               gt::gt_output(outputId = 'stats_owner')),
                                             br(),
                                             fluidRow(
                                               style='padding:1px; background-color: #000080; border-radius: 25px',
                                               h4("Where are the books?", align = "center", 
                                                  style = 'font-weight: bold; color: #FFFFFF'),
                                               gt::gt_output(outputId = 'stats_location')),
                                             br(),
                                             fluidRow(
                                               style='padding:1px; background-color: #000080; border-radius: 25px',
                                               h4("In which language?", align = "center", 
                                                  style = 'font-weight: bold; color: #FFFFFF'),
                                               gt::gt_output(outputId = 'stats_language'))
                                      ),#column end
                                      
                                      #right column
                                      column(3, offset = 2, 
                                             fluidRow(
                                               style='padding:1px; background-color: #000080; border-radius: 25px',
                                               h4("Authors", align = "center", 
                                                  style = 'font-weight: bold; color: #FFFFFF'),
                                               gt::gt_output(outputId = 'stats_author')),
                                             br(),
                                             fluidRow(
                                               style='padding:1px; background-color: #000080; border-radius: 25px',
                                               h4("Most common genres", align = "center", 
                                                  style = 'font-weight: bold; color: #FFFFFF'),
                                               gt::gt_output(outputId = 'stats_genre')),
                                             br(),
                                             fluidRow(
                                               style='padding:1px; background-color: #000080; border-radius: 25px',
                                               h4("What type of books?", align = "center", 
                                                  style = 'font-weight: bold; color: #FFFFFF'),
                                               gt::gt_output(outputId = 'stats_type'))
                                      ) #column end
                                   ) #fluid row end
                                    
                                    
                          ) #mainpanel end
                ),#tabpanel end
                
                #Ch Loc panel
                tabPanel("Chamge location",
                         mainPanel(style = "margin-bottom:80px; margin-top:75px; margin-left: 0%;",
                                   id = "ch_loc_form",
                                   fluidRow(width = 12,
                                            column(6,
                                                   selectizeInput(inputId = "ch_loc_title", 
                                                                  label = "Choose book",
                                                                  choices = c(""), selected = "",
                                                                  options=list(create=TRUE)))),
                                   fluidRow(width = 12,
                                            column(width = 6,
                                                   tableOutput(outputId = "ch_loc_table_book_chosen"))),
                                   fluidRow(width = 12,
                                            column(width = 6,
                                                   textInput(inputId = "ch_loc_bookid", label = "Introduce ID of book (e.g., 'id_xx')",
                                                             value = "id_")),
                                            column(6,
                                                   selectizeInput(inputId = "ch_loc_new", label= "Choose new location",
                                                                  choices = c(""), selected = "",
                                                                  options = list(create=TRUE)))),
                                   #edit
                                   fluidRow(width = 12,
                                            column(width = 6,
                                                   textInput('ch_loc_password', 'Password:', value = ''))),
                                   fluidRow(width = 12,
                                            column(width = 6,
                                                   actionButton(style = "background-color:	#1E90FF",
                                                                inputId = 'edit_book', label = 'Change location')
                                            ))
                                            
                                   
                                   
                                   )#end of main panel
                         ),#end of tab panel
                 inverse = T,
                 position = c("fixed-top")), #navbarpage end
      
      
      #change style of navegation bar####
      tags$style(type = 'text/css', '.navbar { font-weight: bold; font-size: 20px; padding: 10px }')
     
      

) #bootstrappage end
) #fluidpage end

# END OF UI ----------------------------------------------------------------------------------------------------------














# START OF SERVER -------------------------------------------------------------------------------------------------


server <- function(input, output) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(shinytitle)
  library(tidyverse)
  library(stringr)
  library(gt)
  library(gtExtras)
  library(DT)
  library(googlesheets4)
  library(rsconnect)


  
# get data --------------------------------------------------------------------------------------------------------
  #get data from a google spreadsheet that is in a google drive account
  
  
  # Set authentication token to be stored in a folder called `.secrets`
  options(gargle_oauth_cache = ".secrets")
  # Authenticate manually
  gs4_auth()
  # If successful, the previous step stores a token file.
  # Check that a file has been created with:
  list.files(".secrets/")
  # Check that the non-interactive authentication works by first deauthorizing:
  gs4_deauth()
  # Authenticate using token. If no browser opens, the authentication works.
  gs4_auth(cache = ".secrets", email = "") 
  
  books <- read_sheet("") #here write code of the sheet.

  #get data to reactive
  values <- reactiveValues()
  values$df <- books
  

# add new book and display database-------------------------------------------------------------------------

     #adds book to df####
     addData <- observe({
       if(input$add_book > 0){
          isolate(if(input$password == "aaa"){
            #gather info from inputs
            input$add_book
            newLine = isolate(c(input$title, 
                                ifelse(input$same_title > 0, input$title, input$og_title),  
                                input$author, input$language , input$editorial, input$owner, input$isbn, 
                                str_to_title(input$location), 
                                input$type, input$genre,
                                input$summary, Sys.Date(),
                                paste0("id_", nrow(books)+1)))
            #add info to db
            isolate(values$df <- rbind(as.data.frame(values$df), unlist(newLine)))
            books <<- values$df
            #save new db with new book
            write_sheet(books, ss = "1r7oyQTwVckAoBhqCglPiWop_jsmyVCvINkjQDe32-qU", sheet = "books")
            #clean add book form after a book has been added
            reset('add_book_form')
          })
       }
     })
     
     #display book database####
     output$allbooks = DT::renderDataTable ({
       datatable(values$df %>% 
                   #mutate(date = as.Date(date, origin = "1970-01-01"))%>% 
                   arrange(title) %>% 
                   select(title, ogtitle, author, type, genre, location, owner, language,
                          publisher, ISBN, summary, date, bookid) %>% 
                   rename(Title = title, `Original title` = ogtitle,
                          Author = author, Type = type, Genre = genre, 
                          Publisher = publisher, Language = language,
                          Loaction = location, Owner = owner, ISBN = ISBN, Summary = summary, `Added on` = date, BookID = bookid),
                 options = list(pageLenght = 50,
                                lengthMenu = c(50,100),
                                autoWidth = TRUE,
                                columnDefs = list(list(width = '300px', targets = c(10))),
                                scrollX = TRUE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}")),
                 selection = 'none',
                 filter = 'bottom',
                 rownames = FALSE)
       })
  
    # preview table of the categories of the book to be added####
    output$added_book_table = renderTable(
      data.frame(Title = input$title, 
                 `Original title` = ifelse(input$same_title > 0, input$title, input$og_title),
                 Author = input$author, Language = input$language,
                 ISBN = input$isbn,
                 Editorial = input$editorial,
                 Owner = input$owner,
                 Location = input$location,
                 Type = input$type,
                 Genre = input$genre
                 ) #data.frame
    )

  
  
  
    
    #message that the book has been added####
    addbookmessage = eventReactive(input$add_book, {
      if_else(input$password == 'aaa', 
              'Your book has been succesfully added.',
              'Password is incorrect. Please try again.')
    })
    output$added_book_message = renderText({
      addbookmessage()
    })
  
    
    
    

# Selectinput choices update for adding book --------------------------------------------------------------------------------------
    #author
    observe({
      authors <- sort(unique(books$author))
      updateSelectizeInput(inputId = "author", label = "Author:",
                           choices = c("", authors),
                           options=list(create=TRUE))

    })

    #publisher
    observe({
      publishers <- sort(unique(books$publisher))
      updateSelectizeInput(inputId = "editorial", label = "Publisher:",
                           choices = c("", publishers),
                           options=list(create=TRUE))
      
    })
    
    #location
    observe({
      locations <- unique(books$location)
      updateSelectizeInput(inputId = "location",
                        label = "Where is it?",
                        choices = c("", locations))
    })    
      
    #genre
    observe({
      if(input$type == "Textbook"){
        genres <- books %>% filter(type == "Textbook") %>%  {unique({.$genre})}
      } else{
        genres <- books %>% filter(type == "Reading book") %>%  {unique({.$genre})}
        }
      updateSelectizeInput(inputId = "genre",
                        label = "Genre:",
                        choices = c("", genres))
    })    
  
    
    
    
# Home panel server -----------------------------------------------------------------------------------------------
  
  #book icons display
    output$home_books = renderUI({
      div(replicate(nrow(subset(books, owner == "Pablo")), icon("book", class = "book1"), simplify=FALSE),
          replicate(nrow(subset(books, owner == "Javier")), icon("book", class = "book2"), simplify = FALSE))
    })

  #latest books added
    lapply(1:10, function(i) {
      output[[paste0('latest_', i)]] <- renderUI({
        tagList(tags$h3(books$title[nrow(books) - (i - 1)], align = "center"),
                tags$h4(books$author[nrow(books)- (i - 1)], align = "center"),
                tags$h5(ifelse(!is.na(books$summary[nrow(books) - (i - 1)]),
                               paste0('"', books$summary[nrow(books) - (i - 1)], '"'),
                               ""), 
                        align = "center"))
      })
    })

    
    

# Stats! server ---------------------------------------------------------------------------------------------------

    #initial text - number of books
    output$stats_text1 = renderText(
       paste0("You have ", nrow(books)," books")
    )
    
    #owner table
    output$stats_owner = render_gt(
      books %>% 
        group_by(owner) %>% 
        summarise(n = n()) %>% 
        arrange(-n) %>% 
        gt() %>% 
        cols_align(align = "center", columns = c("n")) %>% 
        tab_options(column_labels.hidden = TRUE,
                    table.background.color = "#000080",
                    table.font.color = "#FFFFFF") %>% 
        opt_table_lines(extent = "none")
    )
    
    #location table
    output$stats_location = render_gt(
      books %>% 
        group_by(location) %>% 
        summarise(n = n()) %>% 
        arrange(-n) %>% 
        gt() %>% 
        cols_align(align = "center", columns = c("n")) %>% 
        tab_options(column_labels.hidden = TRUE,
                    table.background.color = "#000080",
                    table.font.color = "#FFFFFF",
                    row.striping.include_table_body =TRUE)%>% 
        opt_table_lines(extent = "none")
    )
    
    #languages table
    output$stats_language = render_gt(
      books %>% 
        group_by(language) %>% 
        summarise(n = n()) %>% 
        arrange(-n) %>% 
        drop_na() %>% 
        select(language,n) %>% 
        gt() %>% 
        #gtExtras::gt_img_rows(columns = flag, img_source = "web") %>% 
        tab_options(column_labels.hidden = TRUE,
                    table.background.color = "#000080",
                    table.font.color = "#FFFFFF")%>% 
        opt_table_lines(extent = "none")
    )
    
    #authors table
    output$stats_author = render_gt(
      books %>% 
        group_by(author) %>% 
        summarise(n = n()) %>% 
        arrange(-n) %>% 
        slice(1:5) %>% 
        gt() %>% 
        tab_options(column_labels.hidden = TRUE,
                    table.background.color = "#000080",
                    table.font.color = "#FFFFFF")%>% 
        opt_table_lines(extent = "none")
    )
    
    #genre table
    output$stats_genre = render_gt(
      books %>% 
        group_by(genre) %>% 
        summarise(n = n()) %>% 
        arrange(-n) %>% 
        slice(1:5) %>% 
        gt() %>% 
        tab_options(column_labels.hidden = TRUE,
                    table.background.color = "#000080",
                    table.font.color = "#FFFFFF")%>% 
        opt_table_lines(extent = "none")
    )
    
    #type table
    output$stats_type = render_gt(
      books %>% 
        group_by(type) %>% 
        summarise(n = n()) %>% 
        arrange(-n) %>% 
        gt() %>% 
        tab_options(column_labels.hidden = TRUE,
                    table.background.color = "#000080",
                    table.font.color = "#FFFFFF")%>% 
        opt_table_lines(extent = "none")
    )
    


# Change location server ------------------------------------------------------------------------------------------

    
    observe({
      titles <- sort(unique(books$title))
      updateSelectizeInput(inputId = "ch_loc_title" ,
                           label = "Choose book",
                           choices = c("", titles))
    })  
    
    output$ch_loc_table_book_chosen = renderTable(
      books %>% 
        filter(title == input$ch_loc_title) %>% 
        dplyr::select(title, location, bookid, owner) %>% 
        rename(Title = title, Loaction = location, BookID=bookid, Owner = owner)
    )
    
    observe({
      locations <- unique(books$location)
      updateSelectizeInput(inputId = "ch_loc_new" ,
                           label = "Choose new location",
                           choices = c("", locations))
    })  
    
    
  #actually change the location of the book
    ch_loc <- observe({
      if(input$edit_book > 0){
        isolate(if(input$ch_loc_password == "aaa"){
          #gather info from inputs
          isolate(books[books$bookid == input$ch_loc_bookid, "location"] <- input$ch_loc_new)
          books <<- values$df
          #save new db with new book, 
          write_sheet(books, ss = "1r7oyQTwVckAoBhqCglPiWop_jsmyVCvINkjQDe32-qU", sheet = "books")
          #clean add book form after a book has been added
          reset('ch_loc_form')
        })
      }
    })
 
} #END OF SERVER




# Run the app ####
shinyApp(ui = ui, server = server)


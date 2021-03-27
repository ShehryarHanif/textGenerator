# --------------------------
# load libraries
# --------------------------
library(shiny)
library(rdrop2)
library(dplyr)
library(shiny.semantic)
library(purrr)
library(magrittr)
library(data.table)

# --------------------------
# load data and functions
# --------------------------
task_data <- readRDS("data/task_data.rds")
training_data <- readRDS("data/training_data.rds")
screening_data <- readRDS("data/screening_data.rds")
source("SemanticFluencyTask.R", local = TRUE)

# set timer precision
options(digits.secs = 6)
time <- 3
compensation <- "3.00"
min_words <- 10

# --------------------------
# data saving
# --------------------------
outputDir <- "GitHub/large_data/shinySFT/output"   # directory to save data
droptoken <- readRDS("----INSERT AUTH TOKEN HERE----")   # reads in authentication for dropbox (must be store in same folder as this code)
# check out https://github.com/karthik/rdrop2 to see how to generate your auth tokens and link to your dropbox

# --------------------------
# java scripts
# --------------------------
# js to register when keyboard is used
# Shiny.onInputChange only reacts when the js object it references changes
# to solve this we add a random number generator
# see: https://stackoverflow.com/questions/35831811/register-repeated-keyboard-presses-in-shiny
keyboard <- ' $(document).on("keydown", function (e) {
Shiny.onInputChange("lastkeypresscode", [e.which, Math.random()]); 
});
'

# --------------------------------
#
# USER INTERFACE
#
# --------------------------------
ui <- fluidPage(
  theme = "cosmo.css",   # css theme dowloaded from bootswatch.com (https://bootswatch.com/3/)
  tags$script(keyboard),     # load java scripts
  title = "Semantica",       # define title
  uiOutput("MainAction"),    # render function for dynamic ui
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}")   # prevents gray screen during Sys.sleep()
)

# --------------------------------
#
# SERVER FUNCTION
#
# --------------------------------
server <- function(input, output, session) {
  
  # ----------------------------
  # navigation - training cues
  # ----------------------------
  training.cues <- sample(training_data) # randomize training cues
  num_training <- length(training.cues)
  # InputID (from)
  num_training <- length(training.cues) # register number of cues
  navigator_training <- data.table("cue" = training.cues, "InputID" = c(paste0("training.fluency", seq(1:num_training))), "title" = c(paste0("Training ", seq(1:num_training), " of ", num_training)))                  
  # NextInputID (to)
  if(num_training < 2){navigator_training$nextInputID <- "ready"}else{
    navigator_training$nextInputID <- c(paste0("nextcue", 1:(num_training - 1)), "ready")}
  
  # ----------------------------
  # navigation - task cues
  # ----------------------------
  task.cues <- sample(task_data)   # randomize task cues
  # InputID (from)
  num_tasks <- length(task.cues) # register number of cues
  navigator_task <- data.table("cue" = task.cues, "InputID" = c(paste0("task.fluency", seq(1:num_tasks))), "title" = c(paste0("Task ", seq(1:num_tasks), " of ", num_tasks)))                  
  # NextInputID (to)
  if(num_tasks < 2){navigator_task$nextInputID <- "savedata"}else{
    navigator_task$nextInputID <- c(paste0("nextcue", 1:(num_tasks - 1)), "survey")}
  
  # ----------------------------
  # navigation - rbind
  # ----------------------------
  navigator <- rbind(navigator_training, navigator_task)
  
  # ----------------------------
  # screener tasks
  # ----------------------------
  # screener 1
  screener1 <- sample(names(screening_data[1:2]))
  screener1.left <- screening_data[[screener1[1]]]
  screener1.right <- screening_data[[screener1[2]]]
  # screener 2
  screener2 <- sample(names(screening_data[3:4]))
  screener2.left <- screening_data[[screener2[1]]]
  screener2.right <- screening_data[[screener2[2]]]
  # randomly sample correct answer
  screener.cue1 <- sample(names(screening_data[1:2]), 1)
  screener.cue2 <- sample(names(screening_data[3:4]), 1)
  # correct responses
  screener1.correct <- data.frame("screener1.left" = ifelse(screener.cue1 == screener1[1], TRUE, FALSE), "screener1.right" = ifelse(screener.cue1 !=screener1[1], TRUE, FALSE), stringsAsFactors = FALSE)
  screener2.correct <- data.frame("screener2.left" = ifelse(screener.cue2 == screener2[1], TRUE, FALSE), "screener2.right" = ifelse(screener.cue2 !=screener2[1], TRUE, FALSE), stringsAsFactors = FALSE)
  
  
  # --------------------------------
  #   define reactive values
  # --------------------------------
  
  # CurrentValues stores navigation related values
  CurrentValues <- reactiveValues(page = "welcome",
                                  errors = "none",
                                  task_index = 1)
  
  FluencyData <- reactiveValues()  # fluency task values
  RTStartData <- reactiveValues()  # time of first key press (for each token entry)
  RTEndData <- reactiveValues()    # time of last key press (for each token entry)
  RTCurrentStartData <- reactiveValues()  # necessary to compute intra/inter retrieval times
  EventTimeLog <- reactiveValues()  # stored current system time (necessary for timed tasks)

  # --------------------------------
  # send dynamic UI to ui
  # --------------------------------
  output$MainAction <- renderUI( {
    PageLayouts()
  })
  

  # --------------------------------
  # define page layouts
  # --------------------------------
  PageLayouts <- reactive({
    
    # --------------------------------
    # welcome page
    # --------------------------------
    if (CurrentValues$page == "welcome") {  # conditionl determining whether page is displayed
      
      # conditional: if ID not entered, ouput inputLabel text in red to remind user that he must enter an ID
      if (CurrentValues$errors == "Blank_Name") {
        inputLabel <- p(style = "color:Red", "Please enter your MTurk ID!")   
      } else {
        inputLabel <- p("Please enter your MTurk ID")
      }
      
      # page content
      # can use HTML (e.g. HTML("<br><br>")) or RShiny HTML tags (e.g. br()) 
      # see: https://shiny.rstudio.com/articles/tag-glossary.html
      return(
        list(
          br(),
          h1(span(strong("Context Words"), style="color:#2780e3")),   # title
          br(),
            mainPanel(    
          p(span(strong("Purpose:"), style="color:#2780e3"),"collect context words."),
          p(span(strong("Confidentiality:"), style="color:#2780e3"), "responses are anonymous, we have no way of linking the data to individual identities."),
          p(span(strong("Length:"), style="color:#2780e3"), "task takes on average 15 to 20 minutes to complete."),
          p(span(strong("Compensation:"), style="color:#2780e3"), paste0("$", compensation)),
          br(),
          p("If you consent to participate in this study, please enter your MTurk ID and press ''Start''.")),
          # main panel contents
          mainPanel(
            # text input control
          textInput(inputId = "workerid",   # control ID
                    label = inputLabel,     # label to appear on top of control (color conditioned above)
                    placeholder = "enter MTurk ID here"), # text to appear as placeholder inside control (an example of a unique ID)
          # action button to be pressed by user to continue
          actionButton(inputId = "consent",   # button ID
                       label = "Start",   # button label (text displayed to the user)
                       class = "btn btn-primary")   # css class (defines button color as specified in the css file)
          )
        )
      )}
    
    # --------------------------------
    # instructions 1
    # --------------------------------
    if (CurrentValues$page == "instructions1") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Context Words")), style="color:#2780e3"),
          p("A famous maxim in the study of linguistics states that:"),
          p(strong(em("You shall know a word by the company it keeps.")), "(Firth, 1957)"),
          p("This task is designed to help us understand the nature of the ''company'' that words ''keep'': that is, their CONTEXT. "),
          br(),
          p("Specifically, for a CUE WORD, its CONTEXT WORDS include words that:"),
          column(12,
                 wellPanel(
          tags$ul(
            tags$li("Tend to occur in the vicinity of the CUE WORD. That is, they are words that appear close to the CUE WORD in written or spoken language.")),
            p("AND/OR", align = "center"),
          tags$ul(
            tags$li("Tend to occur in similar situations to the CUE WORD in spoken and written language. That is, they are words that regularly appear with other words that are closely related to the CUE WORD.")))),
          br(),
          p("For example, CONTEXT WORDS for the cue word COFFEE include:"),
          tags$ol(
            tags$li(em("cup"), "(tends to occur in the vicinity of COFFEE)."), 
            tags$li(em("tea"), "(tends to occur in similar situations to COFFEE, for example when discussing drinks).")
          ),
          br(),
          p("Click ''Next'' to continue"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.instructions2",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
      )}
    
    # --------------------------------
    # instructions 2
    # --------------------------------
    if (CurrentValues$page == "instructions2") {   # conditionl determining whether page is displayed
      return(
        list(
          br(),
          span(h2(strong("Context Words")), style="color:#2780e3"),
          br(),
          p("To make sure you understand what context words are, in what follows you will be given a", strong("cue word"), "and two
            lists of", strong("context words"),". Please read both lists and select the list that best meets the above criteria for 
            the cue word provided.", strong("Keep in mind wrong answers in these screening tasks will end your participation in the task without remuneration.")),
          br(),
          p("Click ''Next'' to continue to task"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.screening1",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
      )}
    
    # --------------------------------
    # screening 1
    # --------------------------------
    if (CurrentValues$page == "screening1") {   # conditionl determining whether page is displayed

      return(
        list(
          fluidPage(
          br(),
          fluidRow(
            column(12,
                   h3(strong(toupper(screener.cue1))),
                   br(),
                     column(4,
                            wellPanel(
                            p(screener1.left[1],br(),screener1.left[2],br(),
                              screener1.left[3],br(),screener1.left[4],br(),
                              screener1.left[5],br(),screener1.left[6],br(),"")), 
                            checkboxInput(inputId = "screener1.left", ""),
                            offset = 1, align = "center"),
                     column(4,
                            wellPanel(
                            p(screener1.right[1],br(),screener1.right[2],br(),
                              screener1.right[3],br(),screener1.right[4],br(),
                              screener1.right[5],br(),screener1.right[6],br(),"")), 
                            checkboxInput(inputId = "screener1.right", ""),
                            offset = 2, align = "center"),
            align = "center")
          ),
          br(),
          p("Select the list with the best context words for the cue word provided by clicking on the respective checkbox below the list."),
          br(),
          br(),
          p("Click ''Next'' to continue"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.screening2",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br(),
          br()
        )
        )
      )}
    
    # --------------------------------
    # screening 2
    # --------------------------------
    if (CurrentValues$page == "screening2") {   # conditionl determining whether page is displayed

      return(
        list(
          fluidPage(
            br(),
            fluidRow(
              column(12,
                     h3(strong(toupper(screener.cue2))),
                     br(),
                     column(4,
                            wellPanel(
                              p(screener2.left[1],br(),screener2.left[2],br(),
                                screener2.left[3],br(),screener2.left[4],br(),
                                screener2.left[5],br(),screener2.left[6],br(),"")), 
                            checkboxInput(inputId = "screener2.left", ""),
                            offset = 1, align = "center"),
                     
                     column(4,
                            wellPanel(
                              p(screener2.right[1],br(),screener2.right[2],br(),
                                screener2.right[3],br(),screener2.right[4],br(),
                                screener2.right[5],br(),screener2.right[6],br(),"")), 
                            checkboxInput(inputId = "screener2.right", ""),
                            offset = 2, align = "center"),
                     align = "center")
            ),
            br(),
            p("Select the list with the best context words for the cue word provided by clicking on the respective checkbox below the list."),
            # action button to be pressed by user to continue
            br(),
            br(),
            p("Click ''Next'' to continue"),
            actionButton(inputId = "goto.instructions3",   # button ID 
                         label = "Next",   # button label (text displayed to the user) 
                         class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
            br(),
            br()
          )
        )
      )}
    
    # --------------------------------
    # instructions 3
    # --------------------------------
    if (CurrentValues$page == "instructions3") {
      
      return(
        list(
          br(),
          span(h2(strong("Task Description")), style="color:#2780e3"),
          br(),
          p("In what follows we want you to list ", strong("off the top of your head"), " context words for a series of cues."),
          br(),
          p(strong("We are especially interested in context words likely to appear in",  span("political discourse.", style="color: #ff0000"))),
          br(),
          p(paste0("For each iteration of the task (", num_tasks, " in total):")),
          br(),
          tags$ol(
            tags$li("You will be given a cue word."),
            br(),
            tags$li("Use the empty text box provided to input", strong(min_words, "context words"), "that come to mind."),
            br(),
            tags$li("Each time you input a word, press enter to have it saved and the input box cleared, then proceed to input the next context word."),
            br(),
            tags$li("It is important you press enter after you input each word in order to have it stored correctly."),
            br(),
            tags$li("Click the ''Next'' button once you have inputted at least", min_words, " context words for a given cue."),
            br(),
            tags$li("Once you click ''Next'', the next cue will immediately appear on the screen."),
            br(),
            tags$li("You will have a maximum of ", time," minutes per cue to list a minimum of ", min_words, " context words."),
            br(),
            tags$li(strong("You must click ''Next'' before the clock runs out"), "otherwise you will not be allowed to continue with the HIT.")
          ),
          br(),
          p("Click ''Next'' to continue"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.instructions4",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
      )}
    
    # --------------------------------
    # instructions 4
    # --------------------------------
    if (CurrentValues$page == "instructions4") {
      
      return(
        list(
          br(),
          span(h2(strong("Compensation")), style="color:#2780e3"),
          br(),
          p(paste0("To receive compensation ($", compensation ," in total) you must:")),
          br(),
          tags$ol(
            tags$li("Enter at least ", min_words, " unique context words for each of the cues (if you can think of more please enter them)."), 
            br(),
            tags$li("For each cue, complete the task within the 3 minutes provided.")),
          br(),
          p("Failure to satisfy any one of these requirements will automatically end your participation in this HIT."),
          br(),
          p("Remember:"),
          br(),
          tags$ol(
          tags$li("After you input a word, press enter to have it saved."), 
          br(),
          tags$li("Only click the ''Next'' button once you are done listing all context words for a given cue.")),
          br(),
          p("Click ''Next'' to start a training trial.", strong("Please note, failure to perform the task as indicated will automatically end your participation.")),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.training.fluency1",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
     )}
    
    # --------------------------------
    # fluency training tasks
    # --------------------------------
    if (CurrentValues$page %in% navigator$InputID){   # conditionl determining whether page is a fluency task training page
      CurrentValues$task_index <- which(navigator$InputID == CurrentValues$page)  # keep record of training trials performed
      return(
        SemanticFluencyTask(title = navigator$title[CurrentValues$task_index],  
                            cue = navigator$cue[CurrentValues$task_index], 
                            InputID = navigator$InputID[CurrentValues$task_index],
                            nextInputID = navigator$nextInputID[CurrentValues$task_index],
                            time = time,
                            min_words = min_words,
                            CurrentValues = CurrentValues, FluencyData = FluencyData, EventTimeLog = EventTimeLog)
      )}
    
    # --------------------------------
    # fluency tasks
    # --------------------------------
    if (CurrentValues$page == "ready") {
      
      return(
        list(
          br(),
          span(h2(strong("Ready to Start?")), style="color:#2780e3"),
          br(),
          p("Thank you for completing the trial run, if you feel ready to begin the real tasks, click ''Continue''. The first cue will immediately appear on the screen."),
          br(),
          p("If you want to return to the instructions click on ''Back to instructions''."), 
          br(),
          
          actionButton(inputId = "goto.task.fluency1",
                       label = "Continue",
                       class = "btn btn-primary"),
          HTML("<br><br>"),  # spacing using HTML
          actionButton(inputId = "goto.instructions3",
                       label = "Back to instructions",
                       class = "btn btn-primary"))
      )}
    
    # --------------------------------
    # survey
    # --------------------------------
    
    if (CurrentValues$page == "survey") {
      
      # Throw an error if not all question have been answered.
      if (CurrentValues$errors == "answerQuestions") {
        answerQuestions <- p(style = "color:Red", "Please answer all required questions!")
      } else {
        answerQuestions <- ""
      }
      
      return(list(
        br(),
        span(h2(strong("Survey (1 of 1)")), style="color:#2780e3"),
        
        br(),
        
        p("To conclude please fill out this short survey."),
        
        br(),
        
        radioButtons("party", 
                     label = "Generally speaking, do you usually think of yourself as a Democrat, a Republican, an Independent, or what?",
                     choices = c("Strong Democrat" =  1,
                                 "Weak Democrat" = 2,
                                 "Independent Democrat" = 3,
                                 "Independent Independent" = 4,
                                 "Independent Republican" = 5,
                                 "Weak Republican" = 6,
                                 "Strong Republican" = 7,
                                 "Other party" = 8,
                                 "No preference" = 9
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("ideology", 
                     label = "Generally speaking, do you usually think of yourself as a Liberal, a Conservative, a Moderate, or what?",
                     choices = c("Extremely liberal" =  1,
                                 "Liberal" = 2,
                                 "Slightly liberal" = 3,
                                 "Moderate; middle of the road" = 4,
                                 "Slightly conservative" = 5,
                                 "Conservative" = 6,
                                 "Extremely conservative" = 7,
                                 "Haven't thought much about this" = 8
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("sex",
                     label = "What is your sex?",
                     choices = list("Male" = 1, "Female" = 2, "Other" = 3),
                     selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("interesting", 
                     label = "How engaging did you find the HIT?",
                     choices = c("1 - Not at all engaging" =  1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5 - Very engaging" = 5
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons("fair", 
                     label = paste0("How fair would you say $", compensation, " is as compensation for this HIT?"),
                     choices = c("1 - Very unfair" =  1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5 - More than fair" = 5
                     ), selected = 99, width = "100%"),
        
        br(),
        
        textAreaInput("comments",
                      label = "If you have any additional comments (e.g. that can help us improve the task), please enter them below.",
                      resize = "both"),
        
        br(),
        
        p(answerQuestions),
        
        actionButton(inputId = "savedata",
                     label = "Next", 
                     class = "btn btn-primary"),
        
        HTML("<br><br><br>"))
      )
    }
    
    # --------------------------------
    # save data
    # --------------------------------
    if (CurrentValues$page == "savedata") {
      
      return(
        list(
          br(),
          span(h2(strong("Save Data")), style="color:#2780e3"),
          br(),
          p("You have completed all the required tasks. To save your data and get your HIT completion code press ''Save my data''."),
          br(),
          p("If for some reason you do not want to have your data saved, simply close this window (NOTE: you will not receive compensation)."), 
          br(),
          
          actionButton(inputId = "goto.goodbye",
                       label = "Save my data",
                       class = "btn btn-primary"))
      )}

    # --------------------------------
    # goodbye
    # --------------------------------
    if (CurrentValues$page == "goodbye") {
      
      # CALCULATE COMPLETION CODE for MTurk
      completion.code <- paste0("CW-", sample(100:999, size = 1), "-", sample(100:999, size = 1), "-", sample(100:999, size = 1))
      
      return(list(
        br(),
        h3("Thank you for your participation!"),
        br(),
        p("Here is your randomly generated study completion code. Please enter this code to submit your HIT."),
        h3(completion.code),
        br(),
        h3("What was this survey about?"),
        br(),
        p("This survey is part of an academic study exploring context words."),
        br(),
        p("You may proceed to close this window.")
      ))
    }
    
    # --------------------------------
    # next cue wait time
    # --------------------------------
    if (CurrentValues$page == "nextcue") {
      return(
        list(
          br(),
          span(h3(strong("Ready for the next cue?")), style="color:#2780e3"),
          br(),
          p("Press ''Next'' to continue to the next cue."),
          actionButton(inputId = "nexttask",
                       label = "Next",
                       class = "btn btn-primary")
        ))
    }
    
    # --------------------------------
    # performance check 1
    # --------------------------------
    if (CurrentValues$page == "booted1") {
      
      return(list(
        br(),
        h3("SORRY!"),
        br(),
        p("The list you selected is incorrect. This ends your participation in this task."),
        br(),
        p("You may proceed to close this window.")
      ))
    }
    
    # --------------------------------
    # performance check 2
    # --------------------------------
    if (CurrentValues$page == "booted2") {
      
      return(list(
        br(),
        h3("SORRY!"),
        br(),
        p("You failed to provide at least 5 unique context words as required. This ends your participation in this task."),
        br(),
        p("You may proceed to close this window.")
      ))
    }
    
    # --------------------------------
    # performance check 3
    # --------------------------------
    if (CurrentValues$page == "booted3") {
      
      return(list(
        br(),
        h3("SORRY!"),
        br(),
        p("You have run out of time. This ends your participation in this task."),
        br(),
        p("You may proceed to close this window.")
      ))
    }
  })
  
  # --------------------------------
  # page navigation
  # --------------------------------
  # consent page
  observeEvent(input$consent, {
    if (input$workerid == ""){
      CurrentValues$errors <- "Blank_Name"
    } else {
      CurrentValues$page <- "instructions1"
    }
  })
  
  # screener 1
  observeEvent(input$goto.screening2, {
    if (input$screener1.left == screener1.correct$screener1.left & input$screener1.right == screener1.correct$screener1.right) {
      CurrentValues$page <- "screening2"
    } else {
      CurrentValues$page <- "booted1"
    }
  })
  
  # screener 2
  observeEvent(input$goto.instructions3, {
    if (input$screener2.left == screener2.correct$screener2.left & input$screener2.right == screener2.correct$screener2.right) {
      CurrentValues$page <- "instructions3"
    } else {
      CurrentValues$page <- "booted1"
    }
  })
  
  # fluency task pages
  observeEvent(input[[navigator$nextInputID[CurrentValues$task_index]]], {
    if (Sys.time() > EventTimeLog[[CurrentValues$page]]){
      CurrentValues$page <- "booted3"
    }else{
      if (length(unique(FluencyData[[CurrentValues$page]][trimws(FluencyData[[CurrentValues$page]])!=""])) < min_words) {
        CurrentValues$errors <- paste0(CurrentValues$page, ".error")
      } else {
          CurrentValues$page <- gsub('[[:digit:]]+', '', navigator$nextInputID[CurrentValues$task_index])
      }
    }})
  
  # survey
  observeEvent(input$savedata, {
    # check wether all questions have been answered:
    if (any(input$age == 0, is.null(input$sex), is.null(input$party), is.null(input$ideology), is.null(input$fair), is.null(input$interesting))) {
      CurrentValues$errors <- "answerQuestions"
    } else {
      CurrentValues$page <- "savedata"
    }})
  
  # other
  observeEvent(input$goto.instructions1, CurrentValues$page <- "instructions1")
  observeEvent(input$goto.instructions2, CurrentValues$page <- "instructions2")
  observeEvent(input$goto.instructions4, CurrentValues$page <- "instructions4")
  observeEvent(input$goto.screening1, CurrentValues$page <- "screening1")
  observeEvent(input$goto.training.fluency1, CurrentValues$page <- "training.fluency1")
  observeEvent(input$goto.task.fluency1, CurrentValues$page <- "task.fluency1")
  observeEvent(input$nexttask, CurrentValues$page <- navigator$InputID[(CurrentValues$task_index + 1)])
   
  # data save
  observeEvent(input$goto.goodbye, {
      
      # Create progress message
      withProgress(message = "Saving data...",
                   value = 0, {
                     
                     incProgress(.25)
                     
                     # function to order fluency data
                     order_data <- function(cue, workerid, FluencyData = NULL, RTStartData = NULL, RTEndData = NULL){
                       return(data.frame(cbind(workerid,
                                               "cue" = rep(cue, 3),
                                               "variable" = c("Fluency", "RTStart", "RTEnd"),
                                               rbind(FluencyData, 
                                                     RTStartData,
                                                     RTEndData)), stringsAsFactors = FALSE))
                     }
                     
                     
                     SemanticData.i <- lapply(names(FluencyData), function(i) order_data(navigator[InputID == i, cue], workerid = input$workerid,
                                                                                         FluencyData = FluencyData[[i]],
                                                                                         RTStartData = RTStartData[[i]],
                                                                                         RTEndData = RTEndData[[i]]))
                     SemanticData.i <- bind_rows(SemanticData.i)
                     
                     # Write survey data to a datatable
                     SurveyData.i <- data.table("workerid" = input$workerid,
                                                "sex" = input$sex,
                                                "party" = input$party,
                                                "ideology" = input$ideology,
                                                "fair" = input$fair,
                                                "interesting" = input$interesting,
                                                "comments" = input$comments
                                                #"interesting" = input$interesting
                     )
                     
                                                   
                     incProgress(.5)
                     
                     SemanticDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SemanticData.i), "_fluency.csv")
                     SurveyDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SurveyData.i), "_survey.csv")
                     
                     # create filepath to dropbox folder

                       SemanticDatafilePath <- file.path(tempdir(), SemanticDatafileName)
                       write.csv(SemanticData.i, SemanticDatafilePath, row.names = FALSE, quote = TRUE)
                       rdrop2::drop_upload(SemanticDatafilePath, path = outputDir, dtoken = droptoken)
                       
                       SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
                       write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                       rdrop2::drop_upload(SurveyDatafilePath, path = outputDir, dtoken = droptoken)
              
                     # report progress (of data saving) to the user
                     incProgress(.40)
                     
                     # go to goodbye page
                     CurrentValues$page <- "goodbye"
                     Sys.sleep(.25)
                     incProgress(1)
                   })
  })
  
  # --------------------------------
  # SECTION 3C: WHAT TO DO WHEN A KEY IS PRESSED ----
  # http://www.javascripter.net/faq/keycodes.htm
  # --------------------------------
  # upon observing a key event
  observeEvent(input$lastkeypresscode, {
    # isolate keyboard event
    n <- input$lastkeypresscode[1]
    
    # if we are on the welcome page
    if (CurrentValues$page == "welcome") {
      if (n == 13) {   # if the enter key is pressed
        CurrentValues$page <- "instructions1"   # go to the literature page
      }
    }
    
    # if we are on a fluency task page
    # we want to store an entry upon the user pressing enter
    # and for each entry we want to register the time when the first key is pressed and the time when the enter key is pressed (i.e. when it is stored)
    # this will allow us to compute both inter- and intra-item response times
    if (CurrentValues$page %in% navigator$InputID) {
      page <- CurrentValues$page
      if (n == 13) {   # if the enter key is pressed
        FluencyData[[page]] <- c(FluencyData[[page]], input[[CurrentValues$page]])   # update the fluency list
        RTEndData[[page]] <- c(RTEndData[[page]], Sys.time())   # register the time the response was stored
        if(!is.null(RTCurrentStartData[[page]])){   # if non-enter key presses have been registered
          RTStartData[[page]] <- c(RTStartData[[page]], min(RTCurrentStartData[[page]]))   # select the minum time registered and store
        }
        RTCurrentStartData[[page]] <- c()   # reset the start time counter upon pressing enter
        updateTextInput(session, CurrentValues$page, value = "")   # clear the text input box
      } 
      # if either a number or letter key is pressed
      if(n %in% c(48:57,65:90)) {
        # RTCurrentStartData keeps a time register for each key pressed
        # it is reset once an entry is stored
        # RTStartData keeps a register of the minimum RTCurrentStartData for each entry
        # the minimum corresponds to the time the first key was pressed for a given entry
        RTCurrentStartData[[page]] <- c(RTCurrentStartData[[page]], Sys.time())   # register the time it is pressed
      }
  }
  })
  
}

#----------------------------
# CREATE APP                 ---
#----------------------------
shinyApp(ui = ui, server = server)

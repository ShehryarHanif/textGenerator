SemanticFluencyTask <- function(title = NULL, cue = NULL, InputID = NULL, nextInputID = NULL, time = time, min_words = NULL,
                                CurrentValues = CurrentValues, FluencyData = FluencyData, EventTimeLog = EventTimeLog){
  
  EventTimeLog[[InputID]] <- EventTime <- Sys.time() + time*60
  
  if (CurrentValues$errors == paste0(InputID, ".error")){
    instructions <- p(style = "color:Red", "You must input at least ", min_words, " context words in order to proceed.")      
  } else {
    instructions <- p("Please input at least ", min_words, " context words before clicking ''Next''.") 
  }
  
  return(list(
    br(),
    span(h2(strong(title)), style="color:#2780e3"),
    br(),
    h3(strong(cue)),   # training cue word
    textInput(inputId = InputID,   # control ID
              label = "",   # label to appear on top of control (color conditioned above)
              placeholder = "Click here to enter text"),  # text to appear as placeholder inside control
    p(tags$small("Press enter to save entry.")), # reminder to press enter
    HTML("<br>"),
    # render words entered
    renderText({
      paste0(rep(" - ", length(FluencyData[[InputID]])), FluencyData[[InputID]])
    }),
    HTML("<br>","<br>"),
    renderText({
      paste0("Number of unique words entered: ",  length(unique(FluencyData[[InputID]][trimws(FluencyData[[InputID]])!=""])))   # text user sees
    }),
    renderText({
      paste0("Number of words required to satisfy minimum: ",  max(10 - length(unique(FluencyData[[InputID]][trimws(FluencyData[[InputID]])!=""])),0))   # text user sees
    }),
    # render countdown clock
    renderText({
      invalidateLater(1000)   # specifies countdown to be in 1000 milliseconds (i.e. in seconds) 
      if(Sys.time() <= EventTime){
      paste("Time remaining:",  round(difftime(EventTime, Sys.time(), units = "secs")), "secs")}
      else{"You have run out of time, you may proceed to close this window."}# text user sees
    }),
    HTML("<br><br>"),   # vertical spacing
    instructions,
    # action button to be pressed by user to continue
    actionButton(inputId = nextInputID,   # button ID 
                 label = "Next",   # button label (text displayed to the user) 
                 class = "btn btn-primary")
  ))
}
library(shiny)
library(RJSONIO)

# permanent data (until database system is set up)
passwords <- read.csv("passwords.csv")
# just a variable until I set up the database
responses <- NULL


# =================
# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Function to handle a submission
  doSubmit <- function(val,text="NA") {
    if (isValidJSON(I(val))) {
      info <- fromJSON(I(val))
      # package up submission for saving
      submit <- list(user=loggedIn(),when=date(),pts=info$pts,
                     set=info$itemInfo$setID,
                     itemN=info$itemInfo$itemN,
                     name=info$itemInfo$name,
                     text=toJSON(I(text)),
                     content=info$content)
      cat(as.character(submit))
      # Don't submit blank responses, otherwise they will be sent on initialization of the page
      if( text != "") responses <<- rbind(responses,submit)
      res = as.character(info$itemInfo$setID)
      cat(paste(" with set ID",res))
      invisible(3)
    }
    else {
      info=list(itemInfo=list(setID=3,itemN=2,name="BOGUS"),content="unknown",pts=0)
      cat("initializing ...")
      invisible(1)
    }
    
  }
  
  # is the user logged in?
  loggedIn <- reactive({
    return("for debugging") # SKIP login during development
    m <- subset(passwords, name==input$loginID) ## use tolower()?
    if( nrow(m)>0  & m[1,]$pass==input$password) 
      return(m[1,]$name)
    else return(NULL)
  })
  
  
  # Each problem should have a set of filenames/URLs and flags about grading, interactivity, etc.  

  
  # just for debugging.
  statusMessage = reactive({
    vals1 = input$in1
    vals2 = input$info1
    return(paste(  vals1,"  |  ", vals2,"goodbye")) #vals
  })
  
  # Store the problem name and contents for later use
  probData = reactive({
    res <- list()
    res$fileName <- paste("Contents/",input$ThisProb,".html",sep="")
    res$html <- readChar(res$fileName, file.info(res$fileName)$size)
    res$type <- "Future use"
    res$initialized <- input$ThisProb != "Select Problem"
    return(res)
  })
  # Initialize the problem tab, in part for friendliness,
  # but ALSO so that the MathJax update doesn't screw things up.
  
  probHTML = reactive({
    if( input$ThisProb == "Select Problem")
      return("<center>No problem selected.</center>")
    fileName <- paste("Contents/",input$ThisProb, ".html",sep="")
    # MAYBE AT THE START PUT A DIRECTIVE TO KILL THE ANSWER BLOCKS, perhaps by 
    # Substituting out everything between the blocks.
    hoo <- readChar(fileName, file.info(fileName)$size)
    # MathJax update at the end of the file.
    # This kills the activity of the Shiny controls
#    hoo <- c(hoo,"<script type='text/javascript'>MathJax.Hub.Typeset()</script>")
    return(hoo)
  }) 
  
  output$asLoggedInStatus = renderText(
    paste("Logged in as",loggedIn())
  )
  
  
  output$loginStatus = renderText(
    ifelse (length(loggedIn())>0 ,"Login Successful!","Please login ..." )
  )
  
  output$probContents <- renderText({
    HTML(probHTML())
  })
  
 output$testingTextOutput <- renderPrint({statusMessage()})
  
 output$inProblemOutput <- renderPrint({statusMessage()})
#  output$tout1 <- renderPrint({input$text1})
  output$tout1 <- renderPrint({input$trigger1 #just to trigger the text
                               doSubmit(val=isolate(input$info1),text=isolate(input$text1))})
  output$tout2 <- renderPrint({input$trigger2 #just to trigger the text
                               doSubmit(val=isolate(input$info2),text=isolate(input$text2))})
  output$tout3 <- renderPrint({input$trigger3 #just to trigger the text
                               doSubmit(val=isolate(input$info3),text=isolate(input$text3))})
  output$tout4 <- renderPrint({input$trigger4 #just to trigger the text
                               doSubmit(val=isolate(input$info4),text=isolate(input$text4))})
  output$tout5 <- renderPrint({input$trigger5 #just to trigger the text
                               doSubmit(val=isolate(input$info5),text=isolate(input$text5))})
  # Multiple choice
  output$out1 <- renderPrint({doSubmit(input$in1)}) 
  output$out2 <- renderPrint({doSubmit(input$in2)}) 
  output$out3 <- renderPrint({doSubmit(input$in3)}) 
  output$out4 <- renderPrint({doSubmit(input$in4)}) 
  output$out5 <- renderPrint({doSubmit(input$in5)}) 
   
  
  # Need to figure out how to trigger this when "Scores" tab is revealed.
  output$submissions <- renderTable({input$scoreChoice
                                     responses})
  
})


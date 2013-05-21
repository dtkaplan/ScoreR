library(shiny)

# permanent data (until database system is set up)
passwords <- read.csv("passwords.csv")
# just a variable until I set up the database
responses <- NULL


# =================
# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Function to handle a submission
  doSubmit <- function(val) {
    res = val
    if (isValidJSON(I(val))) {
      info <- fromJSON(I(val))
      submit <- list(user=loggedIn(),when=date(),pts=info$pts,
                     set=info$itemInfo$setID,
                     itemN=info$itemInfo$itemN,
                     name=info$itemInfo$name,
                     content=info$content)
      cat(as.character(submit))
      responses <<- rbind(responses,submit)
      # ADD IN TO THIS the value of the answer:,
      #                   ans=info$answer)
      # need to add this to newScorerSet.R
      res = as.character(info$itemInfo$setID)
    }
    
    cat(res)
    invisible(3)
  }
  
  # is the user logged in?
  loggedIn <- reactive({
    return("for debugging") # SKIP login during development
    m <- subset(passwords, name==input$loginID) ## use tolower()?
    if( nrow(m)>0  & m[1,]$pass==input$password) 
      return(m[1,]$name)
    else return(NULL)
  })
  
  statusMessage = reactive({
    vals1 = input$in1
    vals2 = input$in2
    return(paste("in1: ",vals1,"  | in2: ", vals2,"goodbye")) #vals
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
  
  output$tout1 <- renderPrint({cat(input$text1)})
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


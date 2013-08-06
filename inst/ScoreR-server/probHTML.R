

probHTML <- reactive({  
  # Character string "Select Problem" is a flag not to load in any problem
  if( length(input$thisProblem)==0 || input$thisProblem == "Select Problem"){
    return(readChar("IntroBanner.html", file.info("IntroBanner.html")$size))
    # return("<center>No problem selected.</center>")
  }
  prob <- probData("from probHTML") # Get the data on the selected problem, File, Answers, etc.
  #   cat(paste("File name: ", prob$File,"\n"),file=stderr())
  
  if (grepl("http",prob$File,fixed=TRUE)) { # a web address
    contents <- getURL(prob$File)
  } # a file on this system
  else 
    contents <- readChar(prob$File, file.info(prob$File)$size)
  
  # Check to see if it's available.  If not, give a message to that effect.
  if (!prob$Available) {
    contents <- paste("Problem '",prob$Problem,
                      "' in assignment '",prob$Assignment,
                      "' not yet available.",sep="")
  }
  # Set up a retrigger of Mathjax
  #      contents <- gsub("</head>",
  #                       # "<script type='text/javascript'>MathJax.Hub.Queue(['Typeset',MathJax.Hub]);</script></head>",
  #                       "<script type='text/javascript'>MathJax.Hub.Typeset();</script></head>",
  #                       contents,fixed=TRUE)
  
  if( !prob$Answers ) # Strip answers from the HTML file
    contents <- gsub("<aside.*?</aside>","",contents)
  # The regex will match the first closing aside, so can handle multiple asides
  # but it won't handle nested asides.
  
  # MathJax update at the end of the file.
  # This kills the activity of the Shiny controls
  contents <- c(contents,"<script type='text/javascript'>MathJax.Hub.Typeset()</script>")
  return(contents)
}) 

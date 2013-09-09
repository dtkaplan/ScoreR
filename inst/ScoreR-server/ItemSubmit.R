
#' @param val a character string containing information from 
#' the user-interactive component of the item.  This will be JSON encoded
#' unless the item is a text submission (or potentially something else)
#' @param info a character string, JSON encoded, containing static information
#' about the item
#' @param P a character string naming the problem (in the user's view)
#' @param A a character string naming the assignment (in the user's view)
#' @param who a character string with the user's ID
newItemSubmit <- function(val,info,P=prob,A=assign,who=who) {
  # Test if there was a change to the item value.  If not, simply return.
  if (val=="" || is.null(val) || is.na(val) || val=="NA") return(NULL)
  # Get information about the problem to see if answers are being accepted
  prob <- probData(assignment=A,problem=P) # Get the data on the selected problem, File, Answers, etc.
  if( !prob$Accept || !prob$Available) return(NULL) 
  
  info <- fromJSON(I(info)) # translate the static info from JSON 
  
  # Pull out the value set interactively by the user
  # If it's a text item, that's simply the character string <val>.  Otherwise, unpack
  # the JSON-encoded value
  # browser()
  checkit(info,"itemSubmit info:")
  checkit(val,"itemSubmit val:")
  if( isValidJSON(I(val)) ) checkit("valid JSON","itemSubmit:")
  else checkit("invalid invalid invalid", "itemSubmit:")
  if (isValidJSON(I(val)) && info$type %in% "Free text") {
    checkit("bad free text","itemSubmit returning:")
    return()
  }
  if (!isValidJSON(I(val)) && !(info$type %in% "Free text")) {
    checkit("bad fixed choice", "itemSubmit returning:")
    return()
  }
  if (info$type=="Free text") {
    text <- val 
    content <- ""
    autoscore <- 0  # Not auto-scored
    pts <- 0 
  } else {
    val <- fromJSON(I(val))
    content <- val$content
    autoscore <- 1 # auto-scored
    text <- ""
    pts <- val$pts
  }
  type <- info$type
  totalpts <- info$totalpts
  setID <- info$setID
  ItemName <- info$name
  
  
  #browser()
  # TO DO : if (prob$Accept=="Immediate") flash up the hint or reward.  
  
  
  
  
  # check if the problem has an earlier entry
  searchQuery = paste("select * from submit where ",
                      "user='",who,"' and ",
                      "assignment='",A,"' and ",
                      "probID='",P,"' and ",
                      "itemName='",ItemName,"'",
                      sep="")
  res = dbGetQuery(db,searchQuery)
  
  if( nrow(res) == 0 ) {
    query = paste("insert into submit values (null, '",
                  setID,"','",
                  A,"','",
                  P,"','",
                  ItemName,"','",
                  date(),"','",date(),"',",
                  pts,",", # points scored
                  totalpts,",", # points possible
                  autoscore,",'", # not scored if Free text, etc
                  content,"','", # Fixed strings, don't need JSON
                  toJSON(I(text)),"','", 
                  who,"')",sep="")
    #   cat(paste(query,"\n"),file=stderr())
    dbGetQuery(db,query)
  }
  else { # replace the item
    # cat(paste("item id",res$id,"\n"),file=stderr())
    query = paste("update submit set ",
                  "answer='",content,"',",
                  "freetext='",toJSON(I(text)),"',",
                  "score=",pts,",",
                  "lasttime='",date(),"' ",
                  "where id='",res$id[1],"'",sep="")
    #  cat(query,file=stderr())
    dbGetQuery(db,query)
  }
}
# ==================
# check if the problem has an earlier entry
getSubmittedAnswers <- function(who,assignment,problem) {
  searchQuery = paste("select * from submit where ",
                      "user='",who,"' and ",
                      "assignment='",assignment,"' and ",
                      "probID='",problem,"'",
                      sep="")
  res = dbGetQuery(db,searchQuery)
  return(res)
}
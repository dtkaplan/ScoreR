# ===================
itemSubmit <- function(val,A="bogus",P="bogus",who="bogus",text="",flag="blank",roster=NULL){
  #  cat(paste("Flag", flag, "Roster", paste(roster,collapse=" "),"\n"),file=stderr())
  if( !(flag %in% roster)) return()
  if (is.null(val) || is.na(val) || val=="NA" || val=="") { 
    # cat("empty val\n",file=stderr()); 
    return(0)
  }
  if (who == "bogus") {cat("bogus who\n",file=stderr()); return()}
  prob <- probData(assignment=A,problem=P) # Get the data on the selected problem, File, Answers, etc.
  if( !prob$Accept || !prob$Available) {cat("prob not avail\n",file=stderr());return() }# don't accept submission
  if( substr(flag,1,4)=="text" && nchar(text)==0) return(); # empty text submission
  #cat(paste(nchar(val),"\n"),file=stderr())
  
  # plan for Tuesday morning.
#   # The NEW STUFF
#   v <- try(fromJSON(I(val)), silent=TRUE) 
#   if (inherits(foo,'try-error')) {
#     text <- val # it was just plain text
#     # extract out the number <n> from the "flag" and query the input$info<n>
#      v <- fromJSON(I(input[['info<n>']])) # REPLACE <n> with the number.
#   }
# When you have made this change, you won't need the next line.
  v <- fromJSON(I(val)) # read in the structure from the stored data
  
  
  
  content <- v$content
  type <- v$type
  pts <- v$pts
  totalpts <- v$itemInfo$totalpts
  setID <- v$itemInfo$setID
  ItemName <- v$itemInfo$name
  
  # TO DO : if (prob$Accept=="Immediate") flash up the hint or reward.  
  
  
  # check if the problem has an earlier entry
  searchQuery = paste("select * from submit where ",
                      "user='",who,"' and ",
                      "assignment='",A,"' and ",
                      "probID='",P,"' and ",
                      "itemName='",ItemName,"'",
                      sep="")
  res = dbGetQuery(db,searchQuery)
  #  cat(paste(searchQuery,"\n"),file=stderr())
  if( nrow(res) == 0 ) {
    query = paste("insert into submit values (null, '",
                  setID,"','",
                  A,"','",
                  P,"','",
                  ItemName,"','",
                  date(),"','",date(),"',",
                  pts,",", # points scored
                  totalpts,",", # points possible
                  ifelse(type %in% c("Free text"),0,1),",'", # not scored if Free text, etc
                  content,"','",
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
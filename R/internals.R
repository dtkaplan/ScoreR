.scorerEnv <- new.env()
assign("questionCount",0,envir=.scorerEnv)
#assign("textCount",0,envir=.scorerEnv) # how many text items
# a text item is one that has an associated "info" structure that holds the 
# problem identity information
assign("uniqueID",NULL,envir=.scorerEnv)

nextScorerItem <- function(counter="questionCount",
                           increment=TRUE,
                           name=NULL,
                           possiblePoints=1){
  nextScorerCount <- function(increment=TRUE) {
    .nextScorerCount <- function() {
      n <- get(counter, envir=.scorerEnv)
      assign(counter,n+1, envir=.scorerEnv)
      return(n+1)
    }
    if (increment) n <- .nextScorerCount()
    else n <- get(counter, envir=.scorerEnv)
    if (n > 1000) stop("Server still only configured for 1000 items of each type.")
    return(n)
  }
  # return a list with pertinent information
  setID <- get('uniqueID', envir=.scorerEnv)
  roster <- get('roster', envir=.scorerEnv)
  if( is.null(setID)) stop("Must set an ID with newScorerSet() at top of file.")
  if( is.null(name)) stop("Must give a unique name for each item.")
  if (name %in% roster) stop(paste(name,"is not unique among problem items."))
  assign('roster',c(roster,name), envir=.scorerEnv)
  
  itemN=nextScorerCount(increment)
  return(list(setID=get("uniqueID",envir=.scorerEnv),
              itemN=itemN,
              name=ifelse(is.null(name),as.character(itemN),name),
              totalpts=possiblePoints
  )
  )
}

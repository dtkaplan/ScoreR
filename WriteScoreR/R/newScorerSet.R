#' @title Constructs IDs for questions for the ScoreR system
#' 
#' @description Create the IDs for a new ScoreR set and generate the numerical count 
#' for successive items within the set.
#' 
#' @rdname newScorerSet
#' @aliases newScorerSet,nextScorerItem,textItem,choiceItem
#' @param ID a character string giving a (hopefully) unique ID for the set of questions
#' @param increment (default TRUE) for each item, add to the count of items within
#' the set.
#' 
#' @details
#' \code{newScorerSet} is for use in an Rmd or similar environment.  It should be called
#' once at the start of the file, so that later item-creation commands can generate
#' appropriate IDs for themselves.
#' 
#' 
#' @examples
#' newScorerSet("Math155-Sept-3-2014") 
#' nextScorerItem(name="algebra")
#' @export
newScorerSet <-function(ID) {
  assign("fixedChoiceCount", 0, envir=.scorerEnv) # count of select/dropdown
  assign("textCount", 0, envir=.scorerEnv) # count of text entry
  assign("multChoiceCount", 0, envir=.scorerEnv) # Count of multiple choice
  assign("uniqueID", ID, envir=.scorerEnv) # which item set is being displayed
  assign("timeStamp", date(),envir=.scorerEnv)
  # Put boilerplate, e.g. CSS definitions, here.
}
#' @rdname newScorerSet
#' @export
nextScorerItem <- function(counter="fixedChoiceCount",
                           increment=TRUE,
                           name="default",
                           possiblePoints=1){
  nextScorerCount <- function(increment=TRUE) {
    .nextScorerCount <- function() {
      n <- get(counter, envir=.scorerEnv)
      assign(counter,n+1, envir=.scorerEnv)
      return(n+1)
    }
    if (increment) n <- .nextScorerCount()
    else n <- get(counter, envir=.scorerEnv)
    if (n > 10) stop("Server still only configured for 10 items of each type.")
    return(n)
  }
  # return a list with pertinent information
  setID <- get("uniqueID", envir=.scorerEnv)
  if( is.null(setID)) stop("Must set an ID with newScorerSet() at top of file.")
  itemN=nextScorerCount(increment)
  return(list(setID=get("uniqueID",envir=.scorerEnv),
              itemN=itemN,
              name=ifelse(is.null(name),as.character(itemN),name),
              totalpts=possiblePoints
              )
         )
}

#' @export
newMC <- function(name=NULL,totalPts=1,hint="",reward="",markers=LETTERS){
  itemInfo <- nextScorerItem(counter="multiChoiceCount",name=name,possiblePoints=totalPts)
  thisCount <- 0
  # These must in be in the same order in all the versions, e.g., choiceItem(),textItem()
  vals <- list(pts=totalPts,hint=hint,itemInfo=itemInfo)
  vals$type="MC"
  vals$reward=reward
  
  # a function to create multiple-choice items
  res <- function(correct=TRUE,credit=as.numeric(correct),finish=FALSE,hint="",reward="Right!"){
      if(thisCount < 0) stop("You've already closed this multiple choice with 'finish=TRUE'")
      if(finish) {
        thisCount <<- -1 # Signal that it's finished to trigger error if re-used.
        return(paste("<span id='MCout",itemInfo$itemN,
                     "' class='shiny-html-output'> </span>",
                     sep=""))
      }
      thisCount <<- thisCount + 1
      thisName <- paste("MC",itemInfo$itemN,sep="")
      thisID <- paste("MCitem",thisCount,sep="")
      identifier <- markers[thisCount]
      # update the information for this item
      valsForThisChoice <- vals
      # scale points depending on whether answer is correct
      valsForThisChoice$pts <- valsForThisChoice$pts*credit # scale total points according to credit.
      valsForThisChoice$hint <- hint
      valsForThisChoice$reward <- reward
      valsForThisChoice$content=identifier # just a placeholder
      itemStr = paste("<label for='",thisID,
                  "'><input type='radio' name='",thisName,
                  "' id='",thisID,"' value='",toJSON(valsForThisChoice),
                  "'> <b>",identifier,"</b></label>", sep="")
      return(itemStr)
  }
  return(res) # the function to create items   
}

#' @export
textItem <- function(name=NULL,totalPts=1,hint="",rows=2,cols=30){
  itemInfo <- nextScorerItem(counter="textCount",name=name,possiblePoints=totalPts)
  # These must in be in the same order in all the versions, e.g., choiceItem(),textItem()
  vals <- list(pts=0,hint=hint,itemInfo=itemInfo)
  vals$type="Free text"
  vals$reward=""
  vals$content="Free text" # just a placeholder
  res <- paste("<textarea cols='",cols,"' rows='",rows,"' id='text",
               itemInfo$itemN,
               "' placeholder='Your answer here ...'></textarea>",
               sep="")
  # Trigger for the text so that the input isn't updated every few character strokes
  trigger = paste("<input type='radio' name='trigger",
                  itemInfo$itemN,"' id='g2' value='F'>",
                  "<input type='radio' name='trigger",
                  itemInfo$itemN,"' id='g1' value='M'>",sep="")
  res <- paste(res,trigger)
  # Something to hold the values, e.g. the name of the item, etc.
  holdvals <- paste("<select style='visibility: hidden' width='5' name='info",itemInfo$itemN,
                    "'><option value='",toJSON(vals),
                    "'>Ignore me</option></select>",sep="")
  res <- paste(res, holdvals,sep="")
  res <- paste(res,
               "<span id='tout", itemInfo$itemN,
               "' value='spanvalue' class='shiny-html-output'></span>",sep="")
}

# For marking up individual items
yes <- function(hint="Yes",credit=TRUE){
  return(list(credit=credit,hint=hint))
}
no <- function(hint="Sorry",credit=FALSE){
  yes(hint=hint,credit=credit)
}

#' @export
selectSet <- function(name=NULL,style="dropdown",totalPts=1,hint="",reward="Right!",...) {
  dots <- list(...)
  nms <- names(dots)
  itemInfo <- nextScorerItem(name=name,possiblePoints=totalPts)
  res <- paste("<select name='in",itemInfo$itemN,
               "'><option value='NA' selected>UNANSWERED</option>",
               sep="")
  vv <- list()
  vv$itemInfo <- itemInfo
  vv$type <- "Fixed Choice"
  for (k in 1:length(dots)) {
    # make sure that it's a list.
    thisItem <- 
    if( is.list(dots[[k]])) {
      vals <- dots[[k]]
      vv$pts <- totalPts*vals$credit
      vv$hint <- vals$hint
      vv$reward <- vals$reward
    }
    else {
      vv$pts <- ifelse(dots[k],as.numeric(totalPts),0) # Accept TRUE and FALSE to indicate credit
      vv$hint <- hint
      vv$reward <- reward
    }
    vv$content <- nms[k]

    opt <- paste("<option value='",
                 toJSON(vv),
                 "'>",nms[k],"</option>",sep="")
    res <- paste(res, opt)
  }
  res <- paste(res, 
               paste("</select><span id='out",itemInfo$itemN,
                     "' class='shiny-html-output'> </span>",sep=""))
  return(res)
}
# Numerical version of selectSet()
selectNumber <- function(choices,correct, name=NULL,totalPts=1) {
  vals <- choices %in% correct
  names(vals) <- paste(choices)
  # put things in the format for selectSet()
  do.call(selectSet,c(list(name=name,totalPts=1),vals))
}

# Storage for internal data

.scorerEnv <- new.env()
assign("fixedChoiceCount",0,envir=.scorerEnv)
assign("textCount",0,envir=.scorerEnv)
assign("multiChoiceCount",0,envir=.scorerEnv)
assign("uniqueID",NULL,envir=.scorerEnv)
assign("timeStamp",NULL,envir=.scorerEnv)


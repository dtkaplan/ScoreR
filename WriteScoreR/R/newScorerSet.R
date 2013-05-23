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
  assign("fixedChoiceCount", 0, envir=.scorerEnv)
  assign("textCount", 0, envir=.scorerEnv)
  assign("uniqueID", ID, envir=.scorerEnv)
  assign("timeStamp", date(),envir=.scorerEnv)
  # Put boilerplate, e.g. CSS definitions, here.
}
#' @rdname newScorerSet
#' @export
nextScorerItem <- function(counter="fixedChoiceCount",increment=TRUE,name="default"){
  nextScorerCount <- function(increment=TRUE) {
    .nextScorerCount <- function() {
      n <- get(counter, envir=.scorerEnv)
      assign(counter,n+1, envir=.scorerEnv)
      return(n+1)
    }
    if (increment) n <- .nextScorerCount()
    else n <- get(counter, envir=.scorerEnv)
    return(n)
  }
  # return a list with pertinent information
  setID <- get("uniqueID", envir=.scorerEnv)
  if( is.null(setID)) stop("Must set an ID with newScorerSet() at top of file.")
  itemN=nextScorerCount(increment)
  return(list(setID=get("uniqueID",envir=.scorerEnv),
              itemN=itemN,
              name=ifelse(is.null(name),as.character(itemN),name)
              )
         )
}
#' @export
textItem <- function(name=NULL,pts=1,hint=""){
  itemInfo <- nextScorerItem(counter="textCount",name=name)
  # These must in be in the same order in all the versions, e.g., choiceItem(),textItem()
  vals <- list(pts=pts,hint=hint,itemInfo=itemInfo)
  vals$type="Free text"
  vals$reward=""
  vals$content="Free text" # just a placeholder
  res <- paste("<textarea cols='60' rows='5' id='text",
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

#' @export
choiceItem <- function(name=NULL,style="dropdown",pts=0,hint="",reward="",...) {
  dots <- list(...)
  nms <- names(dots)
  itemInfo <- nextScorerItem(name=name)
  res <- paste("<select name='in",itemInfo$itemN,
               "'><option value='NA' selected>UNANSWERED</option>",
               sep="")
  for (k in 1:length(dots)) {
    # make sure that it's a list.
    if( is.list(dots[[k]]))
      vals <- dots[[k]]
    else
      vals <- dots[k]
    vals$itemInfo <- itemInfo
    vals$type <- "Fixed Choice"
    vals$pts <- pts
    vals$hint <- hint
    vals$reward <- reward
    vals$content <- nms[k]
    opt <- paste("<option value='",
                 toJSON(vals),
                 "'>",nms[k],"</option>",sep="")
    res <- paste(res, opt)
  }
  res <- paste(res, 
               paste("</select><span id='out",itemInfo$itemN,
                     "' class='shiny-html-output'> </span>",sep=""))
  return(res)
}


# Storage for internal data

.scorerEnv <- new.env()
assign("fixedChoiceCount",0,envir=.scorerEnv)
assign("textCount",0,envir=.scorerEnv)
assign("uniqueID",NULL,envir=.scorerEnv)
assign("timeStamp",NULL,envir=.scorerEnv)


# Actions to take that are shared across sessions
passwords <- read.csv("passwords.csv",stringsAsFactors=FALSE)
# Get the list of problems
tmp <- fetchGoogle("https://docs.google.com/spreadsheet/pub?key=0Am13enSalO74dF9ZZnRmekpxcVp6MllOZDlPZU5GcXc&single=true&gid=0&output=csv")
# Reading from the local files.
problemSets2 <- read.csv("problemSets.csv",stringsAsFactors=FALSE)
tmp <- rbind(problemSets2,tmp)
tmp$File <- as.character(tmp$File)
tmp$Assignment <- as.character(tmp$Assignment)
tmp$Problem <- as.character(tmp$Problem)
problemSets <- tmp
assignmentList <- with(problemSets,Assignment[!duplicated(Assignment)])

db = dbConnect(dbDriver("SQLite"),dbname="submissions.db",loadable.extensions=TRUE)
dbGetQuery(db,paste("create table if not exists submit (",
                    "id INTEGER PRIMARY KEY,",
                    "setID TEXT NOT NULL,",
                    "assignment TEXT NOT NULL,",
                    "probID TEXT NOT NULL,",
                    "itemName TEXT NOT NULL,",
                    "firsttime TEXT NOT NULL,",
                    "lasttime TEXT NOT NULL,",
                    "score INTEGER,",
                    "possible INTEGER,",
                    "autoScore INTEGER,", # has it been automatically scored
                    "answer TEXT NOT NULL,",
                    "freetext TEXT NOT NULL,",
                    "user TEXT NOT NULL)",sep=""))

# ===============
probData <- function(assignment,problem){
  #  cat( paste("probData: Assignment ' ", assignment, "' Problem '",problem,"'\n",sep=""),file=stderr())
  if(problem=="Select Problem") {
    return(list(Assignment=assignment,Problem="No prob. selected",
                Accept=FALSE,Answers=FALSE,Available=TRUE))
  }
  else{
    probs <- subset(problemSets,Assignment==assignment & Problem == problem)
    if( nrow(probs)==0) stop("BUG: No such problem in problem list.")
    if( nrow(probs)>1 ) warning(paste(
      "More than one problem matches problem '",problem, 
      "' in assignment '", assignment,"'. Check problemSets.csv"))
    return(as.list(probs[1,]))
  }
}

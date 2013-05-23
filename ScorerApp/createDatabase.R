# creating the database
library(RSQLite)
db = dbConnect(dbDriver("SQLite"),dbname="submissions.db",loadable.extensions=TRUE)
dbGetQuery(db,paste("create table if not exists submit (",
                    "id INTEGER PRIMARY KEY,",
                    "setID TEXT NOT NULL,",
                    "probID TEXT NOT NULL,",
                    "firsttime TEXT NOT NULL,",
                    "lasttime TEXT NOT NULL,",
                    "score INTEGER,",
                    "answer TEXT NOT NULL,", 
                    "user TEXT NOT NULL)",sep=""))
words = c("apple","berry","cherry","danish","egret")
for (k in 1:length(words)) {
  assignment=paste("assigment",sqrt(k),sep="")
  problem = paste("prob",k^2,sep="")
  score = k 
  user = paste("Fred",k,sep="-")
  dbGetQuery(db,paste("insert into submit values (null, '",
                      assignment,"','",problem,"','",
                      date(),"','",date(),"',",score,",'",words[k],"','",user,"')",sep=""))
}

foo = dbGetQuery(db,"select * from submit")
goo = dbGetQuery(db,"select * from submit where user=='George'")
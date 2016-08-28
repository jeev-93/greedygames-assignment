library(jsonlite)
library(arules)
library(e1071)
tables <- stream_in(file("ggevents.json"), flatten = T)
rules = apriori(temp, parameter=list(support=0.01, confidence=0.5));


temp <- tables$headers
temp <- cbind(temp,tables$post,tables$params,tables$bottle)
PerGame <- data.frame(gameid = as.numeric(), userid = as.character(), noofsession = as.numeric(),
                      validity = as.numeric(), duration = as.numeric())
loopPerGameOut <- PerGame
t1 <- PerGame


Total <- data.frame(gameid = as.numeric(), noofTotalsession = as.numeric(),
                    noofValidsession = as.numeric(), avgDuration = as.numeric())
t2 = Total


summingup <- function(temp2)
{
  options(digits.secs = 6)
  temp2$timest <- strptime(temp2$timestamp, "%Y-%m-%d %H:%M:%OS")
  diffe <- diff(temp2$timest)
  if(temp2$event[nrow(temp2)] == "ggstop")
  {
    diffe[nrow(temp2)] <- 31
  }else
  {
    diffe[nrow(temp2)] <- 0
  }
  
  temp2$differ <- diffe
  
  
  temp2$status <- 1
  temp2$validity <- 0
  
  for( i in 2:nrow(temp2))
  {
    if(!is.na(match(temp2$event[i-1], temp2$event[i])& (match(temp2$event[i-1], "ggstart"))))
    {
      temp2$status[i-1] <- 0
    }
    else
    {
      if(!is.na(match(temp2$event[i-1], temp2$event[i])& match(temp2$event[i-1], "ggstop")))
        
      {
        temp2$status[i] <- 0
      }else
      {
        if(temp2$differ[i-1] < 1)
        {
          temp2$status[i-1] <- 0
         }
        
      }
    }
  }
  
  efficient <- temp2[temp2$status == 1,]
  efficient$sessionCount <- 0
  
  efficient$sessionCount[efficient$event== "ggstop"& efficient$differ > 30 ] <- 1
  if(sum(efficient$sessionCount) < 1)
    efficient$sessionCount[nrow(efficient)] <- 1
  
  efficient$differ[efficient$event== "ggstop"] <- 0
  timer <- 0
  efficient$timing <- 0
  for( i in 1:nrow(efficient))
  {
    if(efficient$sessionCount[i] == 1)
    {
      efficient$timing[i] <- timer
      timer <- 0
    }else
    {
      timer <- timer + efficient$differ[i]
    }
  }
  
  efficient$validity[efficient$timing > 60] <- 1
  efficient$sessionCount[efficient$timing < 1] <- 0
  
  sessionperUsertemp <- cbind(efficient$game_id[1], efficient$ai5[1],sum(efficient$sessionCount),
                              sum(efficient$validity), sum(efficient$timing))
  sessionperUsertemp <- as.data.frame(sessionperUsertemp, stringsAsFactors = F)
  names(sessionperUsertemp) <- names(t1)
  loopPerGameOut <- rbind(loopPerGameOut, sessionperUsertemp )
  names(loopPerGameOut) <- names(t1)
  return(loopPerGameOut)
}
userids <- unique(temp$ai5)
for(k in 1: length(userids))
{
  temp1 <- temp[temp$ai5 == userids[k],]
  gamefinal = PerGame
  gameids <- unique(temp1$game_id)
  for(j in 1: length(gameids))
    
  {
    temp2 <- temp1[temp1$game_id == gameids[j],]
    if( nrow(temp2)>1)
    {
      loopoutperuser <- summingup(temp2)
      gamefinal <- rbind(gamefinal,loopoutperuser)
      names(gamefinal) <- names(t1)
    }
  } 
  gamefinal$noofsession <- as.numeric(gamefinal$noofsession)
  gamefinal$validity <- as.numeric(gamefinal$validity)
  gamefinal$duration <- as.numeric(gamefinal$duration)
  
  totalSessions <- sum(gamefinal$noofsession)
  validSessions <- sum(gamefinal$validity)
  avgDuration <- sum(gamefinal$duration)/validSessions
  
  
  loopoutpergame <- cbind(gamefinal$userid[1], totalSessions,validSessions,avgDuration)
  loopoutpergame <- as.data.frame(loopoutpergame, stringsAsFactors = F)
  names(loopoutpergame) <- names(t2)
  Total <- rbind(Total, loopoutpergame )
  names(Total) <- names(t2)
  
}
Total <- Total1
Total <- Total[!is.na(Total$gameid),]
Total$avgDuration <- as.numeric(Total$avgDuration)
Total$noofTotalsession <- as.numeric(Total$noofTotalsession)
Total$avgDuration[Total$avgDuration == Inf] <- 0
Total <- Total[order(Total$avgDuration, decreasing = T),]

# load require libraries
require(sqldf)
require(caret)
require(dplyr)
require(lazyeval)

#2 way count
my.f2cnt<-function(th2, vn1, vn2, filter=TRUE) {
    df<-data.frame(f1=th2[,vn1], f2=th2[,vn2], filter=filter)
    sum1<-sqldf("select f1, f2, count(*) as cnt from df where filter=1 group by 1,2")
    tmp<-sqldf("select b.cnt from df a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
    tmp$cnt[is.na(tmp$cnt)]<-0
    return(tmp$cnt)
}

#3 way count
my.f3cnt<-function(th2, vn1, vn2, vn3, filter=TRUE) {
    df<-data.frame(f1=th2[,vn1], f2=th2[,vn2], f3=th2[, vn3], filter=filter)
    sum1<-sqldf("select f1, f2, f3, count(*) as cnt from df where filter=1 group by 1,2, 3")
    tmp<-sqldf("select b.cnt from df a left join sum1 b on a.f1=b.f1 and a.f2=b.f2 and a.f3=b.f3")
    tmp$cnt[is.na(tmp$cnt)]<-0
    return(tmp$cnt)
}

# randomized leave-one out average with skrink and randomization
cat.code <- function(th, key1, y, folds = 4, cname) {
  
    th[[cname]] <- -333
    ss <- th[, c("QuoteNumber", key1, y, "split1", cname)]
    th1 <- ss[th$split1 == 0,]
    th2 <- ss[th$split1 == 2,]
    fld <- createFolds(th1[[y]], k = 4, list = TRUE)
    
    for (i in 1:length(fld)) {
      
        comp <- th1[unlist(fld[-i], use.names = FALSE),]
        eval <- th1[unlist(fld[i], use.names = FALSE),]
        
        smry <- comp %>% group_by_(.dots = key1) %>% summarise_( oos_avg = interp(~mean(a), a = as.name(y)))
        names(smry)[2] <- cname
        eval <- merge(eval, smry, by = key1, all.x = TRUE)
        th1[match(eval$QuoteNumber, ss$QuoteNumber),]$pincode <- eval$pincode.y
    }
  
}
# clip low frequent categories
clipFreq <- function(x, df = alldata) {
  
    t <- table(df[[x]])
    lowCard <-  as.integer(names(which(t < 1000)))
    df[[x]] <- ifelse(df[[x]] %in% lowCard, lowCard[1], df[[x]])
    return(df[[x]])
  
} 
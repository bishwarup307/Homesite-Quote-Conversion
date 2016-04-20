require(readr)
require(caret)
require(irlba)
require(Matrix)
require(lubridate)

setwd("~/Kaggle/Homesite")
source("./Scripts/final_utils__.R")

train <- read_csv("./Data/train.csv")
test <- read_csv("./Data/test.csv")


test$QuoteConversion_Flag <- (-1)
train$split1 <- 0
test$split1 <- 2


md <- rbind(train, test)

constCols <- names(md)[which(sapply(md, function(x) length(unique(x))) == 1)]
md <- md[, !names(md) %in% constCols]

md$SalesField8 <- log(md$SalesField8)
md$weekDay <- wday(md$Original_Quote_Date)
md$month <- month(md$Original_Quote_Date)
md$qt <- quarter(md$Original_Quote_Date)
md$year <- year(md$Original_Quote_Date)


factCols <- names(md)[sapply(md, is.character)]
mfact <- md[, factCols]

lab <- unique(c(unique(mfact$PersonalField16), unique(mfact$PersonalField17), unique(mfact$PersonalField18), unique(mfact$PersonalField19)))
mfact$PersonalField16 <- factor(mfact$PersonalField16, levels = lab)
mfact$PersonalField17 <- factor(mfact$PersonalField17, levels = lab)
mfact$PersonalField18 <- factor(mfact$PersonalField18, levels = lab)
mfact$PersonalField19 <- factor(mfact$PersonalField19, levels = lab)

mfact$PF16_PF17_tw <- my.f2cnt(mfact, "PersonalField16", "PersonalField17")
mfact$PF16_PF18_tw <- my.f2cnt(mfact, "PersonalField16", "PersonalField18")
mfact$PF16_PF19_tw <- my.f2cnt(mfact, "PersonalField16", "PersonalField19")
mfact$PF17_PF18_tw <- my.f2cnt(mfact, "PersonalField17", "PersonalField18")
mfact$PF17_PF19_tw <- my.f2cnt(mfact, "PersonalField17", "PersonalField19")
mfact$PF18_PF19_tw <- my.f2cnt(mfact, "PersonalField18", "PersonalField19")
mfact$PF16_PF17_PF18_thw <- my.f3cnt(mfact, "PersonalField16", "PersonalField17", "PersonalField18")
mfact$PF16_PF18_PF19_thw <- my.f3cnt(mfact, "PersonalField16", "PersonalField18", "PersonalField19")
mfact$PF17_PF18_PF19_thw <- my.f3cnt(mfact, "PersonalField17", "PersonalField18", "PersonalField19")


##############################################################################


mfact$PersonalField16 <- as.integer(mfact$PersonalField16)
mfact$PersonalField17 <- as.integer(mfact$PersonalField17)
mfact$PersonalField18 <- as.integer(mfact$PersonalField18)
mfact$PersonalField19 <- as.integer(mfact$PersonalField19)

sum1 <- sqldf("select PersonalField16, PersonalField17, sum(1) as cnt from mfact group by 1, 2")
sm1 <- sparseMatrix(i = sum1$PersonalField17, j = sum1$PersonalField16, x = 1)

svd1 <- irlba(sm1, nu = 5, nv = 5)

mfact$PF_16_ev1 <- svd1$v[mfact$PersonalField16, 1]
mfact$PF_16_ev2 <- svd1$v[mfact$PersonalField16, 2]
mfact$PF_16_ev3 <- svd1$v[mfact$PersonalField16, 3]
mfact$PF_16_ev4 <- svd1$v[mfact$PersonalField16, 4]
mfact$PF_16_ev5 <- svd1$v[mfact$PersonalField16, 5]

mfact$PF_17_ev1 <- svd1$u[mfact$PersonalField17, 1]
mfact$PF_17_ev2 <- svd1$u[mfact$PersonalField17, 2]
mfact$PF_17_ev3 <- svd1$u[mfact$PersonalField17, 3]
mfact$PF_17_ev4 <- svd1$u[mfact$PersonalField17, 4]
mfact$PF_17_ev5 <- svd1$u[mfact$PersonalField17, 5]

sum1 <- sqldf("select PersonalField16, PersonalField18, sum(1) as cnt from mfact group by 1, 2")
sm1 <- sparseMatrix(i = sum1$PersonalField18, j = sum1$PersonalField16, x = 1)

svd1 <- irlba(sm1, nu = 5, nv = 5)

mfact$PF_16_2_ev1 <- svd1$v[mfact$PersonalField16, 1]
mfact$PF_16_2_ev2 <- svd1$v[mfact$PersonalField16, 2]
mfact$PF_16_2_ev3 <- svd1$v[mfact$PersonalField16, 3]
mfact$PF_16_2_ev4 <- svd1$v[mfact$PersonalField16, 4]
mfact$PF_16_2_ev5 <- svd1$v[mfact$PersonalField16, 5]

mfact$PF_18_2_ev1 <- svd1$u[mfact$PersonalField18, 1]
mfact$PF_18_2_ev2 <- svd1$u[mfact$PersonalField18, 2]
mfact$PF_18_2_ev3 <- svd1$u[mfact$PersonalField18, 3]
mfact$PF_18_2_ev4 <- svd1$u[mfact$PersonalField18, 4]
mfact$PF_18_2_ev5 <- svd1$u[mfact$PersonalField18, 5]

sum1 <- sqldf("select PersonalField16, PersonalField19, sum(1) as cnt from mfact group by 1, 2")
sm1 <- sparseMatrix(i = sum1$PersonalField19, j = sum1$PersonalField16, x = 1)

svd1 <- irlba(sm1, nu = 5, nv = 5)

mfact$PF_16_3_ev1 <- svd1$v[mfact$PersonalField16, 1]
mfact$PF_16_3_ev2 <- svd1$v[mfact$PersonalField16, 2]
mfact$PF_16_3_ev3 <- svd1$v[mfact$PersonalField16, 3]
mfact$PF_16_3_ev4 <- svd1$v[mfact$PersonalField16, 4]
mfact$PF_16_3_ev5 <- svd1$v[mfact$PersonalField16, 5]

mfact$PF_19_3_ev1 <- svd1$u[mfact$PersonalField19, 1]
mfact$PF_19_3_ev2 <- svd1$u[mfact$PersonalField19, 2]
mfact$PF_19_3_ev3 <- svd1$u[mfact$PersonalField19, 3]
mfact$PF_19_3_ev4 <- svd1$u[mfact$PersonalField19, 4]
mfact$PF_19_3_ev5 <- svd1$u[mfact$PersonalField19, 5]

for (i in 1:ncol(mfact)) {
  
  if(class(mfact[[i]]) == "character") {
    
    mfact[[i]] <- as.integer(as.factor(mfact[[i]]))
  }
  
}

###################################################################################

md$int_1 <- md$Field11 - (md$Field8 + md$Field9)
md$int_2 <- md$Field8/md$Field11
md$int_3 <- md$CoverageField2A - md$CoverageField1A
md$int_4 <- md$CoverageField2B - md$CoverageField1B
md$int_5 <- md$CoverageField2A - md$CoverageField3A
md$int_6 <- md$CoverageField3B - md$CoverageField2B
md$int_7 <- md$SalesField4 - md$SalesField5
md$int_8 <- md$PropertyField26B - md$PropertyField26A
md$int_9 <- md$PropertyField18 - md$PropertyField19
md$int_10 <- ifelse(md$PropertyField32 == "Y" & md$PropertyField34 == "Y" & md$PropertyField36 == "N" & md$PropertyField37 == "Y", 1, 0)
md$int_11 <- ifelse(md$PropertyField32 == "Y" & md$PropertyField34 == "Y" & md$PropertyField36 == "N" & md$PropertyField37 == "N", 1, 0)

######################################################################################

md <- md[, !names(md) %in% factCols]
md2 <- cbind(md, mfact)
rm(md)

load("./Data/homesite_v1.RData")
a <- md[, c("QuoteNumber", "PCOMP_TSNE_1", "PCOMP_TSNE_2")]

md2 <- merge(md2, a, by = "QuoteNumber", all.x = TRUE)
rm(md)
md <- md2
rm(md2)

sum(is.na(md))
md[is.na(md)] <- -1

#################################################################

save(md, file ="./Data/homesite_v2.RData")

#################################################################

load("./Data/homesite_v2.RData")

md$int_12 <- ifelse(md$PersonalField22 == 1, 0, 1)
md$int_13 <- ifelse(md$PersonalField58 == 1, 0, 1)
md$int_14 <- ifelse(md$PersonalField44 == 0, 0,
                    ifelse(md$PersonalField44 == 1, 1, 2))

save(md, file = "./Data/homesite_v3.RData")
write_csv(md, "./Data/homesite_data.csv")

#################################################################
load("./Data/homesite_v3.RData")
load("./Data/exhaustive_TSNE.RData")

nm <- names(md)[grepl("_ev", names(md))]
md <- md[, !names(md) %in% nm]
md <- cbind(md, p)

save(md, file = "./Data/homesite_v4.RData")
write_csv(md, "./Data/homesite_v4.csv")

#################################################################

load("./Data/homesite_v4.RData")
ss <- md[, c("CoverageField1A", "CoverageField1B", "CoverageField2A", "CoverageField2B", "CoverageField3A", "CoverageField3B", 
             "CoverageField4A", "CoverageField4B")]

ss$coverage_int_1 <- apply(ss, 1, function(x) length(x) - length(unique(x)))
md$coverage_int_1 <- ss$coverage_int_1

ss <- md[, c("CoverageField1A", "CoverageField2A", "CoverageField3A", "CoverageField4A")]
ss$coverage_int_2 <- apply(ss, 1, function(x) length(unique(x)))
md$coverage_int_2 <- ss$coverage_int_2

ss <- md[, c("CoverageField1B", "CoverageField2B", "CoverageField3B", "CoverageField4B")]
ss$coverage_int_3 <- apply(ss, 1, function(x) length(unique(x)))
md$coverage_int_3 <- ss$coverage_int_3

md$coverage_int_4 <- md$coverage_int_2 - md$coverage_int_3

ss <- md[, c("CoverageField5A", "CoverageField5B", "CoverageField6A", "CoverageField6B")]
md$coverage_int_5 <- ifelse(ss$CoverageField5A == ss$CoverageField6A & ss$CoverageField5B == ss$CoverageField6B, 1, 0)

nm <- paste0("PersonalField", 23:42)
ss <- md[, nm]
ss$sumZero <- apply(ss, 1, function(x) sum( x == 0))
ss$sum <- apply(ss, 1, sum)
ss$sum <- ifelse(ss$sum == 20, 20,
                 ifelse(ss$sum == 21, 21,
                        ifelse(ss$sum == 22, 22,
                               ifelse(ss$sum == 23, 23, 24))))

md$personal_int_1 <- ss$sumZero
md$personal_int_2 <- ss$sum

save(md, file = "./Data/homesite_v5.RData")
####################################################################

load("./Data/homesite_v5.csv")
imp <- read_csv("./Data/ImportanceMatrix2.csv")

ptr <- md[md$split1 == 0,]
pte <- md[md$split1 == 2,]

test <- read_csv("./Data/pte.csv")
train <- read_csv("./Data/ptr.csv")

sstr <- train[, c("PersonalField16_cat", "PersonalField17_cat", "PersonalField18_cat", "PersonalField19_cat")]
sste <- test[, c("PersonalField16_cat", "PersonalField17_cat", "PersonalField18_cat", "PersonalField19_cat")]

ptr <- cbind(ptr, sstr)
pte <- cbind(pte, sste)

md <- rbind(ptr, pte)

md$L2_int_1 <- as.integer(as.factor(paste(md$PropertyField37, md$SalesField5, sep = "_")))
md$L2_int_2 <- as.integer(as.factor(paste(md$PropertyField37, md$PersonalField9, sep = "_")))
md$L2_int_3 <- as.integer(as.factor(paste(md$PropertyField37, md$PersonalField2, sep = "_")))
md$L2_int_4 <- as.integer(as.factor(paste(md$SalesField5, md$PersonalField9, sep = "_")))
md$L2_int_5 <- as.integer(as.factor(paste(md$SalesField5, md$PersonalField2, sep = "_")))
md$L2_int_6 <- as.integer(as.factor(paste(md$PersonalField9, md$PersonalField2, sep = "_")))

md$L3_int_1 <- as.integer(as.factor(paste(md$PropertyField37, md$SalesField5, md$PersonalField9, sep = "_")))
md$L3_int_2 <- as.integer(as.factor(paste(md$PropertyField37, md$SalesField5, md$PropertyField29, sep = "_")))

save(md, file = "./Data/homesite_v6.RData")

#########################################################################

load("./Data/homesite_v5.RData")
imp <- read_csv("./Data/ImportanceMatrix2.csv")

ptr <- md[md$split1 == 0,]
pte <- md[md$split1 == 2,]

test <- read_csv("./Data/pte.csv")
train <- read_csv("./Data/ptr.csv")

sstr <- train[, c("PersonalField16_cat", "PersonalField17_cat", "PersonalField18_cat", "PersonalField19_cat")]
sste <- test[, c("PersonalField16_cat", "PersonalField17_cat", "PersonalField18_cat", "PersonalField19_cat")]

ptr <- cbind(ptr, sstr)
pte <- cbind(pte, sste) 

md <- rbind(ptr, pte)

md$L2_int_1 <- paste(md$PropertyField37, md$SalesField5, sep = "_")
md$L2_int_2 <- paste(md$PropertyField37, md$PersonalField9, sep = "_")
md$L2_int_3 <- paste(md$PropertyField37, md$PersonalField2, sep = "_")
md$L2_int_4 <- paste(md$SalesField5, md$PersonalField9, sep = "_")
md$L2_int_5 <- paste(md$SalesField5, md$PersonalField2, sep = "_")
md$L2_int_6 <- paste(md$PersonalField9, md$PersonalField2, sep = "_")

md$L3_int_1 <- paste(md$PropertyField37, md$SalesField5, md$PersonalField9, sep = "_")
md$L3_int_2 <- paste(md$PropertyField37, md$SalesField5, md$PropertyField29, sep = "_")
md$L3_int_3 <- paste(md$PersonalField12, md$PersonalField13, md$PersonalField27, sep = "_")
md$L3_int_4 <- paste(md$SalesField1A, md$SalesField1B, md$SalesField5, sep = "_")
md$L3_int_5 <- paste(md$PersonalField1, md$PersonalField2, md$PersonalField29, sep = "_")

md$L2_int_7 <- paste(md$SalesField1A, md$SalesField1B, sep = "_")
md$L2_int_8 <- paste(md$PersonalField12, md$PersonalField13, sep = "_")
md$L2_int_9 <- paste(md$PersonalField12, md$PersonalField27, sep = "_")
md$L2_int_10 <- paste(md$PersonalField13, md$PersonalField27, sep = "_")
md$L2_int_11 <- paste(md$SalesField5, md$SalesField6, sep = "_")
md$L2_int_12 <- paste(md$PropertyField1B, md$PropertyField2B, sep = "_")
md$L2_int_13 <- paste(md$PersonalField4A, md$PersonalField4B, sep = "_")
md$L2_int_14 <- paste(md$SalesField2A, md$SalesField2B, sep = "_")
md$L2_int_15 <- paste(md$PersonalField10A, md$PersonalField10B, sep = "_")
md$L2_int_16 <- paste(md$CoverageField8, md$CoverageField9, sep = "_")
md$L2_int_17 <- paste(md$PropertyField39A, md$PropertyField39B, sep = "_")
md$L2_int_18 <- paste(md$PersonalField26, md$PersonalField27, sep = "_")

ptr_v7 <- md[md$split1 == 0,]
pte_v7 <- md[md$split1 == 2,]

write_csv(ptr_v7, "./Data/ptr_v7.csv")
write_csv(pte_v7, "./Data/pte_v7.csv")

save(md, file = "./Data/homesite_v7.RData")


##############################################################################################
load("./Data/homesite_v7.RData")

coverage <- train[, names(train)[grepl("Coverage", names(train))]]
Personal <- train[, names(train)[grepl("Personal", names(train))]]
Property <- train[, names(train)[grepl("Property", names(train))]]
Geo <- train[, names(train)[grepl("Geo", names(train))]]

sapply(coverage, function(x) length(unique(x)))

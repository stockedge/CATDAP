# 分割表解析モデル

# テストデータ作成
test1 <- data.frame(gender=rep(1,749), which=rep(1,749))
test2 <- data.frame(gender=rep(1,83), which=rep(2,83))
test3 <- data.frame(gender=rep(2,445), which=rep(1,445))
test4 <- data.frame(gender=rep(2,636), which=rep(2,636))

test <- rbind(test1,test2)
test <- rbind(test,test3)
test <- rbind(test,test4)
table(test)
#       which
# gender   1   2
#      1 749  83
#      2 445 636

# AIC計算
target <- "which"
var <- "gender"
df <- test[c(target,var)]
n <- nrow(df)
c1 <- nrow(unique(df[target]))
c2 <- nrow(unique(df[var]))

df.freq <- as.data.frame(table(df))
LogFreq <- log(df.freq$Freq)
df.freq <- cbind(df.freq, LogFreq)
df.freq <- cbind(df.freq, FreqLogFerq=df.freq$Freq*LogFreq)

df.target <- as.data.frame(table(df[target]))
df.var <- as.data.frame(table(df[var]))
df.sum.freq <- rbind(df.target, df.var)
LogFreq <- log(df.sum.freq$Freq)
df.sum.freq <- cbind(df.sum.freq, LogFreq)
df.sum.freq <- cbind(df.sum.freq, FreqLogFerq=df.sum.freq$Freq*LogFreq)

aic.0 <- (-2) * (sum(df.sum.freq$FreqLogFerq) - 2 * n * log(n)) + 2 * (c1 + c2 - 2)
# [1] 5156.274
aic.1 <- (-2) * (sum(df.freq$FreqLogFerq) - n * log(n)) + 2 * (c1 * c2 - 1)
# [1] 4630.196
aic <- aic.1 - aic.0
# [1] 4630.196

###########################################################
# catdapのR実装
###########################################################

# tipsデータに適用
library("reshape2")
library("arules")

### 分析用tipsデータ作成
# ターゲット変数の作成
tips.test <- transform(tips, flg=ifelse(tips$sex=="Male",1,0))
# 連続値はカテゴリ化が必要
tips.test <- transform(tips.test, total_bill=categorize(tips.test$total_bill, quantile = TRUE))
tips.test <- transform(tips.test, tip=categorize(tips.test$tip, quantile = TRUE))
# ターゲット変数指定
target <- "flg"
# 説明変数の指定
varList <- c("total_bill", "tip", "smoker", "day", "time", "size")

# catdap関数
catdap <- function(data,target,varList) {  

  for(var in varList) {
    df <- data
    print(var)
    
    df <- df[c(target,var)]
    n <- nrow(df)
    c1 <- nrow(unique(df[target]))
    c2 <- nrow(unique(df[var]))  
    
    df.freq <- as.data.frame(table(df))
    LogFreq <- log(df.freq$Freq)
    df.freq <- cbind(df.freq, LogFreq)
    df.freq <- cbind(df.freq, FreqLogFerq=df.freq$Freq*LogFreq)
    
    df.target <- as.data.frame(table(df[target]))
    df.var <- as.data.frame(table(df[var]))    
    df.sum.freq <- rbind(df.target, df.var)
    LogFreq <- log(df.sum.freq$Freq)
    df.sum.freq <- cbind(df.sum.freq, LogFreq)
    df.sum.freq <- cbind(df.sum.freq, FreqLogFerq=df.sum.freq$Freq*LogFreq)
    
    aic.0 <- (-2) * (sum(df.sum.freq$FreqLogFerq) - 2 * n * log(n)) + 2 * (c1 + c2 - 2)
    aic.1 <- (-2) * (sum(df.freq$FreqLogFerq) - n * log(n)) + 2 * (c1 * c2 - 1)
    
    aic <- aic.1 - aic.0
    aic.df <- data.frame(var=var,
                      aic=aic)
    print(aic.df)
    assign(paste("aic.df.", var, sep=""), aic.df)
  }
  eval(parse(text = paste("aic.rank <- list(", paste("aic.df.", varList, sep="", collapse = ","), ")", sep="")))
  aic.rank <- Reduce(function(a,b)rbind(a,b), aic.rank)
  return(list(aic.rank=aic.rank))
}
aic <- catdap(tips.test,"flg",varList)
aic$aic.rank[order(aic$aic.rank[,"aic"], decreasing=FALSE),] 
#          var       aic
# 5       time -8.028523
# 4        day -7.194401
# 1 total_bill -2.443896
# 3     smoker  1.998065
# 2        tip  3.901094
# 6       size  4.119833



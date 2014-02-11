#加载包
# Fselector，用随机森林算法选择最重要的前几个变量
# 此文档为重要分析文档，会调用别的函数
library(tm)
library(Rwordseg)
library(RTextTools)
library(FSelector)


# 防止读入的所有string都被当成factor
options(stringsAsFactors=FALSE)
# 读入csv文件
Infor.sweet <- read.csv('material/sweetsong.csv', header=TRUE)
Infor.sad <- read.csv('material/sadsong.csv', header=TRUE)

# 分词并形成语料库
corpus <- makeCorpus(Infor.sweet$lyric, Infor.sad$lyric)

# 形成DocumentTermMatrix
corpus.dtm.tfidf <- dtm(corpus, tfidf=TRUE)


# 转为data frame
corpus.df <- as.data.frame(inspect(corpus.dtm.tfidf))

## 随机森林算法选取前100个重要的词语
weights.rf <- random.forest.importance(label~., corpus.df, 1)
subset <- cutoff.k(weights.rf, 100)


## 把提取的特征作为新的docment-term matrix
feature.df <- as.DocumentTermMatrix(corpus.df[subset],weighting=weightTf)


result_all_corpus <- algorithm_summary(corpus.dtm.tfidf)
result_feature <- algorithm_summary(feature.df)




# 提取出来的特征做为一个新的数据框
d <- data.frame(word = rownames(weights.rf), freq= weights.rf$attr_importance)
# 按重要性从大到小排列
newdata <- d[order(-d$freq),]
#画出词云
wordcloud(d$word, d$freq, random.order = F, colors = brewer.pal(8, "Dark2"))



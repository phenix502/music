## 由两个character类型的变量
## 形成语料库
removeEnglish <- function(x){
  gsub("[a-z]+|[A-Z]+","",x)
}

makeCorpus <- function(str1, str2){
# 伤感歌曲分词   组成语料库
word.sad <- lapply(str1,removeEnglish)
word.sad <- lapply(word.sad,segmentCN)
corpus.sad <- Corpus(VectorSource(word.sad))

# 甜蜜歌曲分词   组成语料库
word.sweet <- lapply(str2, removeEnglish)
word.sweet <- lapply(word.sweet, segmentCN)
corpus.sweet <- Corpus(VectorSource(word.sweet))

# 合成预料库
corpus <- c(corpus.sad, corpus.sweet)
return(corpus)
}
dtm <- function(corpus, tfidf = FALSE){
  ## 文档-词矩阵 词的长度大于1就纳入矩阵  TFIDF minDocFreq
  mystopwords <- readLines("material/stopwords.txt")
  if (tfidf==TRUE){
    cor.dtm <- DocumentTermMatrix(corpus, control=list( wordLengths = c(2, Inf),
                                                     stopwords=mystopwords,
                                                     weighting = weightTfIdf))
  }
  else{
    cor.dtm <- DocumentTermMatrix(corpus, control=list( wordLengths = c(2, Inf),
                                                     stopwords=mystopwords))
  }
  ##去掉稀疏矩阵中低频率的词
  cor.dtm <- removeSparseTerms(cor.dtm, 0.98)
  
  ## 使得每一行至少有一个词不为0
  #rowTotals <- apply(cor.dtm, 1, sum)
  #cor.dtm <- cor.dtm[rowTotals > 0]
  return (cor.dtm)
}
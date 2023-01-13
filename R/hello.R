#' @title Multiple imputation of latent class analysis
#'
#' @param item is the data frame containing J categorical variables (the same specified in fs formula) all measured on a categorical scale with K − 1 categories.The categorical variables in item must be coded with consecutive values from 1 to K − 1. All missing values should be coded with NA (see poLCA manual Linzer, D. and Lewis, J. (2011) pag. 8 for details)
#' @param m_random is the number of M randomly imputed data sets
#' @param k_categories is the number of categories of the items plus 1 (e.g. if items are measured on a 4 category scale, then K=5)1
#' @param class is number of latent classes (see poLCA manual pag. 8 for details)
#' @param rep is the number of times the poLCA procedure has to be iterated in order to avoid local maxima (see poLCA manual)
#'





miLCApol<-function(item,m_random,K_categories, class, rep, formular){
  replacemiss<-function(item){
    itemp<-matrix(NA,nrow(item), ncol(item))
    for(i in 1:ncol(item)){
      itemp[,i]<-ifelse(is.na(item[,i]),K,item[,i]) }
    return(itemp) }
  itempr<-replacemiss(item)
  library(poLCA)
  itempr<-as.data.frame(itempr)
  dimnames(itempr)<-dimnames(item)
  ##see poLCA manual to specify further options in poLCA
  msim<-poLCA(fs,nclass=cl, itempr, nrep=rep ,na.rm=FALSE)
  pr<-msim$probs
  classm<-msim$predclass
  n<-nrow(itempr)
  R<-length(table(classm))
  J<-ncol(itempr)
  p<-array(NA,c(J,K, R))
  for(r in 1:R){
    for(j in 1:J){
      p[j,,r]<-pr[[j]][r,] }}
  impm<-array(NA, c(n,J,m))
  for(t in 1:m){
    for(i in 1:n){
      r<-classm[i]
      for(j in 1:J){
        impm[i,j,t]<- if(itempr[i,j]==K){
          cate<-rmultinom(1, 1, p[j,,r])
          for(k in 1:K){
            cate[k]<-ifelse(cate[k]==1, k, cate[k])}
          label<-sum(cate)
          while(label>K-1){
            cate<-rmultinom(1, 1, p[j,,r])
            for(k in 1:K){
              cate[k]<-ifelse(cate[k]==1, k, cate[k])}
            label<-sum(cate) }
          label }
        else(itempr[i,j])}}}
  return(impm) }

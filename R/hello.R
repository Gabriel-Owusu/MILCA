#' @title Multiple imputation of latent class analysis
#'
#' @param
#'
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

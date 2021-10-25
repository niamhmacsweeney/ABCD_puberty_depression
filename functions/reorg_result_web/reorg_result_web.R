# by Shen
# 17/10/2020
# *result.table
# *ls.vertical.vars= a vector of values in factor variable
# *cols.select= cols to keep in the result table
# *dep.kw: key word for dependent variable to select part of the result
# *colheaders

##### Settings
library(dplyr)

##### Main script

reorg_result_web<-function(kw.combo.factor.dep=NA,
                           result.table,cols.select,ls.label=NA,colheaders){
  if (length(kw.combo.factor.dep)>0){
    factor.kw=kw.combo.factor.dep[1]
    dep.kw=kw.combo.factor.dep[2]    
  }else{
    factor.kw=NA
    dep.kw=NA       
  }
  
  # Choose results based the keyword given for dependent variable ---------
  if (!is.na(dep.kw)){
    tmp.result.total=result.table[grep(dep.kw,result.table$dependent),]
  }else{
    tmp.result.total=result.table
  }
  
  # Choose results based the keyword given for independent variable -------
  if (!is.na(factor.kw)){
    tmp.result.total=tmp.result.total[grep(factor.kw,tmp.result.total$factor),]
  }else{
    tmp.result.total=tmp.result.total
  }
  
  # Labels to apply to dependent variables --------------------------------
  if (sum(!is.na(ls.label))>0){
    for(k in 1:nrow(ls.label)){
      tmp.result.total$dependent[grep(ls.label[k,1],tmp.result.total$dependent)]=ls.label[k,2] 
    }
  }
  
  # Round up numbers
  tmp.result.num = tmp.result.total[,cols.select]
  cache.result.num = tmp.result.total[,cols.select]
  tmp.result.num = apply(round(tmp.result.num,3),MARGIN = 2,as.character) %>%
    as.data.frame
  colnames(tmp.result.num)=cols.select
  
  for (i in 1:length(cols.select)){
    tmp.result.num[,i][abs(cache.result.num[,i])<0.002]=
      format(cache.result.num[,i][abs(cache.result.num[,i])<0.002],scientific = T,digits = 2)
  }
  
  tmp.result.num$p.corrected[cache.result.num$p.corrected<0.05]=
    paste0(tmp.result.num$p.corrected[cache.result.num$p.corrected<0.05],'*')
  
  tmp.result.return=data.frame(Dependent=tmp.result.total$dependent,
                               tmp.result.num,stringsAsFactors=F)
  colnames(tmp.result.return)=c('Dependent variable',colheaders)
  return(tmp.result.return)
}

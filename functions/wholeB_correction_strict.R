
ls.correction <- function(TargetResult,TargetCol,TargetVal,ColtoCorr){
  tmp.loc = TargetResult[,TargetCol]==TargetVal
  cl.name =  paste0('p.cor.',TargetCol)
  tmp.dat = TargetResult[tmp.loc,] %>%
    mutate(p.cor=p.adjust(.[,ColtoCorr],method='fdr'))
  colnames(tmp.dat)[ncol(tmp.dat)]=cl.name
  return(tmp.dat)
}

wholeB_correction<-function(TargetResult.step1,ls.factor,ls.dep){
  
  for (f in ls.factor){
    chunk.f=TargetResult.step1[TargetResult.step1$factor==f,]
    for (d in ls.dep){
      chunk.d=chunk.f[grep(d,chunk.f$dependent),]
      chunk.d$p.corrected=p.adjust(chunk.d$p.value,method='fdr')
      if (d==ls.dep[1]){
        tmp.collate=chunk.d
      }else{
        tmp.collate=rbind(tmp.collate,chunk.d)
      }
    }
    if(f==ls.factor[1]){
      TargetResult.step2=tmp.collate
    }else{
      TargetResult.step2=rbind(TargetResult.step2,tmp.collate)
    }
  }
  newResult = as.list(unique(TargetResult.step2$dependent)) %>%
    lapply(.,FUN=ls.correction,TargetCol='dependent',
           TargetResult=TargetResult.step2,ColtoCorr='p.corrected') %>%
    bind_rows %>%
    mutate(p.corrected=p.cor.dependent)
  return(newResult)
}
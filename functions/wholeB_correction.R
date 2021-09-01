wholeB_correction<-function(TargetResult,ls.factor,ls.dep){
  for (f in ls.factor){
    chunk.f=TargetResult[TargetResult$factor==f,]
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
      newResult=tmp.collate
    }else{
      newResult=rbind(newResult,tmp.collate)
    }
  }
  return(newResult)
}
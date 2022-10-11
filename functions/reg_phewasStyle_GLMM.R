library(lmerTest)

run_model <- function(ls.mod,mod.dat_short,mod.dat_long){
   # define vars
  
   mod.dep = as.character(ls.mod[1])
   mod.factor = as.character(ls.mod[2])
   mod.covs = as.character(ls.mod[3])
   mod.type = as.character(ls.mod[4])
   

  mod=paste0(mod.dep,'~', mod.factor, '+', mod.covs,'+(1|scnr_id) + (1|fam_id)')
  fit=glmer(as.formula(as.character(mod)),data=mod.dat_short,na.action=na.exclude,
  control = glmerControl(calc.derivs = FALSE, optimizer ="Nelder_Mead"),
  family = 'poisson')
         
  tmp.res = summary(fit)$coefficients %>% 
     as.data.frame %>% 
     dplyr::select(beta=`Estimate`,std=`Std. Error`, z.value = `z value`, p.value=`Pr(>|z|)`) %>% 
     head(2)%>% 
     tail(1) %>%
     mutate(mod_name = paste0(mod.dep,'~',mod.factor)) %>% 
     dplyr::select(mod_name, everything())
   
   return(tmp.res)
}

reg_phewasStyle <- function (ls.models,dat_short, correctByFactor=F){
  
  result.table = ls.models %>% split(.,seq(nrow(.))) %>% 
    pblapply(.,FUN = run_model,
             mod.dat_short=dat_short) %>% 
    bind_rows %>% 
    as.data.frame %>% 
    mutate(ls.models[,1:2])
  
  ls.factor=unique(ls.models$p_batch)
  result.table$p.corrected=99999
  if (correctByFactor==T){
    for (f in ls.factor){
      loc=grep(f,ls.models$p_batch)
      result.table$p.corrected[loc]=p.adjust(result.table$p.value[loc],method='fdr')
    }
  }else{
    result.table$p.corrected=p.adjust(result.table$p.value,method='fdr')
  }
  rownames(result.table)=NULL
  return(result.table)
}
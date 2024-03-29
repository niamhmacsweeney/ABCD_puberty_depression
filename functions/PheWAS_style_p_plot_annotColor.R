# By shen
# 14/05/2020
# Not very well annotated. But don't ask me to annotate... I'll be depressed
# *TargetResult: result table (column names:beta, std, p.value, p.corrected)
# *color_theme_usr:Shen/a list of colours
# *shape_sig: T=a different shape for significant assoc
# *category.input: a table of categories, first column=keywords for dependent var
#  second column=category label
# *labels_annot: a table of labels, 1st col=phenotype name in the result table
#  2nd col=phenotype label
# *outputpath: path for a tiff image to be generated.
# *plot_title: plot title
# *add_category_name: T=add category labels
# *fig_size: default=c(24,20)

library(dplyr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(cowplot)
library(ggrepel)
############################## XShen style p plot ##############################

p_plot<-function(TargetResult,color_theme_usr='Shen',shape_sig=T,sig_nominal=F,
                 category.input,labels_annot,outputpath=NA,plot_title=NA,
                 add_category_name=F,fig_size=c(24,20),y_lim=5){
  
  # Add label for some regions to have a different shape
  ls.shape = labels_annot$V1[labels_annot$is.shape==T]
  TargetResult$shape=1
  if (shape_sig==T){
    TargetResult$shape[TargetResult$dependent %in% ls.shape]=19
  }else{}
  
  TargetResult$sig = 99999
  TargetResult$sig[TargetResult$p.corrected<0.05]='*'
  TargetResult$sig[TargetResult$sig==99999]=''
  
  # Set color theme
  if(length(color_theme_usr)>1){
    cl.theme=color_theme_usr
  }else if (color_theme_usr=='Shen'){
    cl.theme=c('orangered1','orange1','royalblue3','lightseagreen','maroon',
               'salmon2','palevioletred2','olivedrab','darkslategray3',
               'slategray3','mediumpurple1')
  }else{}
  
  # Identify significance line
  if (sum(TargetResult$p.corrected<0.05)>0){
    max.p.corrected = max(TargetResult$p.value[TargetResult$p.corrected<0.05])
    pt=TargetResult$p.value[TargetResult$p.value==max.p.corrected]
    pt=-log10(max(pt))
    pt.color='pink'
  }else{
    pt=-log10(0.05)
    pt.color='grey'
  }
  
  if (sig_nominal==T){
    pt=-log10(0.05)
    pt.color='grey'
  }

  
  # Reorder result table by category and add another column of category
  tmp.result.table=TargetResult
  tmp.result.table$category=''
  add_category <- function(ls.capture,category.name,targetMat,col.tocap){
    loc.tocap=grep(ls.capture,targetMat[,col.tocap])
    targetMat[loc.tocap,'category']=category.name
    return(targetMat)
  }
  for (i in 1:nrow(category.input)){
    if (i==1){
      tmp.result.table=add_category(category.input[i,1],category.input[i,2],
                                    targetMat = TargetResult, col.tocap = 'dependent')
    }else{
      tmp.result.table=add_category(category.input[i,1],category.input[i,2],
                            targetMat = tmp.result.table, col.tocap = 'dependent')
    }
  }
  # tmp.result.table$category=factor(tmp.result.table$category,
  #                              levels=category.input[,2])
  #tmp.result.table=tmp.result.table[order(as.numeric(tmp.result.table$category),
  #                                        decreasing = F),]
  TargetResult=tmp.result.table
  
  # Add labels
  if (sum(!is.na(labels_annot))>0){
    TargetResult$labels=rep(99999,nrow(TargetResult))
    for (i in 1:nrow(labels_annot)){
      TargetResult$labels[grep(labels_annot[i,1],TargetResult$dependent)]=labels_annot[i,2]
    }
    TargetResult$labels[TargetResult$labels==99999]=''
  }else{
    TargetResult$labels=''
  }

  
  # Add a column to fix the order of phenotypes
  TargetResult$ord=1:nrow(TargetResult)
  
  # Set x axis to print categories
  axis.set <- TargetResult %>% 
    group_by(category) %>% 
    summarize(center = (max(ord) + min(ord)) / 2-5)
  
  # Make the plot
  fig=ggplot(TargetResult, aes(x=ord, y=-log10(p.value),label=labels)) +
    geom_point(size=1, shape=TargetResult$shape,stroke=1.5,
               aes(colour = category),
               alpha=0.5)+
    scale_colour_manual(values = cl.theme)+
    geom_text_repel(box.padding = unit(0.5,'lines'),segment.size = 0.2,
                    max.iter = 2000)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_text(angle = 90, size = 12,hjust=1, vjust = 1, face='bold'),
          axis.ticks.x=element_blank(),
          legend.position = 'none',
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "grey"),
          axis.line.x = element_blank(),
          plot.title=element_text(lineheight = 1,face='bold'))+
    geom_hline(yintercept=0 , color = "grey", size=0.5)+
    geom_hline(yintercept=pt, linetype="dashed", color = pt.color, size=1)+
    ylab('-log10(p)')+
    ylim(c(0,y_lim))
  if (!is.na(plot_title)){
    fig=fig+ggtitle(plot_title)
  }
  if (add_category_name==T){
    fig=fig+
      scale_x_continuous(label = axis.set$category, breaks = axis.set$center)
  }else{
    fig=fig+theme(axis.text.x=element_blank())
  }
  
  # Print figure to a file
  if (!is.na(outputpath)){
    ggsave(plot = fig,filename = outputpath,
           width = fig_size[1], height = fig_size[2], 
           units = 'cm', dpi = 300)
  }else{}
  
  return(fig)
}


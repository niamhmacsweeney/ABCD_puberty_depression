# by Shen and adapted by Niamh 
# 23/011/2021
# *result.table
# *ls.vertical.vars= a vector of values in factor variable
# *cols.select= cols to keep in the result table
# *dep.kw: key word for dependent variable to select part of the result
# *table_title: title that appears at the top
# *file_name
# *colheaders

##### Settings
library(xlsx)
library(dplyr)

##### Main script

reorg_result_xlsx<-function(result.table,ls.vertical.vars,cols.select,
                            dep.kw=NA,ls.label=NA,table_title,file_name=NA,
                            colheaders){
  # Choose results based the keyword given for dependent variable ---------
  if (!is.na(dep.kw)){
    tmp.result.total=result.table[grep(dep.kw,result.table$dependent),]
  }else{
    tmp.result.total=result.table
  }
  
  # A list of labels that you want to apply to dependent variables --------
  if (sum(!is.na(ls.label))>0){
    for(k in 1:nrow(ls.label)){
      tmp.result.total$dependent[grep(ls.label[k,1],tmp.result.total$dependent)]=ls.label[k,2] 
    }
  }
  
  # Put the tables vertically ---------------------------------------------
  for (i in ls.vertical.vars){
    tmp.result.block=tmp.result.total[grep(i,tmp.result.total$factor),]
    tmp.result.block=tmp.result.block[,cols.select]
    tmp.result.block$extra=''
    #colnames(tmp.result.block)=paste0(i,'__',colnames(tmp.result.block))
    if(i==ls.vertical.vars[1]){
      result.vertical=tmp.result.block
    }else{
      result.vertical=data.frame(result.vertical,tmp.result.block)
    }
  }
  result.vertical=data.frame(dependent=unique(tmp.result.total$dependent),result.vertical)

  # Write tables to xlsx files - beautifully!! ----------------------------
  
  if(!is.na(file_name)){
    
    # Some general indices
    cols.n=length(cols.select)
    facs.n=length(ls.vertical.vars)
    wb_width=(cols.n+1)*facs.n
    n.merged.cells=2+length(ls.vertical.vars)
    rown_sub=data.frame(start.ncol=(1:facs.n-1)*(cols.n+1)+1+1,
                        end.ncol=1:facs.n*(cols.n+1)+1) # Identify start and end cols for each sub table
    
    # Create a work book
    wb <- createWorkbook()
    sheet1 <- createSheet(wb, "Sheet1")
    
    # -> Create a cells for title
    rows <- createRow(sheet1, 1:1)
    cell <- createCell(rows, colIndex=1)[[1,1]]
    # Title
    setCellValue(cell, table_title)
    cs.title <- CellStyle(wb) +
      Font(wb, heightInPoints=14, isBold=TRUE, name="Calibri") +
      Fill(backgroundColor="goldenrod1", foregroundColor="goldenrod1",pattern="SOLID_FOREGROUND") +
      Alignment(h="ALIGN_CENTER",v="VERTICAL_CENTER",wrapText = T)
    setCellStyle(cell, cs.title)
    addMergedRegion(sheet1, startRow = 1, endRow = 1, 
                    startColumn = 1, endColumn = wb_width)
    setRowHeight(rows = rows, inPoints = 25)
    
    # Dependent variable and factors
    cs.header.dep <- CellStyle(wb) +
      Font(wb, heightInPoints=12, isBold=TRUE, name="Calibri") +
      Alignment(h="ALIGN_CENTER",v="VERTICAL_CENTER",wrapText = F)
    cs.header.var <- CellStyle(wb) +
      Font(wb, heightInPoints=12, isBold=TRUE, name="Calibri") +
      Alignment(h="ALIGN_CENTER",v="VERTICAL_CENTER",wrapText = F) +
      Fill(foregroundColor = "lightgoldenrod1", backgroundColor="lightgoldenrod1")
    cs.header.beta <- CellStyle(wb) +
      Font(wb, heightInPoints=12, isBold=TRUE, name="Calibri") +
      Alignment(h="ALIGN_CENTER",v="VERTICAL_CENTER",wrapText = F) +
      Border(color="black", position=c("BOTTOM"),pen=c("BORDER_THIN"))
    
    rows2 <- createRow(sheet1, 3)
    cell2 <- createCell(rows2, colIndex=c(1,rown_sub$start.ncol))
    setCellValue(cell2[[1,1]], 'Dependent variable')
    for (c in 1:length(ls.vertical.vars)){
      # Vertical variable (factor)
      setCellValue(cell2[[1,c+1]], ls.vertical.vars[c])
      # Header Beta etc
      header_block.tmp=CellBlock(sheet1, startRow=4, 
                             startColumn=rown_sub$start.ncol[c], noRows=1, 
                             noColumns=4, create = TRUE)
      eval(parse(text=paste0('header_block.',c,'=header_block.tmp')))
      CB.setRowData(get(paste0('header_block.',c)),x=colheaders,showNA = F,
                    rowStyle = cs.header.beta,rowIndex = 1)
    }
    
    setCellStyle(cell2[[1,1]], cs.header.dep)
    for (c in 2:length(cell2)){
      setCellStyle(cell2[[1,c]], cs.header.var)
    }
    
    addMergedRegion(sheet1, startRow = 3, endRow = 4, 
                    startColumn = 1, endColumn = 1) # merge dependent variable
    for (c in 1:nrow(rown_sub)){
      addMergedRegion(sheet1, startRow = 3, endRow = 3, 
                      startColumn = rown_sub$start.ncol[c], 
                      endColumn = rown_sub$end.ncol[c]-1)
      setColumnWidth(sheet1, 
                     colIndex=rown_sub$start.ncol[c]:rown_sub$end.ncol[c], 
                     colWidth=10)
    }
    setColumnWidth(sheet1, colIndex=1, colWidth=20)
    setColumnWidth(sheet1, colIndex=rown_sub$end.ncol, colWidth=2)
    
    # Create a cell for table header - beta etc
    
    
    # Add style to th headers
    # header_allCell=CellBlock(sheet1, 
    #                          startRow=3, startColumn=2, 
    #                          noRows=2, noColumns=facs.n*(cols.n+1), create = TRUE)
    # Fill(backgroundColor="goldenrod1", foregroundColor="goldenrod1",pattern="SOLID_FOREGROUND")
    # 
    # Add the result table
    addDataFrame(x = result.vertical,sheet=sheet1,col.names=F,row.names=F,
                 startRow = 5,startColumn = 1,showNA = FALSE)
    
    # Save workbook
    saveWorkbook(wb,file=file_name)
    
  }
  if(is.na(file_name)){return(result.vertical)}
  
}

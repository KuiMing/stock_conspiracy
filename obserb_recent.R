library(readxl)
library(httr)
library(XML)
library(googlesheets)
library(dplyr)
library(xlsx)
som <- function(x) {
  as.Date(format(x, "%Y-%m-1"))
}

xls_ETL <- function(files){
  coln <- c("券商名稱", "均價", "買價",
            "買量", "賣價", "賣量",
            "買賣超", "start","end",
            "marked","comment")
  x=read.xlsx(files,sheetIndex = 1)
  colnames(x) <- coln
  x$marked=""
  x$comment=""
  ratio <- x$買賣超[1:4]/x$買賣超[2:5]
  if (length(which(ratio>=1.5))>0){
    x$marked[1:max(which(ratio>=1.5))]=1:max(which(ratio>=1.5))
  }
  return(x)
}

new_stock <- function(stock){
  gs_auth()
  url='https://docs.google.com/spreadsheets/d/1z_2E7G5aVgzoFmgK9tPWM2PN8fppLgd-lkpQU08VLKM/edit#gid=0'
  List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)
  stock_list <- gs_read(List,ws=1)
  stock_list <- filter(stock_list,is.na(close))
  start <- stock_list$start[stock_list$code==stock] %>% 
    as.Date() %>% format("%Y%m%d")
  end <- stock_list$end[stock_list$code==stock] %>% 
    as.Date() %>% format("%Y%m%d")
  
  url=paste0('http://www.wantgoo.com/stock/',stock,
             '?searchType=stocks')
  res=GET(url)
  restr <- content(res,'text',encoding = 'utf8')
  res <- htmlParse(restr, encoding = 'utf8')
  x=xpathSApply(res,"//h3[@class='idx-name']",xmlValue)
  if (length(x)==0){
    List=gs_add_row(List,ws=2,input=c(stock,"", "not exist"))
    return()
  }
  titlename=xpathSApply(res,"//h3[@class='idx-name']",xmlValue) %>% 
    paste(.,'recent',start,end,sep = "_")
  
  newsheet <- gs_new(titlename,ws_title = "overbought")

  x=c(stock,newsheet$browser_url, newsheet$sheet_title)
  List=gs_add_row(List,ws=2,input=x)
  
  command=paste('sh /Users/benjamin/Github/stock_conspiracy/wangoo_agentstat.sh',stock,start,end)
  system(command,ignore.stderr = T)
  
  x <- xls_ETL('overbought.xls')
  newsheet <- gs_edit_cells(newsheet,input =x)
  
  x <- xls_ETL('oversold.xls')
  newsheet <- gs_ws_new(newsheet,ws_title = 'oversold')
  newsheet <- gs_edit_cells(newsheet,ws = 2,input = x)
  
}

ob_recent <- function(){
  url='https://docs.google.com/spreadsheets/d/1z_2E7G5aVgzoFmgK9tPWM2PN8fppLgd-lkpQU08VLKM/edit#gid=0'
  List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)
  old <- gs_read(List,ws=2)
  stock=gs_read(List,ws=1)
  stock$close[which(!is.na(old$close))]='done'
  List <- gs_edit_cells(List,ws = 1,input = stock)
  
  old <- filter(old,is.na(close))
  stock <- filter(stock,is.na(close))
  newstock <- setdiff(stock$code,old$code)
  if (length(newstock)>0){
    for (i in 1:length(newstock)){
      new_stock(newstock[i])
      if (i%%5==0){
        Sys.sleep(sample(120:180,1))
      }
      if (i%%300==0){
        Sys.sleep(600)
      }
    }
    old <- gs_read(List,ws=2)
    ind <- which(old$code %in% newstock & is.na(old$close))
    
    write.csv(old[ind,],file = 'newstock.csv',fileEncoding = 'utf8',row.names = F)
    
    system('/Users/benjamin/anaconda/bin/python /Users/benjamin/Github/stock_conspiracy/move_files.py recent',ignore.stderr = T)
    
    new_url <- read.csv('newstock.csv',stringsAsFactors = F,fileEncoding = 'utf8')
    
    ind <- which(old$code %in% new_url$code & is.na(old$close))
    
    old$url[ind] <- new_url$url
    
    List <- gs_edit_cells(List,ws = 2, input = old)
    
  }
}

while (TRUE){
  ob_recent()
  source('/Users/benjamin/Github/stock_conspiracy/main_daily.R')
  Sys.sleep(60*60*24)
}

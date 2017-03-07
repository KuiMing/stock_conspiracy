library(readxl)
library(httr)
library(XML)
library(googlesheets)
library(dplyr)
source('/Users/benjamin/Github/stock_conspiracy/TWSE_csv.R')
som <- function(x) {
  as.Date(format(x, "%Y-%m-1"))
}
long_date <- function(){
  date <- c(som(som(Sys.Date())-1),
            som(Sys.Date()-1))
  year <- c(format(date[1],"%Y"),
            format(date[2],"%Y"))
  mon <- c(format(date[1],"%m"),
           format(date[2],"%m"))
  date=c()
  for (i in 1:2){
    x=TWSE_csv('1101', year[i], mon[i])
    x=index(x)
    date=c(date,gsub('-','',x))
  }
  date=tail(date,5)
  
}

main_everyday <- function(recent,date){
  stock <- recent$code
  newurl <- recent$url
  

  url=paste0('http://www.wantgoo.com/stock/',stock,
             '?searchType=stocks')
  res=GET(url)
  restr <- content(res,'text',encoding = 'utf8')
  res <- htmlParse(restr, encoding = 'utf8')
  titlename <- xpathSApply(res,"//h3[@class='idx-name']",xmlValue) %>% 
    paste0('_daily_',date[1])
  
  mark <- gs_url(newurl) %>% 
    gs_read(ws=1) %>% 
    filter(!is.na(marked))
  
  newsheet <- gs_new(titlename,ws_title = mark$券商名稱[1])
  for (i in 2:dim(mark)[1]){
    newsheet <- gs_ws_new(newsheet,ws_title = mark$券商名稱[i])
  }
  
  url='https://docs.google.com/spreadsheets/d/1z_2E7G5aVgzoFmgK9tPWM2PN8fppLgd-lkpQU08VLKM/edit#gid=0'
  List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)
  List=gs_add_row(List,ws=3,input=c(stock,newsheet$browser_url,titlename))
  
  
  coln <- c("券商名稱", "均價", "買價",
            "買量", "賣價", "賣量",
            "買賣超", "date","comment")
  
  buyer=c()
  
  for (i in 1:5){
    command=paste('sh /Users/benjamin/Github/stock_conspiracy/wangoo_buyer.sh',stock,date[i],date[i])
    system(command)
    x=read_excel('buyer.xls')
    x=x[!is.na(x[,1]),]
    colnames(x)=coln
    buyer=rbind(buyer,x)
    #Sys.sleep(sample(20:30,1))
  }
  for (i in 1:dim(mark)[1]){
    output <- data.frame(matrix(0,nrow = length(date),ncol = length(coln)))
    colnames(output) <- coln
    output$date=date
    output[,1]=mark[i,1]
    output$comment=""
    x=buyer[buyer[,1]==as.character(mark[i,1]),]
    ind <- which(output$date %in% x$date)
    output[ind,]=x
    newsheet <- gs_edit_cells(newsheet,ws = i,input = output)
  }
}

get_date <- function(){
  date <- som(Sys.Date()-1)
  year <- format(date,"%Y")
  mon <- format(date,"%m")
  
  x=TWSE_csv('1101', year, mon)
  x=index(x)
  date=gsub('-','',x)
  today=format(Sys.Date(),"%Y%m%d")
  date=tail(date,1)
  if (date != today){
    return()
  }
  return(date)
}

main_daily <- function(daily,date){
  stock <- daily$code
  newurl <- daily$url

  coln <- c("券商名稱", "均價", "買價",
            "買量", "賣價", "賣量",
            "買賣超", "date","comment")
  
  command=paste('sh /Users/benjamin/Github/stock_conspiracy/wangoo_buyer.sh',stock,date,date)
  system(command)
  buyer=read_excel('buyer.xls')
  buyer=buyer[!is.na(buyer[,1]),]
  colnames(buyer)=coln

  sheet <- gs_url(newurl)
  ws <- gs_ws_ls(sheet)
  for (i in 1:length(ws)){
    output <- buyer[buyer[,1]==ws[i],]
    if (length(output$date)==0){
      output=buyer[1,]
      output$date=date
      output[1,1]=ws[i]
      output[1,2:7]=0
      output[1,9]=""
    }
    sheet <- gs_add_row(sheet,ws = i,input = output)
  }
  
}

gs_auth()
url='https://docs.google.com/spreadsheets/d/1z_2E7G5aVgzoFmgK9tPWM2PN8fppLgd-lkpQU08VLKM/edit#gid=0'
List=gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)


daily <- gs_read(List,ws=3)
date <- get_date()
daily=daily[is.na(daily$close), ]
for (i in 1:dim(daily)[1]){
  main_daily(daily[i,],date)
  if (i%%5==0){
    Sys.sleep(sample(120:180,1))
  }
}

stock=gs_read(List,ws=2)
newstock <- setdiff(stock$code, daily$code)
recent <- gs_read(List, ws = 2)
recent <- filter(recent, code %in% newstock)
date=long_date()
if (dim(recent)[1]>0){
  for (i in 1:dim(recent)[1]){
    main_everyday(recent[i,],date)
    Sys.sleep(sample(120:180,1))
  }
  old <- gs_read(List,ws=3)
  ind <- which(old$code %in% recent$code)
  write.csv(old[ind,],file = 'newstock.csv',fileEncoding = 'utf8',row.names = F)
  system('/Users/benjamin/anaconda/bin/python /Users/benjamin/Github/stock_conspiracy/move_files.py daily')
  
  new_url <- read.csv('newstock.csv',stringsAsFactors = F,fileEncoding = 'utf8')
  ind <- which(old$code %in% new_url$code)
  old$url[ind] <- new_url$url
  List <- gs_edit_cells(List,ws = 3, input = old)
}



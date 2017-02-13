library(httr)
library(magrittr)
args = commandArgs(trailingOnly=TRUE)
url <- paste0('http://www.wantgoo.com/Stock/aStock/AgentStat_Ajax?',
              'StockNo=',args[1],
              '&Types=3.5&StartDate=',args[2],
              '&EndDate=',args[3],'&Rows=35')
res <- GET(url)
content(res,'raw',encoding = 'utf8') %>%
  writeBin('tmp.txt')

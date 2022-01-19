#library(devtools)
#install_github('amberalpha/repeatsales1')
library(repeatsales1) 

if(F) { #setuo
  setv(ver=116)
  x1 <- pxpaths()$pxlr1
  x2 <- fread(x1)
  x3 <- x2[substr(V4,1,2)=='AL']
  x3a <- x2[substr(V4,1,2)=='SM']
  fwrite(x3,file='c:/temp/ppd/pp-AL-.csv')
  fwrite(x3a,file='c:/temp/ppd/pp-SM-.csv')
  f1(pcx='AL',fnam ='c:/temp/ppd/pp-AL-.csv') #read records for one postcode area to data.table
}

f1d <- f1(pcx='SM',fnam ='c:/temp/ppd/pp-SM-.csv') #records <- one postcode area 

f2d <- f2(f1d) #regular postcodes

f3d <- f3() #clean up and generate identifier

f4d <- f4(f3d) #repeat sales

x1 <- f4d[,.(idtr,buy=format(startdate,'Y%Y'),sell=format(deed_date,'Y%Y'),r)]%>%
  .[sell>buy]

x2 <- CJ(x1[,idtr],paste0('Y',1995:2021))%>%
  setnames(.,c('idtr','year'))%>%
  .[,.(idtr,year,dummy=0)]%>%
  rbind(.,x1[,.(idtr,year=buy,dummy=-1)])%>%
  .[,.(dummy=min(dummy)),.(idtr,year)]%>%
  rbind(.,x1[,.(idtr,year=sell,dummy=1)])%>%
  .[,.(dummy=max(dummy)),.(idtr,year)]%>%
  dcast(.,idtr~year,value.var='dummy')

x3 <- x1[,.(idtr,r)][x2,on=c(idtr='idtr')]%>%
  .[,-'idtr']%>%
  lm(r~.-1,.)%>%
  summary(.)%>%
  .[['coefficients']]%>%
  data.table(.,keep.rownames=T)%>%
  setnames(.,c('rn','est','se','tval','pval'))%>%
  .[,est:=est-min(est)]%>%
  .[,.(rn,x=est,xmin=est-se,xmax=est+se)]%>%
  .[-1,.(year=as.numeric(substr(rn,2,5))+.5,x,xmin,xmax)]

ggplot(x3,aes(year,x,ymin=xmin, ymax=xmax))+
  geom_line(linetype="dotted")+
  geom_errorbar(width=.2)+
  xlab('')+
  ylab('cumulative log return index')+
  labs(
    title="standard repeat sales index estimated in levels",
    subtitle = "(1) dates are not properly defined (2) regression without intercept, standard errors need care")


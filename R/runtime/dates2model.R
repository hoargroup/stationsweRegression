dates2model=function(opt){

     # Option A will only model dates for dates selected from modscag images.
     # Option B will model selected dates from modscag for months prior to March and then 1st, 8th, 15th, 22nd of March, April, May

      if(opt=='F'){#to replicate fassnacht et al. 2003
              yr=c(1993,1998,1999)
              mth=c(1,2,3,4,5)
              dy=c(1,8,15,22)
              dF=expand.grid(yr,mth,dy)
              colnames(dF)=c('yr','mth','dy')
              dateselect=as.POSIXct(strptime(with(dF,paste(yr,mth,dy)),'%Y %m %d',tz='MST'))
            }

      if(opt=='A' | opt=='B'){
          ## option A/B
          dateselect=read.table('data/selectdates/alldates.txt')#dates selected as clear from modscag.
          dateselect=as.POSIXct(strptime(dateselect$V1,'%d%b%Y',tz='MST'))
      }

      if(opt=='B') {
          ## option B
          dts=data.frame(date=dateselect)
          dts=mutate(dts,
                     yr=as.numeric(strftime(date,'%Y')),
                     mth=as.numeric(strftime(date,'%m')),
                     dy=as.numeric(strftime(date,'%d')))
          # dts=subset(dts,mth>=3 & mth<6)
          make_gendts=function(){
               yr=seq(min(dts$yr),max(dts$yr))
               mth=c(3,4,5,6)
               dy=c(1,8,15,22)
               dF=expand.grid(yr,mth,dy)
               colnames(dF)=c('yr','mth','dy')
               dF$date=as.POSIXct(strptime(with(dF,paste(yr,mth,dy)),'%Y %m %d',tz='MST'))
               return(dF)
          }
          gendts=make_gendts()
          dts=rbind(dts,gendts)
          # dts=dts[dts$mth==6,]
          dateselect=dts$date
     }
     if(opt=='B2') {##this will run survey dates without dropping the station
     dates=c('2008-03-16','2008-04-03','2008-04-04','2008-04-05','2008-04-07','2008-05-01','2008-05-02','2008-05-05',
'2009-01-31','2009-02-28','2009-02-28','2009-03-01','2009-03-06','2009-03-17','2009-03-28','2009-03-29','2009-04-02','2009-04-03','2009-05-02',
'2001-04-27','2002-04-03','2001-04-22','2002-04-06','2001-04-23','2002-04-05','2001-04-24','2002-04-04',
'2007-05-10','2006-05-11','2005-05-10','2004-05-12','2003-05-14','2002-05-01','2001-05-09')

dateselect=as.POSIXct(dates,tz='MST')
}


if(opt=='surveyvalidation'){
dates=c('2008-03-16','2008-04-03','2008-04-04','2008-04-05','2008-04-07','2008-05-01','2008-05-02','2008-05-05',
'2009-01-31','2009-02-28','2009-02-28','2009-03-01','2009-03-06','2009-03-17','2009-03-28','2009-03-29','2009-04-02','2009-04-03','2009-05-02',
'2001-04-27','2002-04-03','2001-04-22','2002-04-06','2001-04-23','2002-04-05','2001-04-24','2002-04-04',
'2007-05-10','2006-05-11','2005-05-10','2004-05-12','2003-05-14','2002-05-01','2001-05-09')

dateselect=as.POSIXct(dates,tz='MST')
}

     ## CSU
## 2008-03-16 Lizard Head
## 2008-04-03 Joe Wright
## 2008-04-04 Dry Lake
## 2008-04-05 S. Brush Creek
## 2008-04-07 Niwot
## 2008-05-01 Joe Wright
## 2008-05-02 Dry Lake
## 2008-05-05 Niwot

## 2009-01-31 Joe Wright  --  clear MODSCAG image available
## 2009-02-27 Joe Wright  -- clear MODSCAG image available 2-28
## 2009-02-28 Dry Lake  --  clear MODSCAG image available
## 2009-03-01 S. Brush Creek
## 2009-03-06 Niwot
## 2009-03-17 Togwotee Pass
## 2009-03-28 Dry Lake
## 2009-03-29 S. Brush Creek
## 2009-04-02 Joe Wright
## 2009-04-03 Niwot
## 2009-05-02 Joe Wright

## Upper Rio Grande Surveys
## 2001-04-25 (april 22-27)
## 2002-04-07 (april 3-12)

## GLV Snow Survey
## 2012-04-28
## 2011-05-11
## 2010-05-12
## 2009-5-13
## 2008-5-14
## 2007-5-10
## 2006-5-11
## 2005-5-10
## 2004-5-12
## 2003-5-14
## 2002-5-1
## 2001-5-9
## 2000-5-3

dateselect=dateselect[order(dateselect)]
    return(dateselect)
}


### Load in data & packages for analysis --------------------------------
## Load in requisite packages

mods=c('rgdal','data.table','raster','rgeos','ggplot2','ggthemes','ggmap','sp','tigris','leaflet','maptools','RColorBrewer','scales')
rbind(lapply(mods, function(x) suppressPackageStartupMessages(require(x,character.only=TRUE,quietly = TRUE))))

## Load in dataset. This link has the dataset.
tg=fread("https://www.transportation.gov/sites/dot.gov/files/docs/tiger_allUpdate.csv")

## note to self, tweet USDOT & tell them not to put spaces in column headings...
names(tg) 
setnames(tg, tg[,gsub("[^[:alnum:]]","",names(tg))])

## Create a column for year of award
tg[,Year := as.integer(gsub("[^0-9]","",Round))] 
## change award amount to numeric
tg[,Amount:=as.numeric(gsub("[\\$,]","",Amount))]


# Distribution of award amts by year + Phila ------------------------------
# And a column to show whether we're us or not
tg[,Phila:=ifelse(grepl("*[Pp]hila*|Southeastern Pennsylvania Transportation Authority|*SEPTA*|Center City District",Applicant)==TRUE,1,0)]
tg_p=tg[Phila==1,] # Create indicator for Philadelphia
ggplot(data=tg,aes(x=as.character(Year),y=Amount/1e6,fill=as.character(Year))) + 
  geom_boxplot() +
  geom_point(data=tg_p,
             aes(x=as.character(Year),y=Amount/1e6), size=3,color="darkred")+
  geom_text(data=tg_p,
            aes(x=as.character(Year),y=Amount/1e6,label=paste0('$',round(Amount/1e6,1),'M')),vjust=-1,fontface=('bold.italic'))+
  scale_y_continuous(labels = scales::dollar_format(prefix="$",suffix="M"))+
  labs(x="Funding Year",
       y="Distribution of Amounts in Millions",
       title="Philly's award amounts vs. all other applicants")+
  guides(fill=FALSE)


# Number of awards by year ------------------------------------------------

yr = tg[,.N,Year]
ggplot(data=yr,aes(x=as.character(Year),y=N)) + 
  geom_bar(stat='identity',fill='gray40') +
  geom_text(aes(y=N,label=N),vjust=1.5,color='white',fontface='bold')+
  labs(x="Funding Year"
       ,y="Number of Applications Awarded"
       ,title="Number of successful applications by funding year")+
  theme(axis.text=element_text(size=10,color='black'),
        strip.text=element_text(size=10,color='black',face='bold'),
        legend.text=element_text(size=10))

# Map out data ------------------------------------------------------------

download.file('ftp://ftp2.census.gov/geo/tiger/TIGER2016/COUNTY/tl_2016_us_county.zip','cty_shp.zip') ## Download the Census county file
unzip('cty_shp.zip',exdir='cty_shp') # Create a folder
shp <- readOGR('C:/Users/A770213/Desktop/cty_shp',layer='tl_2016_us_county') # Census county shapefile

# turn TIGER lat/long data to spatial points
crs = shp@proj4string@projargs # align coordinate reference systems
pts=SpatialPointsDataFrame(coords=tg[,list(Longitude,Latitude)], data=tg, proj4string=CRS(crs))

## double check to make sure the shape & point coordinates are aligned...
# plot(shp) # plot county shapefile
# plot(pts,add=TRUE,col='blue') # overlay grant sites point data

sj = data.table(over(pts,shp)) # overlay points on shapefile

sj=data.table(cbind(pts@data,sj)) # bind to original dataset
index=c('GEOID','NAME')
sj[, c(index) := lapply(.SD, as.character) , .SDcols = index ] # strip unused factor levels

# County win probability distribution -------------------------------------
cty_wins = as.data.table(sj[,table(GEOID)]) # Number of wins by county -- the counties may be slightly off due to map overlay errors
# cty_wins[!duplicated(GEOID),.N] 269 unique counties
wins_summary = cty_wins[,list(freq=.N),by=list(n_events=N)] # Number of counties (269) X number of wins (381)
wins_summary[,rate:=n_events*freq] # marginal probabilities

# mean number of wins
mu_wins = wins_summary[,sum(rate)/sum(freq)] 

# standard deviation of mean number of wins
sd_wins = sqrt(mu_wins) 
sd_rng = c(mu_wins+sd_wins, # 1 standard deviation upper
           mu_wins+sd_wins*2, # 2 standard deviation upper
           mu_wins+sd_wins*3 # 3 standard deviation upper
           )
txt=data.frame(lab=c('Average\n# wins','1 Std Dev','2 Std Dev','3 Std Dev'),
  x=c(round(mu_wins,1),round(sd_rng,1)), y=rep(100) )

## Exact confidence intervals
ex_ci=poisson.test(sum(wins_summary$rate),sum(wins_summary$freq))$conf.int[c(1,2)]

## plot out the counts + SDs & exact CIs
ci_pal = brewer.pal(3,'Set1')
sd_pal = brewer.pal(6,'Reds')
hi_cty = data.frame(cty = c('King, WA','Philadelphia, PA','Los Angeles, CA','Cook, IL'),
                    X=c(6,7,7,8),Y=c(10,22,10,10))
ggplot(data=wins_summary,
       aes(x=n_events,y=freq))+
  geom_bar(stat='identity',color='grey58',size=1.2)+
  scale_x_continuous(breaks = c(1:8), labels = c(1:8))+
  geom_vline(xintercept = mu_wins,color=ci_pal[2],size=1)+
  geom_vline(xintercept = ex_ci,color=ci_pal[3],linetype='longdash',size=.7)+
  geom_vline(xintercept = sd_rng,color=sd_pal[c(4:6)],size=1)+
  geom_text(data=txt,aes(label=paste0(lab,':\n',x),
                         x=x,y=y,hjust=-.001),size=3.5)+
  geom_text(data=hi_cty,aes(x=X,y=Y,label=cty),size=3,angle=35,hjust=-.01)+
  theme(panel.margin = unit(0.5, 'lines'), 
        strip.text.y = element_text(angle = 0))+
  labs(x='Number of grants won',
       y='Number of counties',
       title= "Statistical deviation of grant wins")

## test for Distribution
# set.seed(666)
# x.poi<-rpois(n=length(cty_wins[,N]),lambda=mu_wins)
# x.nbi<-rnbinom(length(cty_wins[,N]),mu = mu_wins, size = 8)
# plot(sort(x.poi),sort(cty_wins[,N]),col='blue',pch=20)
# abline(lm(sort(x.poi)~sort(cty_wins[,N])),col='blue')
# points(sort(x.nbi),sort(cty_wins[,N]),col='red',pch=20)
# abline(lm(sort(x.nbi)~sort(cty_wins[,N])),col='red')


## calculate probabilities
p=cbind(lapply(c(1:8), ppois, lambda=mu_wins, lower.tail=FALSE))
probs=data.frame(p=unlist(cbind(lapply(c(1:8), ppois, lambda=mu_wins, lower.tail=FALSE))),
                    wins=c(1:8))

## Plot probability data
ggplot(data=probs,aes(x=wins,y=p))+geom_line(size=1)+
  scale_color_gradient(low='springgreen',high='springgreen4')+
  scale_y_continuous(labels = scales::percent,limits = c(0,.45))+
  geom_text(aes(label=sprintf("%1.2f%%", 100*p)),hjust=-.1,vjust=-.5)+
  labs(x='Number of grants won',
       y='Probability of success',
       title='Probability of earning X number of grants')+
  scale_x_continuous(breaks=c(1:8),limits = c(1,8.5))


# Map regional patterns -----------------------------------------------------
## Download the US Census Division shapefile
download.file('http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_division_500k.zip','USCB_div.zip')
unzip('USCB_div.zip',exdir='div_shp') # unzip & create a folder
div <- readOGR('C:/Users/A770213/Desktop/div_shp',layer='cb_2015_us_division_500k') # Census county shapefile
crs=div@proj4string@projargs # set coordinate reference system
pts=SpatialPointsDataFrame(coords=tg[,list(Longitude,Latitude)], data=tg, proj4string=CRS(crs)) # match points & shapefile's CRS
# div@proj4string@projargs == pts@proj4string@projargs TRUE
site_div = data.table(over(pts,div))
site_div=data.table(cbind(pts@data,site_div))

xtabs(~site_div$NAME+site_div$Year)
tab=site_div[,list(cnt=.N,amt=sum(Amount)),by=list(div_name=NAME,year=Year)]
tab[,':='(yr_cnt=sum(cnt),
          yr_amt=sum(amt)),by=year]
tab[,':='(cnt_pct=round(cnt/yr_cnt,3),
          amt_pct=round(amt/yr_amt,3))]
tab_m=melt(tab,id.vars = c('year','div_name'),measure.vars=c('cnt_pct','amt_pct'))
tab_m[,variable := factor(variable,levels=c('amt_pct','cnt_pct'),labels=c('Percent of Dollars Awarded','Percent of Awards Given'))]
tab_m[,reg_name:=ifelse(div_name %in% c('New England','Middle Atlantic'),'Northwest',
                      ifelse(div_name %in% c('East North Central','West North Central'),'Midwest',
                             ifelse(div_name %in% c('South Atlantic','East South Central','West South Central'),'South','West'
                                    )))]
tab_m2 = tab_m[,list(value=sum(value)),by=list(year,reg_name,variable)]


# plot divisions
ggplot()+
  geom_tile(data=tab_m,
            aes(x=as.character(year),
                y=as.character(div_name),
                fill=value))+
  scale_fill_gradient(low = "white",high = "darkgreen")+
  labs(x="Funding Year",y="Census Division Name")+
  facet_grid(.~variable)

### plot regions
## heat map
ggplot()+
  geom_tile(data=tab_m2,
            aes(x=as.character(year),
                y=as.character(reg_name),
                fill=value))+
  scale_fill_gradient(low = "white",high = "darkgreen")+
  labs(x="Funding Year",y="Census Division Name")+
  theme(axis.text=element_text(size=10,color='black'),
        axis.text.x=element_text(angle = 90,vjust=.5),
        strip.text=element_text(size=10,color='black',face='bold'),
        legend.text=element_text(size=10))+
  facet_grid(.~variable)

## line graph
ggplot(data=tab_m2,aes(x=year,color=reg_name,y=value))+
  geom_line(stat='identity',size=1)+
  scale_x_continuous(breaks=c(2009:2015),labels=c(2009:2015))+
  scale_y_continuous(labels=scales::percent)+
  labs(x='Funding Year',
       y='Percent of Total Awarded',
       title='Regional share of total awards & award amounts',
       color='US Region')+
  theme(axis.text=element_text(size=10,color='black'),
        strip.text=element_text(size=10,color='black',face='bold'),
        legend.text=element_text(size=10))+
  facet_grid(variable~.)


# Analyze Project Types ---------------------------------------------------

## illustrate confusing nameology
tg_ex = tg[grep('*Pedestrian*|*Rail*',tg[,ProjectType])]
xtabs(~tg_ex[,Year]+tg_ex[,ProjectType]) 

# show what Philly won
amt=tg[Phila==1,list(Year,ProjectName,ProjectType,Amount=paste0('$',Amount/1e6,'M'))]
amt[order(Year),]

# Show amounts & number awards by year
tg_m1 = tg[,list(Amount=sum(Amount),N=.N),by=list(Year,ProjectType)]
tg_m2 = tg[,list(Amount=sum(Amount),N=.N),by=list(Year,ProjectType2)]
tg_m12 = tg[,list(Amount=sum(Amount),N=.N),by=Year]
lapply(list(tg_m1,tg_m2,tg_m12), function(x) setkeyv(x, 'Year'))
tg_m1=tg_m12[tg_m1]
tg_m2=tg_m12[tg_m2]
tg_m1[,':='(pct_amt=i.Amount/Amount,
            pct_N=i.N/N)]
tg_m2[,':='(pct_amt=i.Amount/Amount,
            pct_N=i.N/N)]
yrs=data.frame(yr=c(2009.5,2010.5,2011.5,2012.5,2013.5,2014.5))

# tg_m = melt(tg,id.vars='Year',measure.vars=c('Amount','ProjectType','ProjectType2'))
ggplot()+
  geom_tile(data=tg_m2,
            aes(x=as.character(Year),
                y=as.character(ProjectType2),
                fill=pct_amt))+
  # theme(panel.border=element_rect(fill = alpha('black', .5))) +
  # geom_line(data=yrs,aes(x=yr,y=length(yr)),color='black')+
  scale_fill_gradient(low = "white",high = "darkgreen")+
  labs(x="Year",y="Type of Project")
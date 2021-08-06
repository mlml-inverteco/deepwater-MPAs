# new chi square test

library(ggplot2)
library(ggpubr)
library(car)
library(gridExtra)
library(agricolae)
library(pwr)
library(sjstats)
library(tidyverse)
library(plyr)
library(dplyr)
library(vegan)
library(yarrr)
library(GLDEX)

setwd("C:/Users/Jacks/Desktop/Marine Bio/Deepwater_MPAs/Subprojects/Depth_ranges")
prop <- data.frame(read.csv("Habitat_prop.csv"))
Ano <- subset(prop, MPA_Group == "Ano Nuevo")
Big <- subset(prop, MPA_Group == "Big Creek")
Bodega <- subset(prop, MPA_Group == "Bodega Bay")
Campus <- subset(prop, MPA_Group == "Campus Point")
Carrington <- subset(prop, MPA_Group == "Carrington Point")
Farallon <- subset(prop, MPA_Group == "Farallon Islands")
Farnsworth <- subset(prop, MPA_Group == "Farnsworth")
Gull <- subset(prop, MPA_Group == "Gull Island")
Harris <- subset(prop, MPA_Group == "Harris Point")
Montara <- subset(prop, MPA_Group == "Montara")
Piedras <- subset(prop, MPA_Group == "Piedras Blancas")
Pillar <- subset(prop, MPA_Group == "Pillar Point")
Pt.Arena <- subset(prop, MPA_Group == "Point Arena")
Pt.Buchon <- subset(prop, MPA_Group == "Point Buchon")
Pt.Conception <- subset(prop, MPA_Group == "Point Conception")
Pt.Lobos <- subset(prop, MPA_Group == "Point Lobos")
Pt.St.George <- subset(prop, MPA_Group == "Point St. George")
Pt.Sur <- subset(prop, MPA_Group == "Point Sur")
Portuguese <- subset(prop, MPA_Group == "Portuguese Ledge")
Reading <- subset(prop, MPA_Group == "Reading Rock")
Sea <- subset(prop, MPA_Group == "Sea Lion Gulch")
South <- subset(prop, MPA_Group == "South La Jolla")
South.Pt. <- subset(prop, MPA_Group == "South Point")
Swamis <- subset(prop, MPA_Group == "Swami's")
Ten <- subset(prop, MPA_Group == "Ten Mile")

chi_square_table <- function(df){

n1 <- length(unique(subset(df, Designation == "MPA")$Line))
n2 <- length(unique(subset(df, Designation == "Reference")$Line))
Title <- df[1,8]

df <- df %>% 
  group_by_all() %>%
  mutate(newnames=paste0(Designation,Project,Dive,Line))
split.line <- split(df, df$newnames)
for (I in 1:length(split.line)) {assign(unique(split.line[[I]]$newnames), 
                                        split.line[[I]])}

substrate.divide <- lapply(split.line, function(x){
  
  hard.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Hard",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))

  ifelse('Hard' %in% x$Habitat_Type == FALSE, x <- rbind(x, hard.dummy), x)
    
  hard.mixed.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Hard_Mixed",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))
  
  ifelse('Hard_Mixed' %in% x$Habitat_Type == FALSE, x <- rbind(x, hard.mixed.dummy), x)
  
  soft.mixed.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Soft_Mixed",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))
  
  ifelse('Soft_Mixed' %in% x$Habitat_Type == FALSE, x <- rbind(x, soft.mixed.dummy), x)
  
  soft.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Soft",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))
  
  ifelse('Soft' %in% x$Habitat_Type == FALSE, x <- rbind(x, soft.dummy), x)
  
  final_product <- as.data.frame(x)
  
})

all.rows <- bind_rows(substrate.divide)

sub.des <- all.rows %>% 
  group_by_all() %>%
  mutate(newnames=paste0(Designation,Habitat_Type))
split.des <- split(sub.des, sub.des$newnames)
for (I in 1:length(split.des)) {assign(unique(split.des[[I]]$newnames), 
                                        split.des[[I]])}

totals <- lapply(split.des, function(x){
  
  average <- mean(x$Invert_Area_m2)
  sd <- sd(x$Invert_Area_m2)
  substrate <- x[1,16]
  designation <- x[1,10]
  sub.frame <- data.frame(Designation=designation,Habitat=substrate,Average=average,sd=sd)
  
})

summary <- bind_rows(totals)

rownames <- c("MPA","Reference")
colnames <- c("Hard","Hard Mixed","Soft_Mixed","Soft")
pool <- matrix(c(summary[1,3],summary[2,3],summary[4,3],summary[3,3],summary[5,3],summary[6,3],summary[8,3],summary[7,3]),nrow=2,byrow=TRUE,dimnames=list(rownames,colnames))
pool
chisq.test(pool)
E=chisq.test(pool)$expected
O=chisq.test(pool)$observed
N_r=nrow(pool)
N_c=ncol(pool)
df=(N_r-1)*(N_c-1)
x2=sum((O-E)^2/E)
p=1-pchisq(x2,df=df)
label1="x2-value:"
label2="p-value:"
label3="Table of expected values:"
sprintf("%s %f", label1,x2)
sprintf("%s %f", label2,p)
print(E)
Substrate.data <- data.frame(MPA=Title,MPA.Hard.substrate=summary[1,3],MPA.Hard.sd=summary[1,4],MPA.Hard.mixed.substrate=summary[2,3],MPA.Hard.mixed.sd=summary[2,4],MPA.Soft.mixed.substrate=summary[4,3],MPA.Soft.mixed.sd=summary[4,4],MPA.Soft.substrate=summary[3,3],MPA.Soft.sd=summary[3,4],Ref.Hard.substrate=summary[5,3],Ref.Hard.sd=summary[5,4],Ref.Hard.mixed.substrate=summary[6,3],Ref.Hard.mixed.sd=summary[6,4],Ref.Soft.mixed.substrate=summary[8,3],Ref.Soft.mixed.sd=summary[8,4],Ref.Soft.substrate=summary[7,3],Ref.Soft.sd=summary[7,4],n1=n1,n2=n2,x2=x2,p=p)

}

Ano.table <- chi_square_table(Ano) #Fisher
Big.table <- chi_square_table(Big) #Fisher
Bodega.table <- chi_square_table(Bodega)
Campus.table <- chi_square_table(Campus) #Fisher
Carrington.table <- chi_square_table(Carrington)
Farallon.table <- chi_square_table(Farallon) #Fisher
Farnsworth.table <- chi_square_table(Farnsworth)
Gull.table <- chi_square_table(Gull)
Harris.table <- chi_square_table(Harris) #Fisher
Montara.table <- chi_square_table(Montara) #NULL
Pillar.table <- chi_square_table(Pillar) #Fisher
Pt.Arena.table <- chi_square_table(Pt.Arena)
Pt.Buchon.table <- chi_square_table(Pt.Buchon) #Fisher
Pt.Conception.table <- chi_square_table(Pt.Conception)
Pt.Lobos.table <- chi_square_table(Pt.Lobos)
Pt.St.George.table <- chi_square_table(Pt.St.George) #Fisher
Pt.Sur.table <- chi_square_table(Pt.Sur)
Portuguese.table <- chi_square_table(Portuguese) #Fisher
Reading.table <- chi_square_table(Reading)
Sea.table <- chi_square_table(Sea) #Fisher
South.Pt.table <- chi_square_table(South.Pt.)
Swamis.table <- chi_square_table(Swamis)
Ten.table <- chi_square_table(Ten) #Fisher

Fisher.table <- function(df){
  
  set.seed(13)
  n1 <- length(unique(subset(df, Designation == "MPA")$Line))
  n2 <- length(unique(subset(df, Designation == "Reference")$Line))
  Title <- df[1,8]
  
  df <- df %>% 
    group_by_all() %>%
    mutate(newnames=paste0(Designation,Project,Dive,Line))
  split.line <- split(df, df$newnames)
  for (I in 1:length(split.line)) {assign(unique(split.line[[I]]$newnames), 
                                          split.line[[I]])}
  
  substrate.divide <- lapply(split.line, function(x){
    
    hard.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Hard",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))
    
    ifelse('Hard' %in% x$Habitat_Type == FALSE, x <- rbind(x, hard.dummy), x)
    
    hard.mixed.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Hard_Mixed",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))
    
    ifelse('Hard_Mixed' %in% x$Habitat_Type == FALSE, x <- rbind(x, hard.mixed.dummy), x)
    
    soft.mixed.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Soft_Mixed",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))
    
    ifelse('Soft_Mixed' %in% x$Habitat_Type == FALSE, x <- rbind(x, soft.mixed.dummy), x)
    
    soft.dummy <- data.frame(LineID=x[1,1],ID=1,Project=x[1,3],Region=x[1,4],Survey_Year=x[1,5],Survey_Date=x[1,6],Location=x[1,7],MPA_Group=x[1,8],Type=x[1,9],Designation=x[1,10],Site=x[1,11],Dive=x[1,12],Line=x[1,13],distance_m=0,Invert_Area_m2=0,Habitat_Type="Soft",Avg_Lat=0,Avg_Lon=0,Max_Depth..m.=0,Min_Depth..m.=0,Ave_Depth..m.=0,Hab_Transect_ID="NULL",hard_substrata=0,hard_mixed_substrata=0,soft_mixed_substrata=0,soft_substrata=0,newnames=paste0(x[1,10],x[1,3],x[1,12],x[1,13]))
    
    ifelse('Soft' %in% x$Habitat_Type == FALSE, x <- rbind(x, soft.dummy), x)
    
    final_product <- as.data.frame(x)
    
  })
  
  all.rows <- bind_rows(substrate.divide)
  
  all.rows <- all.rows  %>% 
    group_by_all() %>%
    mutate(newnames=paste0(Designation))
  split.des <- split(df, df$newnames)
  for (I in 1:length(split.des)) {assign(unique(split.des[[I]]$newnames), 
                                         split.des[[I]])}
  
  totals <- lapply(split.des, function(x){
    
    average <- mean(x$Invert_Area_m2)
    sd <- sd(x$Invert_Area_m2)
    substrate <- x[1,16]
    designation <- x[1,10]
    sub.frame <- data.frame(Designation=designation,Habitat=substrate,Average=average,sd=sd)
    
  })
  
  summary <- bind_rows(totals)
  
  rownames <- c("MPA","Reference")
  colnames <- c("Hard","Hard Mixed","Soft_Mixed","Soft")
  pool <- matrix(c(summary[1,3],summary[2,3],summary[4,3],summary[3,3],summary[5,3],summary[6,3],summary[8,3],summary[7,3]),nrow=2,byrow=TRUE,dimnames=list(rownames,colnames))
  pool
  fisher.test(pool)
  p <- fisher.test(pool)$p.value
  Substrate.data <- data.frame(MPA=Title,MPA.Hard.substrate=summary[1,3],MPA.Hard.sd=summary[1,4],MPA.Hard.mixed.substrate=summary[2,3],MPA.Hard.mixed.sd=summary[2,4],MPA.Soft.mixed.substrate=summary[4,3],MPA.Soft.mixed.sd=summary[4,4],MPA.Soft.substrate=summary[3,3],MPA.Soft.sd=summary[3,4],Ref.Hard.substrate=summary[5,3],Ref.Hard.sd=summary[5,4],Ref.Hard.mixed.substrate=summary[6,3],Ref.Hard.mixed.sd=summary[6,4],Ref.Soft.mixed.substrate=summary[8,3],Ref.Soft.mixed.sd=summary[8,4],Ref.Soft.substrate=summary[7,3],Ref.Soft.sd=summary[7,4],n1=n1,n2=n2,x2=NA,p=p)
  
}

Ano.fisher <- Fisher.table(Ano)
Big.fisher <- Fisher.table(Big)
Campus.fisher <- Fisher.table(Campus)
Farallon.fisher <- Fisher.table(Farallon)
Harris.fisher <- Fisher.table(Harris)
Pillar.fisher <- Fisher.table(Pillar)
Pt.Buchon.fisher <- Fisher.table(Pt.Buchon)
Pt.St.George.fisher <- Fisher.table(Pt.St.George)
Portuguese.fisher <- Fisher.table(Portuguese)
Sea.fisher <- Fisher.table(Sea)
Ten.fisher <- Fisher.table(Ten)

Sub.table.results <- rbind(Ano.fisher,Big.fisher,Bodega.table,Campus.fisher,Carrington.table,Farallon.fisher,Farnsworth.table,Gull.table,Harris.fisher,Pillar.fisher,Pt.Arena.table,Pt.Buchon.fisher,Pt.Conception.table,Pt.Lobos.table,Pt.St.George.fisher,Pt.Sur.table,Portuguese.fisher,Reading.table,Sea.fisher,South.Pt.table,Swamis.table,Ten.fisher)
Sub.table.results <- mutate(Sub.table.results, Test = c("Fisher","Fisher","x2","Fisher","x2","Fisher","x2","x2","Fisher","Fisher","x2","Fisher","x2","x2","Fisher","x2","Fisher","x2","Fisher","x2","x2","Fisher"))


depth_compare <- function(df){
  
  df <- df %>% distinct()
  
  set.seed(55)
  Title <- df[1,8]
  
  df <- df %>% 
    group_by_all() %>%
    mutate(newnames=paste0(Project,Designation,Dive,Line))
  split.line <- split(df, df$newnames)
  for (I in 1:length(split.line)) {assign(unique(split.line[[I]]$newnames), 
                                          split.line[[I]])}
  
  summary.col <- lapply(split.line, function(x){
    
    sum.area <- sum(x$Invert_Area_m2)
    x <- mutate(x, proportion = x$Invert_Area_m2/sum.area)
    x <- mutate(x, sub.depth = x$proportion*x$Avg_Depth..m.)
    x <- mutate(x, w.avg = sum(x$sub.depth))
    
  })
  
  weighted <- bind_rows(summary.col)
  weighted.summary <- subset(weighted, select = -c(1:9,11:29))
  weighted.summary <- weighted.summary %>% distinct()
  weighted.summary[is.na(weighted.summary)] <- 0
  
  MPA <- subset(weighted.summary,Designation=="MPA")
  Ref <- subset(weighted.summary,Designation=="Reference")
  
  p1=MPA$w.avg
  p2=Ref$w.avg
  b=weighted.summary$w.avg 
  n=length(b)
  n1 = length(p1)
  n2 = length(p2)
  s1 = sd(p1)
  s2 = sd(p2)
  pooledVar=(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
  signal = (1/((1/n1)+(1/n2)))*(mean(p1)-mean(p2))^2
  calcSN =  signal/pooledVar
  trials = 1000
  SN <- as.vector(NULL)
  for (i in 1:trials) {
    bs.H <- sample(b, n1, replace = TRUE)
    bs.C <- sample(b, n2, replace = TRUE)
    bs.s1 = sd(bs.H)
    bs.s2 = sd(bs.C)
    bs.pooledVar=(((n1-1)*bs.s1^2+(n2-1)*bs.s2^2)/(n1+n2-2))
    bs.signal = (1/((1/n1)+(1/n2)))*(mean(bs.H)-mean(bs.C))^2
    SN[i] = bs.signal/bs.pooledVar}
  hist(SN) # plot (good habit)
  bs.p = (sum(SN > calcSN))/trials # p-value.
  label0="S:N:"
  label2="P-value from bootstrap_m1:"
  
  sprintf("%s %f", label0,calcSN)
  
  sprintf("%s %f", label2,bs.p)
  
  depth.results <- data.frame(MPA=Title,Avg_MPA_depth=mean(p1),MPA_depth_sd=s1,Avg_Ref_depth=mean(p2),Ref_depth_sd=s2,MPA_n=n1,Ref_n=n2,SN=calcSN,p=bs.p)
  
  
}

Ano.depth <- depth_compare(Ano)
Big.depth <- depth_compare(Big)
Bodega.depth <- depth_compare(Bodega)
Campus.depth <- depth_compare(Campus)
Carrington.depth <- depth_compare(Carrington)
Farallon.depth <- depth_compare(Farallon)
Farnsworth.depth <- depth_compare(Farnsworth)
Gull.depth <- depth_compare(Gull)
Harris.depth <- depth_compare(Harris)
Montara.depth <- depth_compare(Montara) # NULL
Pillar.depth <- depth_compare(Pillar)
Pt.Arena.depth <- depth_compare(Pt.Arena)
Pt.Buchon.depth <- depth_compare(Pt.Buchon)
Pt.Conception.depth <- depth_compare(Pt.Conception)
Pt.Lobos.depth <- depth_compare(Pt.Lobos)
Pt.St.George.depth <- depth_compare(Pt.St.George) #Fisher
Pt.Sur.depth <- depth_compare(Pt.Sur)
Portuguese.depth <- depth_compare(Portuguese) #Fisher
Reading.depth <- depth_compare(Reading)
Sea.depth <- depth_compare(Sea) #Fisher
South.depth <- depth_compare(South)
South.Pt.depth <- depth_compare(South.Pt.)
Swamis.depth <- depth_compare(Swamis)
Ten.depth <- depth_compare(Ten) #Fisher

All.depth.results <- rbind(Ano.depth,Big.depth,Bodega.depth,Campus.depth,Carrington.depth,Farallon.depth,Farnsworth.depth,Gull.depth,Harris.depth,Pillar.depth,Pt.Arena.depth,Pt.Buchon.depth,Pt.Conception.depth,Pt.Lobos.depth,Pt.St.George.depth,Pt.Sur.depth,Portuguese.depth,Reading.depth,Sea.depth,South.depth,South.Pt.depth,Swamis.depth,Ten.depth)
write.csv(All.depth.results, file="Depth.t.test.results.csv")

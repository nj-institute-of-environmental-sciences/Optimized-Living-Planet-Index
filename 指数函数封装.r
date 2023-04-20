
#########################多年筛样线###################
#################输入data，x1，l 得到line####################
# 
# rm(list=ls())
# ll = "C:\\Users\\LW\\Desktop\\Index\\两栖2015年物种个体数.csv"
# data = read.csv(ll,sep=',',header = TRUE)
# ld="C:\\Users\\LW\\Desktop\\Index\\"
# x1 =c(2014:2017)
# l = "两栖"

fun = function(data, x1, l, ld, sss){
  sss = as.numeric(sss)
  data3=unique(data)
  line=unique(data3$样线编码)
  #for(i in 1:length(x1)){
  #  data1=sub=subset(data3,data3$年份==x1[i]&data3$样线编码%in%line)
  #  line=unique(data1$样线编码)
  #}
  ##################？内存area#################
  area=read.csv(file =paste0(ld, "area.csv"))
  #############################计算数量,输出abc[y]############
  abc=list()
  
  for(y in 1:length(x1)){
    # print(y)
    data=subset(data3,data3$年份==x1[y]&data3$样线编码%in%line)
    
    if(l==c("两栖")){
      data=data[c("年份","物种名","长度","宽度","省份","物种数量")]
    }
    if(l==c("鸟类")){
      data=data[c("年份","物种名","长度","省份","物种数量")]
    }
    data=na.omit(data)
    names=unique(data$物种名)
    names=as.matrix(names)
    
    
    #######建立存储数量的矩阵###########
    ###########所有物种的数量###########
    num=matrix(NA,nrow=length(names),ncol=2)
    #########按物种分类
    for(i in 1:length(names)){
      species=subset(data,物种名==names[i])
      #########选取省份########
      provi=unique(species$省份)
      provi=as.matrix(provi)
      pro_num0=NA
      if(sss!=-1){
        if(l==c("两栖")){
          densi=sum(species$物种数量)/sum(species$长度*species$宽度)
        }
        if(l==c("鸟类")){
          densi=sum(species$物种数量)/sum(species$长度*200)
        }
        
        ########################计算面积###########################
        # pro_num=subset(area,province==provi[j])$aera*densi*1000
        num[i,1]=as.matrix(names[i])
        num[i,2]=ceiling(densi*sss)
      }else{
        
        for(j in 1:length(provi)){ 
          ######################if 是两栖##############
          if(l==c("两栖")){
            species2=subset(species,省份==provi[j])
            densi=sum(species2$物种数量)/sum(species2$长度*species2$宽度)
          }
          if(l==c("鸟类")){
            species2=subset(species,省份==provi[j])
            densi=sum(species2$物种数量)/sum(species2$长度*200)
          }
          
          ########################计算面积###########################
          # pro_num=subset(area,province==provi[j])$aera*densi*1000
          pro_num=subset(area,province==provi[j])$aera*densi*1000
          pro_num0=rbind(pro_num,pro_num0)
        }
        num[i,1]=as.matrix(names[i])
        num[i,2]=ceiling(sum(as.numeric(pro_num0[,1]),na.rm=TRUE) )
      }
    }
    #######################输出该年计算得到的数量#####################
    # file1=paste(x1[y],"num.csv")
    # write.csv(num,file1)  
    abc[[y]]=num
    # ll = paste0(ld, "index/")
    # ll = paste0(ll, l)
    # ll = paste0(ll, x1[y])
    # ll = paste0(ll, "年物种个体数.csv")
    # write.csv(num, ll)
  }
  
  exp_other=matrix(0,ncol=6,nrow=length(x1))
  colnames(exp_other) <- c("year","ent","s","logn","改进1","改进2")
  
  exp_other=as.data.frame(exp_other)
  ################读取文件#############
  
  for(y in 1:length(x1)){
    
    data=abc[[y]]
    data=as.data.frame(data)
    n=sum(as.numeric(as.character(data$V2)))
    exp_other$logn[y]=log(n,10)
    p=as.numeric(as.character(data$V2))/n
    a=sum(log(p,10))/length(p)+log(n,10)
    exp_other$year[y]=x1[y]
    exp_other$ent[y]=sum(-p*log(p))
    exp_other$s[y]=length(p)
    exp_other$改进1[y]=(sum(-p*log(p))+log(n,10))
    exp_other$改进2[y]=(a+sum(-p*log(p)))
  }
  # print(nrow(exp_other))
  # print(exp_other["改进1"])
  # print(class(exp_other))
  ##################输出exp_other########################
  return(exp_other)
}

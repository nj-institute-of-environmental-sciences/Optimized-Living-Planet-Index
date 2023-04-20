#########################多年筛样线###################
#################输入data，x1，x2,q 得到line####################

# ll = "C:\\Users\\lh\\Desktop\\code_data\\code_data\\py.csv"
# data = read.csv(ll,sep=',',header = TRUE)
# x1 = c(2011,2018)
# x2 = 2011:2018
# q = 5
# l = "两栖"
# ld = "C:\\Users\\lh\\Desktop\\code_data\\code_data\\"
# sss=100000

fun1 = function(data, x1, x2, q, l, ld, sss){
  # x1是检验的两年，x2是之前选的年份，q是随机的次数
  sss = as.numeric(sss)
  data3=unique(data)
  line=unique(data$样线编码)
  q =  as.numeric(q)
  
  # x2=c(2012:2017)
  
  # for(i in 1:length(x2))
  # {
  #   data1=sub=subset(data3,data3$年份==x2[i]&data3$样线编码%in%line)
  #   line=unique(data1$样线编码)
  # }
  
  ##################？内存area#################
  area=read.csv(file =paste0(ld, "area.csv"))
  ################################################循环生成指数
  # q
  text1=c(1:q)
  text2=c(1:q)
  
  for(k in 1:q)
  {
     print(k)
    #lines=sample(line,size=round(0.8*as.numeric(length(line))))
    data=subset(data3,data3$年份==x1[1])
    line=unique(data$样线编码)
    lines=sample(line,size=round(0.8*as.numeric(length(line))))
    data=subset(data3,data3$样线编码%in%lines)
    print(x1[1])
    
    
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
    for(i in 1:length(names))
    {
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
        for(j in 1:length(provi))
        {  ######################if 是两栖##############
          if(l==c("两栖")){
              species2=subset(species,省份==provi[j])
              densi=sum(species2$物种数量)/sum(species2$长度*species2$宽度)
            }
            if(l==c("鸟类")){
              species2=subset(species,省份==provi[j])
            densi=sum(species2$物种数量)/sum(species2$长度*200)
          }
          ########################计算面积###########################
          pro_num=subset(area,province==provi[j])$aera*densi*1000
          pro_num0=rbind(pro_num,pro_num0)
        }
        num[i,1]=as.matrix(names[i])
        num[i,2]=ceiling(sum(as.numeric(pro_num0[,1]),na.rm=TRUE) )
      }
    }
    ##################计算指数##############
    n=sum(as.numeric(num[,2]))
    logn=log(n,10)
    p=as.numeric(num[,2])/n
    ent=sum(-p*log(p))
    改进1=((sum(-p*log(p))+log(n,10)))
    text1[k]=改进1
    
    
   
    
    data=subset(data3,data3$年份==x1[2])
    line=unique(data$样线编码)
    lines=sample(line,size=round(0.8*as.numeric(length(line))))
    data=subset(data3,data3$样线编码%in%lines)
    print(x1[2])
    
    
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
    for(i in 1:length(names))
    {
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
        
        #######################计算面积###########################
        # pro_num=subset(area,province==provi[j])$aera*densi*1000
        num[i,1]=as.matrix(names[i])
        num[i,2]=ceiling(densi*sss)
      }else{
        for(j in 1:length(provi))
          
        {  ######################if 是两栖##############
          if(l==c("两栖")){
            species2=subset(species,省份==provi[j])
            densi=sum(species2$物种数量)/sum(species2$长度*species2$宽度)
          }
          if(l==c("鸟类")){
            species2=subset(species,省份==provi[j])
            densi=sum(species2$物种数量)/sum(species2$长度*200)
          }
          
          
          ########################计算面积###########################
          pro_num=subset(area,province==provi[j])$aera*densi*1000
          pro_num0=rbind(pro_num,pro_num0)
        }
        num[i,1]=as.matrix(names[i])
        num[i,2]=ceiling(sum(as.numeric(pro_num0[,1]),na.rm=TRUE) )
      }
    }
    ##################计算指数##############
    n=sum(as.numeric(num[,2]))
    logn=log(n,10)
    p=as.numeric(num[,2])/n
    
    ent=sum(-p*log(p))
    改进1=((sum(-p*log(p))+log(n,10)))
    text2[k]=改进1
    incProgress(1/q)
  }
  print("前一年随机抽样(text1)，指标数据：")
  print(summary(text1))
  print("后一年随机抽样(text2)，指标数据：")
  print(summary(text2))
  a=wilcox.test(text1,text2,paried=TRUE)
  return(a)
}



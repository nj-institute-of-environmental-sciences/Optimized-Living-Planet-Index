rm(list=ls())
link = 'C:/Users/LW/Desktop/Index/py.csv'
file = read.table(link, sep=",", header=TRUE)

point = array(0,dim = c(nrow(file),2))
for(i in 1:nrow(file)){
  point[i,1] = file[i, "经度"]
  point[i,2] = file[i, "纬度"]
}
point = unique(point)

n = 1
for(i in 1:nrow(point)){
  if(point[n,2]>point[i,2]){
    n = i
  }
}

p0 = point[n,]
point = point[-n,]

compare = function(a, b){
  # b做参考，返回的TRUE是小于b的部分
  x = c(a[1]-p0[1], a[2]-p0[2])
  y = c(b[1]-p0[1], b[2]-p0[2])
  dx = (x[1]^2+x[2]^2)^0.5
  dy = (y[1]^2+y[2]^2)^0.5
  cosa = x[1]/dx
  cosb = y[1]/dy
  if(cosa<cosb){
    return(FALSE)
  }else if(cosa>cosb){
    return(TRUE)
  }else{
    if(dx<dy){
      return(TRUE)
    }else if(dx>dy){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}



quick = function(point){
  if(is.null(nrow(point)) || is.null(point) || nrow(point)==0 ){
    return(point)
  }
  pivot = point[1,]
  therest = point[-1,]
  if(is.null(nrow(therest))){
    if(compare(therest,pivot)==TRUE){
      sv1 = therest
      sv2 = NULL
    }else{
      sv1 = NULL
      sv2 = therest
    }
  }else{
    sv1 = therest[apply(therest,1,compare,b=pivot),]
    sv2 = therest[!apply(therest,1,compare,b=pivot),]
  }
  sv1 = quick(sv1)
  sv2 = quick(sv2)
  return(rbind(sv1, pivot, sv2))
}

#aa = matrix(c(9,8,2,1,3,5,13,4,12,9,6,9),6,2)

point = quick(point)
point = rbind(p0, point, p0)

ep = point
#ep=aa
tag = 0
while(tag==0){
  tag = 1
  l = nrow(ep)
  for(i in 1:l){
    i1 = i
    if((i+1)%%l==0){
      i2 = l
    }else{
      i2= (i+1)%%l
    }
    if((i+2)%%l==0){
      i3 = l
    }else{
      i3 = (i+2)%%l
    }
    x = ep[i1,]
    y = ep[i2,]
    z = ep[i3,]
    a1 = c(y[1]-x[1],y[2]-x[2])
    a2 = c(z[1]-y[1],z[2]-y[2])
    if(a1[1]*a2[2]-a1[2]*a2[1]<0){
      tag = 0
      ep = ep[-i2,]
      break
    }else if(a1[1]*a2[2]-a1[2]*a2[1]==0 && a1[1]*a2[1]<0){
      tag = 0
      ep = ep[-i2,]
      break
    }
  }
}


ComputeAera = function(arr){
  arr_len = length(arr)
}



l=10
for(i in 1:l){
  i1 = i
  if((i+1)%%l==0){
    i2 = l
  }else{
    i2= (i+1)%%l
  }
  if((i+2)%%l==0){
    i3 = l
  }else{
    i3 = (i+2)%%l
  }
  print(c(i1,i2,i3))
}



year = unique(data[,'年份'])
a1 = unique(data[data['年份']==2012,'样线编码'])
a2 = unique(data[data['年份']==2013,'样线编码'])
a3 = unique(data[data['年份']==2014,'样线编码'])
a4 = unique(data[data['年份']==2015,'样线编码'])
a5 = unique(data[data['年份']==2016,'样线编码'])
a6 = unique(data[data['年份']==2017,'样线编码'])
b1 = intersect(a1,a2)
b2 = intersect(b1,a3)
b3 = intersect(b2,a4)
b4 = intersect(b3,a5)
b5 = intersect(b4,a6)
dd = data[data$样线编码 %in% b5,]

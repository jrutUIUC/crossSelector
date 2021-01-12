library(crossSelector)
library(wesanderson)
library(ggplot2)
data(crossRank)


males<- c(8, 25)
females<- c(28, 28)
crossesMade<- c('25x28','16x25', '14x25', '39x34', '39x25', '3x14', '14x34', '30x34', '7x36', '36x25', '39x25', '38x25', '28x14', '38x36', '39x7', '14x34', '39x36', '36x39', '7x36',
                '29x7', '30x14', '14x34', '38x25', '38x25', '25x39', '30x36','3x14', '36x14')
maxRepeat<- 5


#function to match parents
matchParents<- function(q, males, females, crossesMade, maxRepeat){
  #track how many times crosses were made in the table
  combMade<- data.frame(t(matrix(as.numeric(unlist(strsplit(crossesMade, split='x'))), nrow=2)))
  colnames(combMade)<- c('V1no', 'V2no')
  combMade2<-combMade[,c(2,1)]; colnames(combMade2)<- colnames(combMade)
  combMade<- rbind(combMade, combMade2)
  for(i in 1:nrow(combMade)){
    rowIx<- intersect(which(combMade$V1no[i]==q$V1no), which(combMade$V2no[i]==q$V2no))
    q[rowIx, 'timesMade']<- q[rowIx, 'timesMade']+1
  }

  tball<- q
  tball$timesMade<- as.character(tball$timesMade)
  tball$timesMade[which(tball$timesMade=='0')]<- ""

   #make image
  tball$V1no<- factor(as.character(tball$V1no), levels=as.character(unique(sort(tball$V1no))))
  tball$V2no<- factor(as.character(tball$V2no), levels=as.character(unique(sort(tball$V2no))))
  pal <- wes_palette("Zissou1", 100, type = "continuous")
  p <- ggplot(tball, aes(V1no, V2no)) + geom_tile(aes(fill = nmvec), colour = "black") +
    geom_text(aes(label = timesMade)) +
    scale_fill_gradientn(colours =pal, na.value="white")


  #eliminate the value of crosses already repeated 'maxRepeat' number of times
  ixrm<- which(q$timesMade>=maxRepeat)
  if(length(ixrm)>0){
    q<- q[-ixrm,]
  }

  #determine which crosses to make
  V1no<- c()
  V2no<- c()
  while(length(males)>0 & length(females)>0){
    top<- idTop(males, females, q)
    if(nrow(na.omit(top))>0){
      maleRm<- top[1,'V2no']
      femaleRm<- top[1,'V1no']
      V1no<- append(V1no, femaleRm)
      V2no<- append(V2no, maleRm)
      ixrm1<- match(maleRm, males)
      ixrm2<- match(femaleRm, females)
      males<- males[-ixrm1]
      females<- females[-ixrm2]
    }else{
      break
    }
  }
  q1<- data.frame(V1no, V2no)
  tab<- merge(q1, q, by=c('V1no', 'V2no'), all.x=TRUE)
  tab<- tab[order(tab$rank, decreasing=FALSE),]
  tab$V1no<- as.character(tab$V1no)
  tab$V2no<- as.character(tab$V2no)

  #Add crosses to plot
  frames <- data.frame(V1no = c(tab$V1no,tab$V2no),
                       V2no = c(tab$V2no, tab$V1no))
  p<- p+ geom_tile(data=frames,fill="black", width=0.3)
  p<- p+ geom_tile(data=frames,fill="black", height=0.3)

  #return results
  return(list(tab=tab, p=p))

}

matchParents()


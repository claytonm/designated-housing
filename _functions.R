# reverse a string
strReverse <- function(x){
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}

thousands_sep = function(num){
  num=as.character(round(num,0))
  num=strReverse(num)
  chars=nchar(num)
  if(chars<=3){return(strReverse(num))}
  if(chars>3 && chars<=6){
    num=paste(substr(num,start=1,stop=3),",",
              substr(num,start=4,stop=chars),sep="")
    return(strReverse(num))
  }
  if(chars>6 && chars<=9){
    num=paste(substr(num,start=1,stop=3),",",
              substr(num,start=4,stop=6),",",
              substr(num,start=7,stop=chars),sep="")
    return(strReverse(num))
  }
}

thousands_sep_neg=function(num){
  if(is.na(num)){return(NA)}
  neg=num<0
  if(neg){
    num=abs(num)
    num=thousands_sep(num)
    num=paste("-",num,sep="")
    return(num)
  } else{
    num=thousands_sep(num)
    return(num)
  }
}

# applys *thousands_*sep to a *vector 
tsv = function(v){
  return(sapply(v,thousands_sep_neg))
}


round_char = function(num,digs=1,percent=FALSE){
  num = as.character(round(num,digs))
  dot=grepl("\\.",num)
  if(digs!=0 && !dot){
    num=paste(num,".0",sep="")
  }
  if(percent){
    return(paste(num,"%",sep=""))
  }else{
    return(num)
  }
}

rcv = function(v,...){
  return(sapply(v,round_char,...))
}

appf=function(df,FUN,...){
  # fun=get(FUN)
  n=ncol(df)
  if(n>1){
    return(as.data.frame(lapply(df,FUN,...)))
  }else{
    return(sapply(df,FUN,...))
  }
}

# prettyTable
# combine 
prettyTable = function(df,
                       counts = vector(length = 0),
                       pcts = vector(length = 0),
                       digits = 1){
     if (length(counts) > 0){
          df[,counts]=appf(df[,counts],tsv)
     }
     if (length(pcts) > 0){
          df[,pcts]=appf(df[,pcts],rcv,digs=1)
     }
     return(df[,c(counts,pcts)[order(c(counts,pcts))]])
}

g.b = function(data = data,
               x = x,
               breaks = seq(from = 0, to = 200, by = 10),
               xmin = 0,
               xmax = 100,
               ymin = 0,
               ymax = 1,
               title = "",
               xlab = "",
               ylab = "",
               title_size = 14,
               axis_text_size = 8,
               axis_title_size = 12){
     labelMaker = function(ymax){
          labelNums = seq(from = 0, to = ymax, by = 0.1)
          labelNums = labelNums*100
          labelNums = paste(labelNums, "%", sep = "")
          evenOdd = (length(labelNums) + 1)%%2
          for (i in 0:length(labelNums)){
               if (i%%2 == evenOdd){
                    labelNums[i] = ""
               }
          }
          return(labelNums)
     }
     
     gg = ggplot(data = data,
                 aes_string(x = x)) +
          geom_histogram(aes(y = ..count../sum(..count..)),
                         breaks = breaks,
                         fill = "#BFBFBF",
                         color = "black") +
          xlim(xmin,xmax) +
          theme_minimal(title_size = title_size,
                        axis_text_size = axis_text_size,
                        axis_title_size = axis_title_size) +
          theme(legend.position = "none",
                text = element_text(family = "twCent")) +
          labs(x = xlab,
               y = ylab,
               title = title) +
          scale_y_continuous(limits = c(ymin, ymax),
                             breaks = seq(from = 0, to = ymax, by = .1),
                             labels = labelMaker(ymax))
     return(gg)
}


g.b.race = function(data = data,
               x = x,
               breaks = seq(from = 0, to = 200, by = 10),
               xmin = 0,
               xmax = 100,
               ymin = 0,
               ymax = 1,
               title = "",
               xlab = "",
               ylab = "",
               title_size = 14,
               axis_text_size = 8,
               axis_title_size = 12){
     gg = ggplot(data = data,
                 aes_string(x = x)) +
          geom_histogram(aes(y = ..count../sum(..count..)),
                         breaks = breaks,
                         fill = "#BFBFBF",
                         color = "black") +
          xlim(xmin,xmax) +
          theme_minimal(title_size = title_size,
                        axis_text_size = axis_text_size,
                        axis_title_size = axis_title_size) +
          theme(legend.position = "none",
                text = element_text(family = "twCent")) +
          labs(x = xlab,
               y = ylab,
               title = title) +
          scale_y_continuous(limits = c(ymin, ymax),
                             breaks = seq(from = 0, to = ymax, by = .1),
                             labels = c("0%", "", "20%", "", "40%", "", "60%", "", "80%", "", "100%"))
     return(gg)
}



g.b.transit = function(data = data,
               x = x,
               breaks = seq(from = 0, to = 200, by = 10),
               xmin = 0,
               xmax = 100,
               ymin = 0,
               ymax = 1,
               title = "",
               xlab = "",
               ylab = "",
               title_size = 14,
               axis_text_size = 8,
               axis_title_size = 12){
     
     labelMaker = function(ymax){
          labelNums = seq(from = 0, to = ymax, by = 0.1)
          labelNums = labelNums*100
          labelNums = paste(labelNums, "%", sep = "")
          evenOdd = (length(labelNums) + 1)%%2
          for (i in 0:length(labelNums)){
               if (i%%2 == evenOdd){
                    labelNums[i] = ""
               }
          }
          return(labelNums)
     }
     
     gg = ggplot(data = data,
                 aes_string(x = x)) +
          geom_histogram(aes(y = ..count../sum(..count..)),
                         breaks = breaks,
                         fill = "#BFBFBF",
                         color = "black") +
          xlim(xmin,xmax) +
          theme_minimal(title_size = title_size,
                        axis_text_size = axis_text_size,
                        axis_title_size = axis_title_size) +
          theme(legend.position = "none",
                text = element_text(family = "twCent")) +
          labs(x = xlab,
               y = ylab,
               title = title) +
          scale_y_continuous(limits = c(ymin, ymax),
                             breaks = seq(from = 0, to = ymax, by = .1),
                             labels = labelMaker(ymax))
     return(gg)
}

leftPad = function(x, l = 5, pad = "0"){
     for (i in 1:length(x)){
          while (nchar(x[i]) < l){
               x[i] = paste(pad, x[i], sep = "")
          }
     }
     return(x)
}

ExportPlot <- function(gplot, filename, figuresFolder = "../figsForPew/", width=10, height=8.5) {
     # Export plot in PDF and EPS.
     # Notice that A4: width=11.69, height=8.27
     png(file = paste(figuresFolder, filename, '.png', sep=""), width = width * 100, height = height * 100)
     print(gplot)
     dev.off()
}





removeOutliers = function(x) { 

  qnt = quantile(x, probs=c(.25, .75))
  
  iqt = 1.5 * IQR(x)
  
  y = x 
  y[x < (qnt[1] - iqt)] = NA
  y[x > (qnt[2] + iqt)] = NA

  return(y[complete.cases(y)])
}

removezero = function(x){
  semzero_rq2 <- c()
  for (i in 1:length(x)){
    if (x[i] != 0){
      semzero_rq2 = c(semzero_rq2, x[i])
    }
  }
  return(semzero_rq2)
}


violinplotALL <-function(data){
	library(ggplot2)
  	library(scales)

	plt_wool <- ggplot(subset(data, value > 0) , aes(x=variable,y=value)) + 
    geom_violin(aes(fill = variable)) +stat_summary(fun.data="mean_sdl", geom="crossbar", width=0.002 ) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    theme_gray() + theme(axis.text.x=element_text(face="plain", color="#000000", 
                                                  size=10, angle=90, vjust=0.2), axis.text.y = element_text(size=12)) +
    theme(axis.title=element_text(size=12,face="bold")) +
    labs(x = "", y = "Horas") +
    annotation_logticks(sides = "rl") +
    theme(panel.grid.minor = element_blank()) +
    guides(title.hjust=0.5) +
    theme(plot.margin=unit(c(0,1,0,0),"mm"))
  
  	yp <- subset(data, value>0)             # Choosing only +ve values in col x
  	sts <- boxplot.stats(yp$value)$stats  # Compute lower and upper whisker limits
  
  	p1 = plt_wool + coord_cartesian(ylim = c(sts[2]/200,max(sts)*70.05))
  
  	p1
  	p1+geom_boxplot(width=0.05)

}

gendercivil <-  function(dat){
	nm <- c()
	separated <- c()
	divorced <- c()

	wind <- c()
	msa <- c()
	mcs <- c()
	sep <- c()

	for (i in 1:length(dataset$sex)){
    	if(dataset$sex[i] == "Female"){
    		print(dataset$maritalstatus[i])
    		if(dataset$maritalstatus[i] == "Divorced"){
    			divorced <- c(divorced, dataset$maritalstatus[i])
    		}
    		if(dataset$maritalstatus[i] == "Never-married"){
    			nm <- c(nm, dataset$maritalstatus[i])
    		}

    		if(dataset$maritalstatus[i] == "Widowed"){
    			wind <- c(wind, dataset$maritalstatus[i])
    		}
    		if(dataset$maritalstatus[i] == "Married-spouse-absent"){
    			msa <- c(msa, dataset$maritalstatus[i])
    		}

    		if(dataset$maritalstatus[i] == "Married-civ-spouse"){
    			mcs <- c(mcs, dataset$maritalstatus[i])
    		}

    		if(dataset$maritalstatus[i] == "Separated"){
    			sep <- c(sep, dataset$maritalstatus[i])
    		}
    	}
	}

	length(nm)
	length(separated)
	length(divorced)
	length(wind)
	length(msa)
	length(mcs)
	length(sep)	


	status <- c("nm", "separated", "divorced", "wind", "msa", "mcs", "sep")
	values <- c(length(nm),	length(separated),	length(divorced),	length(wind),	length(msa),	length(mcs),	length(sep))
	# Create a data frame from the vectors
	Female_df <- data.frame(status, values)
	print(Female_df)
	


}

classmenos50 <-  function(dat){
	#profM <- c()
	ganho <- c()
	classe <- c()
	

	for (i in 1:length(dataset$class)){
    	if(dataset$class[i] == "<=50K"){
    		#pfunrofM <- c(profM, dataset$ocupation[i])
    	  if (dataset$capitalgain[i] != 0){
      		ganho <- c(ganho, dataset$capitalgain[i])
  				classe <- c(classe, dataset$class[i])}
    	}
	}

	#length(profM)
  
	rq2b <- data.frame(ganho, classe)
	return(rq2b)
}

classmais50 <-  function(dat){
	#profM <- c()
	ganho <- c()
	classe <- c()
	

	for (i in 1:length(dataset$class)){
    	if(dataset$class[i] == ">50K"){
    		#profM <- c(profM, dataset$ocupation[i])
    	  if (dataset$capitalgain[i] != 0){
    		ganho <- c(ganho, dataset$capitalgain[i])
				classe <- c(classe, dataset$class[i])
				}
    	}
	}

	#length(profM)

	rq2b <- data.frame(ganho, classe)
	return(rq2b)
}

race_education <-  function(dat){
  #profM <- c()
  race <- c()
  education <- c()
  
  
  for (i in 1:length(dataset$race)){
    if(dataset$race[i] == "Asian-Pac-Islander"){
      #profM <- c(profM, dataset$ocupation[i])
      
        race <- c(race, dataset$race[i])
        education <- c(education, dataset$education[i])
      
    }
  }
  
  #length(profM)
  
  rq5a <- data.frame(race, education)
  return(rq5a)
}

mais50_education <-function(){
  #profM <- c()
  class <- c()
  education <- c()
  age <- c()
  ocupation <- c()
  
  
  for (i in 1:length(dataset$education)){
    if(dataset$class[i] == ">50K"){
      class <- c(class, dataset$class[i])
      education <- c(education, dataset$education[i])
    }
  }
  rq5a <- data.frame(education, age)
  return(rq5a)
}

education_age <-function(){
  #profM <- c()
  education <- c()
  age <- c()
  for (i in 1:length(dataset$education)){
    if(dataset$education[i] == "Prof-school"){
      education <- c(education, dataset$education[i])
      age <- c(age, dataset$age[i])
    }
  }
  
  rq5a <- data.frame(education, age)
  return(rq5a)
}

HS_inconcluido <- function(){
  hs_inc = c()
  th10 = c()
  th11 = c()
  th12 = c()
  st1_st4 = c()
  th7_th8 = c()
  th9 = c()
  preschool = c()
  age =c()
  
  for (i in 1:length(dataset$education)){
    if(dataset$education[i] == "10th"){
      th10 <- c(th10, dataset$education[i])
      age = c(age, dataset$age[i])
    }
    if(dataset$education[i] == "11th"){
      th11 <- c(th11, dataset$education[i])
      age = c(age, dataset$age[i])
    }
    if(dataset$education[i] == "12th"){
      th12 <- c(th12, dataset$education[i])
      age = c(age, dataset$age[i])
    }
    if(dataset$education[i] == "1st-4th"){
      st1_st4 <- c(st1_st4, dataset$education[i])
      age = c(age, dataset$age[i])
    }
    if(dataset$education[i] == "7th-8th"){
      th7_th8 <- c(th7_th8, dataset$education[i])
      age = c(age, dataset$age[i])
    }
    if(dataset$education[i] == "9th"){
      th9 <- c(th9, dataset$education[i])
      age = c(age, dataset$age[i])
    }
    if(dataset$education[i] == "Preschool"){
      preschool <- c(preschool, dataset$education[i])
      age = c(age, dataset$age[i])
    }
  }
  
  hs_inc = c(th10, th11, th12, st1_st4, th7_th8, th9, preschool)
  
  rq5b <- data.frame(hs_inc, age)
  return(rq5b)
  
}



contador <- function(){
  cont = 0
  for(i in 1:length(dataset$class)){
   if(dataset$class[i] == "<=50K"){
         cont = cont +1
     }
  }
  print(cont)
}


dfclass50 = data.frame(menos50 = c(1652,3263,8,3170,2098,879,1284,1752,3158,148,2281,438,2667,645,1277), mais50 = c(191,507,1,929,1968,115,86, 250,137,1,1859, 211,983,283,320))

rq4_new <-  function(){
  capitalgain <- c()	
  countzero <- 0
  for (i in 1:length(dataset$capitalgain)){
    if(dataset$capitalgain[i] != 0){
      capitalgain <- c(capitalgain, dataset$capitalgain[i])
    }
    if(dataset$capitalgain[i] == 0){
      countzero = countzero +1
    }
  }
  print(countzero)
  print(length(capitalgain))
  #capitalgain <-  na.omit(rmvoutlier(capitalgain))
  print(mean(capitalgain))
  print(var(capitalgain))
  print(skewness(capitalgain))
  print(kurtosis(capitalgain))
  
  df <- data.frame(removeOutliers( capitalgain))
  
  
  ggplot(df, aes(x=removeOutliers( capitalgain))) +
    geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=1000)+
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(removeOutliers( capitalgain))), color="blue",
               linetype="dashed")+ 
    geom_vline(aes(xintercept=median(removeOutliers( capitalgain))), color="red",
               linetype="dashed")+
    labs(title="",x="Capital Gain", y = "Count")
  
}



rq4 <-  function(dat){
	Husband <- c()
	Notinfamily <- c()
	Otherrelative <- c()
	Ownchild <- c()
	Unmarried <- c()
	Wife <- c()
	

	for (i in 1:length(dataset$relationship)){
    	if(dataset$relationship[i] == "Husband"){
    		Husband <- c(Husband, dataset$hoursperweek[i])
    	}
    	if(dataset$relationship[i] == "Not-in-family"){
    		Notinfamily <- c(Notinfamily, dataset$hoursperweek[i])
    	}
    	if(dataset$relationship[i] == "Other-relative"){
    		Otherrelative <- c(Otherrelative, dataset$hoursperweek[i])
    	}
    	if(dataset$relationship[i] == "Own-child"){
    		Ownchild <- c(Ownchild, dataset$hoursperweek[i])
    	}
    	if(dataset$relationship[i] == "Unmarried"){
    		Unmarried <- c(Unmarried, dataset$hoursperweek[i])
    	}
    	if(dataset$relationship[i] == "Wife"){
    		Wife <- c(Wife, dataset$hoursperweek[i])
    	}
	}


	Husbandv <- data.frame(value=rmvoutlier(Husband), variable ="Husband")
	Notinfamilyv <- data.frame(value=rmvoutlier(Notinfamily), variable ="Not in Family")
	Otherrelativev <- data.frame(value=rmvoutlier(Otherrelative), variable ="Other relative")
	Ownchildv <- data.frame(value=rmvoutlier(Ownchild), variable ="Own child")
	Unmarriedv <- data.frame(value=rmvoutlier(Unmarried), variable ="Unmarried")
	Wifev <- data.frame(value=rmvoutlier(Wife), variable ="Wife")

	datarq4 <- rbind(Husbandv, Notinfamilyv, Otherrelativev, Ownchildv, Unmarriedv, Wifev)


}


rmvoutlier <- function(C){
  x <- C
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1] 
  x[x > (qnt[2] + H)] <- caps[2] 
  
  return(x)
}


p <- ggplot(data=Female_df, aes(x=status, y=values)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=values), vjust=-0.3, color="black", size=3.5)+
    theme_minimal()

    p + theme(axis.text.x = element_text(face="bold", 
                                     size=11, angle=25),
          axis.text.y = element_text(face="bold", 
                                     size=12))

# grafico de bolinha
    
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)

# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges=flare$edges
vertices = flare$vertices
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Hide the first level (right)
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE") 

# Second one: add 2 first levels
ggraph(mygraph, layout = 'circlepack', weight="size") + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = "white", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "white", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE")


# gráfico de bubble

library(ggplot2)
library(plotly)
library(gapminder)

p <- dataset %>%
  filter(education=="Bachelors") %>%
  ggplot( aes(age,hoursperweek)) +
  geom_point() +
  scale_x_log10() +
  theme_bw()

ggplotly(p)






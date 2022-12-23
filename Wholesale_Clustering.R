rm(list=ls())
dev.off()

library(scales)
library(NbClust)
library(purrr)
library(ggplot2)
library(dplyr)
library(corrplot)


#loading the data
data=read.csv('wholesale.csv', header=TRUE, sep=',')

data
#checked for any missing data
any(is.na(data)) # the  
#check the structure of the data
str(data)
class(data)
class(data$Channel)
data$Channel#looks like channels looks like a categorical variable but with class interger. 1,2
data$Region#looks like Region looks like a categorical variable but with class interger. 1,2,3
#the rests are all integers as well

# Summary of the mean values of the variables
summarise(data,avg_channel=mean(Channel),avg_Region=mean(Region),
          avg_Fresh=mean(Fresh),avg_Milk=mean(Milk),
          avg_Grocery=mean(Grocery),avg_Frozen=mean(Frozen),
          avg_Detergents_Paper=mean(Detergents_Paper),
          avg_Delicassen=mean(Delicassen))

# Summary of the minimum values of the variables
summarise(data,min_channel=min(Channel),min_Region=min(Region),
          min_Fresh=min(Fresh),min_Milk=min(Milk),
          min_Grocery=min(Grocery),min_Frozen=min(Frozen),
          min_Detergents_Paper=min(Detergents_Paper),
          min_Delicassen=min(Delicassen))

# Summary of the maximum values of the variables
summarise(data,max_channel=max(Channel),max_Region=max(Region),
          max_Fresh=max(Fresh),max_Milk=max(Milk),
          max_Grocery=max(Grocery),max_Frozen=max(Frozen),
          max_Detergents_Paper=max(Detergents_Paper),
          max_Delicassen=max(Delicassen))

# From the summaries we can see the mean of the variables and min and max values
# to tell what range of values the variables take. We already know they can only be
# integers. Channel minimun is 1 and maximum is 2, so there are only two sales 
# channels (1 and 2). Region take values 1,2 and 3 so there are 3 different regions.
# Other variabels are annuals spendings on 5 different product categories, and
# their values vary from very small numbers to tens of thousands.

sum(data$Channel==1) #298 observations belong to channel 1
sum(data$Channel==2) # 142 observations belong to channel 2.

sum(data$Region==1) #77 observations from region 1.
sum(data$Region==2) #47 observations from region 2.
sum(data$Region==3) #316 observations from region 3.





#exploration on the data set to find out more about the relationshipsbetween the variable and the Regions
data%>%
  group_by(Region)%>%
  summarise_all(list(avg=mean))

#Exploration on the data set to find out more about the relationships between the  varaible and the sale channels.
data%>%
  group_by(Channel)%>%
  summarise_all(list(avg=mean))





#Distrubition of the data amongst the regions
ggplot(data, aes(x=Region))+geom_bar()+ggtitle('Region')

par(mfrow = c(2,3)) # Create a 2 x 3 plotting matrix for variable Grocery aganist Milk, Detergent_Paper, Frozen,Delicassen and Fresh
# The next 5 plots created will be plotted next to each other
plot(data$Fresh, 
     data$Grocery, ylab='Grocery', xlab = 'Fresh', main = 'Grocery againt Fresh',pch = 1, col = 'blue')
plot(data$Detergents_Paper, 
     data$Grocery,ylab='Grocery', xlab = 'Detergents_Paper', main = 'Grocery againt Detergent_Paper', pch = 1, col = 'blue')
plot(data$Frozen, 
     data$Detergents_Paper,ylab='Grocery', xlab = 'Frozen', main = 'Grocery againt Frozen', pch = 1, col = 'blue')
plot(data$Fresh,
     data$Milk, ylab='Fresh', xlab = 'Milk', main = 'Fresh againt Milk', pch = 1, col = 'blue')
plot(data$Detergents_Paper, 
     data$Frozen,ylab='Detergents_Paper', xlab = 'Frozen', main = 'Detergents_Paper againt Frozen', pch = 1, col = 'blue')

dev.off()






#performing a corelation analysis on the variable
cor(data)
corrplot(cor(data),'number', number.cex = 0.5)# visualize the correlation of the variable. number.cex is for moderating the font size

#scaling the data with the min max method
#i scaled the data but with the exception of Channel and Region as the two had catergorical or discret values and i thought it would be less meangiful to subject them to normalization
rscdata=apply(data[,c(3,4,5,6,7,8)],2,rescale, to=c(0,1))


#determining the optimal number of clusters with our nstart value at 25
tot_within_ss=map_dbl(1:10, function(k){
  model=kmeans(rscdata, center=k, nstart=25)
  model$tot.withinss
})
tot_within_ss
#number of clusters with the elbow method
plot(1:10, tot_within_ss, type='o', xlab='Number of clusters',
     ylab='Total wss', main='Elbow method on Wholesale dataset', panel.first = grid())

#lets use other methods for determining the number of clusters
silclust=NbClust(rscdata,distance='euclidean', min.nc =2, max.nc = 10,
                 method = 'kmeans', index='silhouette')

Gapclust=NbClust(rscdata,distance='euclidean', min.nc =2, max.nc = 10,
                 method = 'kmeans', index='gap')

CHClust=NbClust(rscdata,distance='euclidean', min.nc =2, max.nc = 10,
                method = 'kmeans', index='ch')

#plotting these methods
par(mfrow=c(1,3))
plot(2:10, silclust$All.index,type='o', xlab='Number of clusters', 
     ylab='silhouette score', panel.first = grid())
plot(2:10, Gapclust$All.index,type='o', xlab='Number of clusters', 
     ylab='Gap statistic', panel.first = grid())
plot(2:10, CHClust$All.index,type='o', xlab='Number of clusters', 
     ylab='Calinski Harabasz', panel.first = grid())


#Now lets lets cluster
kmeansmdl=kmeans(rscdata, center=2, nstart=25)
par(mfrow=c(1,1))


#lets add cluster membership to the data
data_cluster=data.frame(rscdata)%>%
  mutate(Cluster_membership=factor(kmeansmdl$cluster))
# here i count the number of observation oin each cluster
table(data_cluster$Cluster_membership)
data_cluster
# i try to find out the avg mean of every variable per cluster
data_cluster%>%
  group_by(Cluster_membership)%>%
  summarise_all(list(avg=mean, std=sd))

#Visualization of the different variable to see which dictates the cluster
ggplot(data_cluster, aes(x = Detergents_Paper, y=Frozen, col=Cluster_membership))+
  geom_point()+
  ggtitle("Clusters in the data set")

#Visualization of the different variable to see which dictates the cluster
ggplot(data_cluster, aes(x = Detergents_Paper, y=Milk, col=Cluster_membership))+
  geom_point()+
  ggtitle("Clusters in the data set")


#lets add the cluster membership also to the all unormalized data set
data_new=data%>%
  mutate(Cluster_membership=factor(kmeansmdl$cluster))
table(data_new$Cluster_membership)

data_new$Cluster_membership
# ggplot showing the count of obersvations in each respective cluster
ggplot(data_new, aes(x=Cluster_membership))+
  geom_bar(aes(fill=Cluster_membership))+
  ggtitle('Count of Observations in the clusters of the dataset') +
  theme(plot.title=element_text(hjust=0.5))
# box plot showing the distrubition of the clusters amongst the different variables
ggplot(data_new, aes(x = Cluster_membership, y=Fresh, fill=Cluster_membership))+
  geom_boxplot()+
  ggtitle("Distribution of Fresh by Cluster")+
  xlab("Cluster")+
  ylab("Fresh values")

ggplot(data_new, aes(x = Cluster_membership, y=Milk, fill=Cluster_membership))+
  geom_boxplot()+
  ggtitle("Distribution of Milk by Cluster")+
  xlab("Cluster")+
  ylab("Milk values")

ggplot(data_new, aes(x = Cluster_membership, y=Grocery, fill=Cluster_membership))+
  geom_boxplot()+
  ggtitle("Distribution of Grocery by Cluster")+
  xlab("Cluster")+
  ylab("Grocery values")


ggplot(data_new, aes(x = Cluster_membership, y=Detergents_Paper, fill=Cluster_membership))+
  geom_boxplot()+
  ggtitle("Distribution of Detergents_Paper by Cluster")+
  xlab("Cluster")+
  ylab("Detergents_Paper values")

ggplot(data_new, aes(x = Cluster_membership, y=Frozen, fill=Cluster_membership))+
  geom_boxplot()+
  ggtitle("Distribution of Frozen by Cluster")+
  xlab("Cluster")+
  ylab("Frozen values")

ggplot(data_new, aes(x = Cluster_membership, y=Delicassen, fill=Cluster_membership))+
  geom_boxplot()+
  ggtitle("Distribution of Delicassen by Cluster")+
  xlab("Cluster")+
  ylab("Delicassen values")


data_new
# explanatory analysis to also find out which variable drives the the cluster
data_new%>%
  group_by(Cluster_membership)%>%
  summarise_all(list(avg=mean, std=sd))

data_new%>%
  group_by(Cluster_membership)

ggplot(data_new,(aes(x=Detergents_Paper, y=Frozen, col=Channel)))+
  geom_point()+
  facet_wrap(~Cluster_membership)+
  ggtitle('Analysis of income and savings')+
  theme(plot.title=element_text(hjust=0.5))


ggplot(data_new,(aes(x=Detergents_Paper, y=Milk, col=Channel)))+
  geom_point()+
  facet_wrap(~Cluster_membership)+
  ggtitle('Analysis of income and savings')+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data_new,(aes(x=Frozen, y=Grocery, col=Channel)))+
  geom_point()+
  facet_wrap(~Cluster_membership)+
  ggtitle('Analysis of Grocery and Frozen in different Channels and clusters')+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data_new,(aes(x=Fresh, y=Milk, col=Channel)))+
  geom_point()+
  facet_wrap(~Cluster_membership)+
  ggtitle('Analysis of Fresh and Milk in different Channels and Clusters')+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data_new,(aes(x=Fresh, y=Milk, col=Channel)))+
  geom_point()+
  facet_wrap(~Region)+
  ggtitle('Analysis of Fresh and Milk in different Channels and Regions')
theme(plot.title=element_text(hjust=0.5))

ggplot(data_new,(aes(x=Detergents_Paper, y=Frozen, col=Channel)))+
  geom_point()+
  facet_wrap(~Region)+
  ggtitle('Analysis of Detergents_Paper and Frozen in different Channels and Regions')
theme(plot.title=element_text(hjust=0.5))
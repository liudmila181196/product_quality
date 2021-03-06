install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("factoextra")
install.packages("ggpubr")
install.packages("fossil")

library(readxl)
library(ggplot2)
library(dplyr)
library(factoextra)
library(ggpubr)
library(fossil)

#�������� ��������
colnames<-readLines("C:/Users/nazar/OneDrive/���������/������_���������/������/names.csv")

#��������� �������
obj<-read.csv("C:/Users/nazar/OneDrive/���������/������_���������/������/stat2.csv", sep = ";", dec=",", 
              col.names = colnames,na.strings = "NaN", header = F)
#tolower(obj)

#str(obj)

#���������� �����
#nrow(obj)

#������ 6 �����
#head(obj, 20)

#tail(obj, 6)
#����������� �����������
obj$defCoef<-obj$DefectQuantity/obj$ProductionQuantity


#������� ������ ����� ������������ ��������
nullProdQuan<-which(is.na(obj$ProductionQuantity))
nullTested<-which(is.na(obj$TestedProductionQuantity))
nullDefect<-which(is.na(obj$DefectQuantity))


nullProdQuan
nullTested
nullDefect


#������� ����������� �������� ��������� �� 0
obj_copy<-obj
obj_copy$ProductionQuantity[nullProdQuan]<-0
obj_copy$TestedProductionQuantity[nullTested]<-0
obj_copy$DefectQuantity[nullDefect]<-0


#������� ������ ����� ������������ ��������
nullProdQuan<-which(is.na(obj_copy$ProductionQuantity))
nullTested<-which(is.na(obj_copy$TestedProductionQuantity))
nullDefect<-which(is.na(obj_copy$DefectQuantity))


nullProdQuan
nullTested
nullDefect


nullDefCoef<-which(is.na(obj_copy$defCoef))
nullDefCoef
obj_copy$defCoef[nullDefCoef]<-0
nullDefCoef<-which(is.na(obj_copy$defCoef))
nullDefCoef

#������ ������ � 0 ��������
#zeroDef<-which(obj_copy$DefectQuantity==0)
#obj_copy<-obj_copy[-zeroDef,]

str(obj_copy)

head(obj_copy, 20)

qualFactors<-read.csv2("C:/Users/nazar/OneDrive/���������/������_���������/������/qualityfactors3.csv")
str(qualFactors)

obj_copy<-inner_join(obj_copy, qualFactors,by=c("ItemGroup"="���.������"))
obj_qual<-obj_copy
obj_qual<-obj_qual[,c(3,6,8:17)]
obj_copy<-obj_copy[,c(3,6,8:17)]
head(obj_qual,10)
str(obj_qual)

#����� ��� �.2
obj_2<-obj_copy

#������� ������ ��� ���������� �������� 0
obj_nozero<-obj_qual[-(which(obj_qual$DefectQuantity==0)),]
str(obj_nozero)
which(obj_nozero$DefectQuantity==0)

#which(levels(obj_copy$ItemGroup)==levels(qualfactors$���.������))
#which(obj_qual$TestedProductionQuantity<obj_qual$DefectQuantity)

#���� ����������� ��� ����� ���������
defGroupCount<-by(obj_copy$DefectQuantity, obj_copy$ItemGroup, sum)
testGroupCount<-by(obj_copy$ProductionQuantity, obj_copy$ItemGroup, sum)
defCoefGroupSum<-defGroupCount/testGroupCount

#���� ����������� ��� ��������
defPhaseCount<-by(obj_copy$DefectQuantity, obj_copy$ProductionPhase, sum)
testPhaseCount<-by(obj_copy$ProductionQuantity, obj_copy$ProductionPhase, sum)
defCoefPhaseSum<-defPhaseCount/testPhaseCount

#���� ����������� ��� �����������
obj_qual$��������������<-obj_qual$defCoef*obj_qual$��������������
obj_qual$��������������<-obj_qual$defCoef*obj_qual$��������������
obj_qual$������������<-obj_qual$defCoef*obj_qual$������������
obj_qual$���������������<-obj_qual$defCoef*obj_qual$���������������
obj_qual$��������������<-obj_qual$defCoef*obj_qual$��������������
obj_qual$����������<-obj_qual$defCoef*obj_qual$����������



#������� ��������
qualMean<-colMeans(obj_qual[,7:12])
qualMean<-data.frame(row.names =levels(obj_qual$ItemGroup))
qualMean$��������������<-by(obj_qual$��������������, obj_qual$ItemGroup, mean)
qualMean$��������������<-by(obj_qual$��������������, obj_qual$ItemGroup,mean)
qualMean$������������<-by(obj_qual$������������, obj_qual$ItemGroup,mean)
qualMean$���������������<-by(obj_qual$���������������, obj_qual$ItemGroup,mean)
qualMean$��������������<-by(obj_qual$��������������, obj_qual$ItemGroup,mean)
qualMean$����������<-by(obj_qual$����������, obj_qual$ItemGroup,mean)

qualMean<-as.matrix(qualMean)
str(qualMean)

#���� ����������� �� �������
#obj_2017<-subset(obj_copy, obj_copy$Year==2017)
#defMonthCount<-by(obj_2017$DefectQuantity, obj_2017$Month, sum)
#testMonthCount<-by(obj_2017$ProductionQuantity, obj_2017$Month, sum)
#defMonthCoef<-defMonthCount/testMonthCount

#str(defCoefPhaseMean)
#str(defFactor)

#�������

barplot(qualMean, xlab = "����������", ylab = "����������� �����������", ylim=c(0,0.04),
        col = topo.colors(6), 
        main = "������� ����������� ����������� ��� ������ ����� ��������� � �����������")
legend(5, 0.04, rownames(qualMean), fill=topo.colors(6))



ggplot(data = obj_nozero, aes(x = �������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_nozero, aes(x = �������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_nozero, aes(x = ������������ , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_nozero, aes(x = ��������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_nozero, aes(x = �������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_nozero, aes(x = ���������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)

ggplot(data = obj_qual, aes(x = �������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_qual, aes(x = �������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_qual, aes(x = ������������ , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_qual, aes(x = ��������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_qual, aes(x = �������������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)
ggplot(data = obj_qual, aes(x = ���������� , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)

barplot(defCoefGroupSum, xlab = "���� ���������", ylab = "����������� �����������", 
        #col = topo.colors(6), 
        main = "����������� ����������� ��� ������ ����� ���������")

barplot(defCoefPhaseSum, xlab = "����� ������������", ylab = "����������� �����������", 
        main = "����������� ����������� ��� ������ ������������")



colors <- c('#FF8000', '#6600CC','#8B0000','#006400','#0000FF','#FF1493')
pairs(obj_qual[,c(7:12)], pch=20,col=colors[obj_qual$ItemGroup])

#barplot(defMonthCoef, xlab = "�����", ylab = "����������� �����������", main = "����������� ����������� �� ������� �� 2017 ���")

tab<-data.frame(obj_copy$ItemGroup, obj_copy$ProductionPhase)
barplot(tab, xlab = "���� ���������", ylab = "���-�� ��������",
        col = ItemGroup, 
        main = "���������� �������� �� ����� ��������� � ������ ������������")
legend("topright",c(levels(obj_copy$ItemGroup)))

ggplot(data = obj_copy, aes(x = defCoef , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~ProductionPhase)+
  labs(x="����������� �����������", y="���������� ���������",fill="��� ���������")

corCoef<-qualFactors[1,(2:7)]
corCoef$ProductionQuantity<-(cor.test(obj_copy$ProductionQuantity,obj_qual$defCoef))$estimate
corCoef$��������������<-(cor.test(obj_copy$��������������,obj_qual$defCoef))$estimate
corCoef$��������������<-(cor.test(obj_copy$��������������,obj_qual$defCoef))$estimate
corCoef$������������<-(cor.test(obj_copy$������������,obj_qual$defCoef))$estimate
corCoef$���������������<-(cor.test(obj_copy$���������������,obj_qual$defCoef))$estimate
corCoef$��������������<-(cor.test(obj_copy$��������������,obj_qual$defCoef))$estimate
corCoef$����������<-(cor.test(obj_copy$����������,obj_qual$defCoef))$estimate
corCoef<-data.matrix(corCoef)
corCoef<-t(corCoef)
colnames(corCoef)<-c("defCoef")
corCoef<-as.data.frame(corCoef)
corCoef$Factors<-c(rownames(corCoef))

barplot(corCoef[,1:6], xlab = "���������� ��������", ylab = "����������� ����������",  ylim=c(-0.1,0.15),
        main = "����������� ���������� ����� ������ ����������� �������� � ������������ �����������")

ggplot(data=corCoef[1:6,], aes(x=Factors, y=defCoef)) +
  geom_bar(stat="identity")+labs(x="����������", y="����������� ����������", title="����������� ���������� ����� ������ ����������� �������� � ������������ �����������")

#ggscatter(obj_qual, x = "ProductionQuantity", y = "defCoef", 
          #add = "reg.line", conf.int = TRUE, 
          #cor.coef = TRUE, cor.method = "pearson",
          #xlab = "ProductionQuantity", ylab = "defCoef")

print(cor(obj_copy[3:12]))
str(obj_qual)
str(obj_copy)
#������������� ���������� ������
to_clust<-obj_copy[,6:12]
str(to_clust)
head(to_clust,10)
fviz_nbclust(na.omit(to_clust), kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
m <- dist(scale(to_clust))
hc <- hclust(m, method = "ward.D")
plot(hc, cex = 0.9)
rect.hclust(hc, k = 3)
groups <- cutree(hc, k = 3)
obj_qual$groups <- factor(groups)

obj_copy$groups<-factor(groups)
obj_copy%>% filter(groups == 1) %>% View
obj_copy%>% filter(groups == 2) %>% View
obj_copy%>% filter(groups == 3) %>% View

obj_qual%>% filter(groups == 1) %>% View
obj_qual%>% filter(groups == 2) %>% View
obj_qual%>% filter(groups == 3) %>% View
obj_qual%>% filter(groups == 4) %>% View

ggplot(data = obj_qual, aes(x = "", y = ItemGroup)) + geom_boxplot() + facet_grid(~groups)
ggplot(data = obj_qual, aes(x = "", y = defCoef)) + geom_boxplot() + facet_grid(~groups)
ggplot(data = obj_qual, aes(x = "", y = defCoef, fill = groups)) + geom_violin() + facet_grid(~groups)
ggplot(data = obj_qual, aes(x = defCoef , fill = groups)) + geom_histogram(bins = 6, col = "black") + facet_grid(~groups)
ggplot(data = obj_qual, aes(x = defCoef, y = ��������������)) + geom_point(aes(color = groups))
ggplot(data = obj_qual, aes(x = defCoef, y = DefectQuantity)) + geom_point(aes(color = groups))

ggplot(data = obj_copy, aes(x = "", y = defCoef)) + geom_boxplot() + facet_grid(~groups)
ggplot(data = obj_copy, aes(x = "", y = defCoef, fill = groups)) + geom_violin() + facet_grid(~groups)
ggplot(data = obj_copy, aes(x = defCoef , fill = ItemGroup)) + geom_histogram(bins = 6, col = "black") + facet_grid(~groups)+
  labs(x="����������� �����������", y="���������� ���������",fill="��� ���������")
ggplot(data = obj_copy, aes(x = defCoef, y = ��������������)) + geom_point(aes(color = groups))
ggplot(data = obj_copy, aes(x = defCoef, y = DefectQuantity)) + geom_point(aes(color = groups))

kc <- kmeans(to_clust, 3)

obj_copy$kmeans <- kc$cluster
View(obj_copy)

groups <- cutree(hc, k = 3)
rand.index(groups, kc$cluster)

barplot(obj_copy[,6], xlab = "���������� ��������", ylab = "����������� �����������", 
        main = "����������� ����������� �� ����������� ��������",
        col=colors[obj_copy$groups])

tab<-table(obj_qual$group, obj_qual$��������������)
barplot(tab, xlab = "0 - �� ������, 1 - ������", ylab = "���-�� ��������",ylim=c(0,1200),
       col = topo.colors(2), 
      main = "���������� ��������� �� ������� � ������� �� ������., ���., ���. � ���. ����������")
legend("topleft",c("1","2","3","4"), fill=topo.colors(4))


tab<-table(obj_qual$group, obj_qual$��������������)
barplot(tab, xlab = "0 - �� ������, 1 - ������", ylab = "���-�� ���������",ylim=c(0,1000),
        col = topo.colors(4), 
        main = "���������� ��������� �� ������� � ������� �� ���������� ���������� � �������������� ����������")
legend("topright",c("1","2","3","4"), fill=topo.colors(4))

###########################################################
t<-obj_nozero[,6:12]
str(t)
t$defCoef

fviz_nbclust(na.omit(t), kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
m <- dist(scale(t))
hc <- hclust(m, method = "ward.D")
plot(hc, cex = 0.9)
rect.hclust(hc, k = 2)
groups <- cutree(hc, k = 2)
obj_nozero$groups <- factor(groups)

obj_nozero%>% filter(groups == 1) %>% View
obj_nozero%>% filter(groups == 2) %>% View

#ggplot(data = obj_qual, aes(x = "", y = ItemGroup)) + geom_boxplot() + facet_grid(~groups)
ggplot(data = obj_nozero, aes(x = "", y = defCoef)) + geom_boxplot() + facet_grid(~groups)
ggplot(data = obj_nozero, aes(x = "", y = defCoef, fill = groups)) + geom_violin() + facet_grid(~groups)
ggplot(data = obj_nozero, aes(x = defCoef , fill = groups)) + geom_histogram(bins = 10, col = "black") + facet_grid(~groups)
ggplot(data = obj_nozero, aes(x = defCoef, y = ��������������)) + geom_point(aes(color = groups))
ggplot(data = obj_nozero, aes(x = defCoef, y = ��������������)) + geom_point(aes(color = groups))
ggplot(data = obj_nozero, aes(x = defCoef, y = ���������������)) + geom_point(aes(color = groups))
ggplot(data = obj_nozero, aes(x = defCoef, y = DefectQuantity)) + geom_point(aes(color = groups))

kc <- kmeans(t, 2)

obj_nozero$kmeans <- kc$cluster
View(obj_nozero)

groups <- cutree(hc, k = 2)
rand.index(groups, kc$cluster)


qualMeanGr<-colMeans(obj_qual[,7:12])
qualMeanGr<-data.frame(row.names =levels(obj_qual$groups))
qualMeanGr$��������������<-by(obj_qual$��������������, obj_qual$groups, mean)
qualMeanGr$��������������<-by(obj_qual$��������������, obj_qual$groups,mean)
qualMeanGr$������������<-by(obj_qual$������������, obj_qual$groups,mean)
qualMeanGr$���������������<-by(obj_qual$���������������, obj_qual$groups,mean)
qualMeanGr$��������������<-by(obj_qual$��������������, obj_qual$groups,mean)
qualMeanGr$����������<-by(obj_qual$����������, obj_qual$groups,mean)
qualMeanGr<-as.matrix(qualMeanGr)

barplot(qualMeanGr, xlab = "����������", ylab = "����������� �����������", ylim=(c(0,0.1)),
        col = topo.colors(3), 
        main = "������� ����������� ����������� ��� ��������� � �����������")
legend("topright",rownames(qualMeanGr), fill=topo.colors(3))

############################################################################################################

##########################################################################################################

str(obj_2)

#����� ���������� ��������
sumdefq<-sum(obj_2$DefectQuantity)

#����� ������� ���������
prodq<-obj_2[(which(obj_2$ItemGroup=="������� ���������")),]
sumprodq<-sum(prodq$ProductionQuantity)

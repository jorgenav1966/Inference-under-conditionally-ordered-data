#NavarroCramerBala

#Example5.1

set.seed(333)
m<-100
X1<-rexp(m,1)
X2<-rexp(m,2)
L<-pmin(X1,X2) #series
U<-pmax(X1,X2) #parallel
I1<-X1<X2
I2<-X2<X1
m1<-sum(I1) 
m1 #31 data
m2<-m-m1
m2 #69 data
sum(I2)

mean(L) #0.3402184
1/3
a1<-(m1/m)/mean(L) #alpha1=1
a1 #estimated value 0.9111795

a2<-(m2/m)/mean(L) #alpha2=2
a2 #estimated value 2.028109

#Conf interval (not used)
z<-qnorm(0.95)
2*0.95-1
a<-a1-z*a1/sqrt(m)
b<-a1+z*a1/sqrt(m)
a
b
z

#simulation 

z<-qnorm(0.95)
set.seed(333)
m<-100
N<-100
i<-1:N
S<-0
for (i in 1:N) {
X1<-rexp(m,1)
X2<-rexp(m,2)
L<-pmin(X1,X2)
I1<-(X1<X2)*1
m1<-sum(I1) 
a1<-(m1/m)/mean(L) #alpha1=1
a<-a1-z*a1/sqrt(m)
b<-a1+z*a1/sqrt(m)
S<-S+(a<1)*(1<b)
}
S
S/N
pnorm(1)-pnorm(-1)

###end simulation (not used) 


mean(L)/(m1/m) #mu1=1 estimated value 1.097479
mean(L)/(m2/m) #mu2=0.5 estimated value 0.4930701

#Censored data <X2
X12<-L[I1]
mean(X12) #wrong estimation 0.4129864
mean(X12)/(m1/m) #mu1=1 estimated value 1.332214

#Censored data <X1
X21<-L[I2]
mean(X21)
mean(X21)/(m2/m) #mu2=0.5 estimated value 0.4456891

#Estimations for the other means
mean(X21)/(m1/m) #mu1=1 estimated value 0.9920176
mean(X12)/(m2/m)  #mu2=0.5 estimated value 0.598531

#Fig 5.1, left
plot(X1,X2,pch=20, col = factor(I1))
abline(0,1,col='blue')

pdf('Fig1.pdf',width=5,height=5)
plot(X1,X2,pch=20, col = factor(I1))
abline(0,1,col='blue')
dev.off()

#Fig5.1, right
-log(0.1)*mean(L)/(m1/m) #confidence band 2.527038
-log(0.1)*mean(L)/(m2/m) #confidence band X2 1.135336

pdf('Fig1rNew.pdf',width=5,height=5)
plot(L,U,pch=20, col = factor(I1))
abline(0,1,col='blue')
abline(1.097479,1) #regres. curve X1
abline(2.527039,1,lty=2) #conf. band
abline(0.4930701,1,col='red') #regres. curve X2 
abline(1.135336,1,col='red',lty=2) #conf band
dev.off()


#Real coberage
100-sum(I1)
66/69
sum((U<L+2.527039)*(1-I1))/69
sum((U<L+1.135336)*(I1))/sum(I1)
27/31

#common censure time 
T<-X2[1]
T #0.5928832
J1<-X1<T
k1<-sum(J1)
k1
Y12<-X1[J1]
mean(Y12)
mean(Y12)/(k1/m) #Estimated value 0.6834739

(0.38*0.2597201+0.62*0.5928832)/0.38 #correct estimation
0.38*0.2597201+0.62*0.5928832 #wrong estimation

#non-included plot
X2bis<-1:m
X2bis[1:m]<-T
#pdf('Fig1r.pdf',width=5,height=5)
plot(X1,X2bis,pch=20, col = factor(J1),ylab='T')
abline(0,1,col='blue')
#dev.off()

#Case 2
mean(U) #Estimation 1.172625
7/6 #real value
X12<-U[I1]
X21<-U[I2]
m2<-sum(I2)
m2
m1

mean(X21)
4/3
mean(X21)/(1+m2/m) #wrong
mean(X21)/(1+m1/m) #mu1=1 estimation 0.9968541
1.305879/(1+31/100)

mean(X12)
5/6
mean(X12)/(1+m1/m) #wrong!
mean(X12)/(1+m2/m) #mu2=0.5 estimation 0.5183591


mean(X21)/(m2/m1+m2/m) #mu2=0.5
1.305879/(69/31+69/100)

mean(X12)/(m1/m2+m1/m) #mu1=1
0.8760269/(31/69+31/100)

#With all the data in U
mean(U)/(m/m2-m1/m)
mean(U)/(m/m1-m2/m)




#Example 5.2 n=3 Case 3 

set.seed(444)
m<-200
X1<-rexp(m,1)
X2<-rexp(m,2)
X3<-rexp(m,3)
L<-pmin(X1,X2,X3)
U<-pmax(X1,X2,X3)
median(c(1,2,3))
T<-1:m
for (i in 1:m) {
T[i]<-median(c(X1[i],X2[i],X3[i]))
}

Data<-data.frame(X1,X2,X3,L,T,U)
View(Data)
I1<-(L==X1)
I2<-(L==X2)
I3<-(L==X3)
sum(I1)->m1
sum(I2)->m2
sum(I3)->m3
m1
m2
m3

#Estimations
mu1<-sum(T*I1)/m1
mu1
1/6+1/5

mu2<-sum(T*I2)/m2
mu2
1/6+1/4

mu3<-sum(T*I3)/m3
mu3
1/6+1/3

m1+m2+m3
ga<-min(mu1,mu2,mu3)
ga
g<-function(x) x/(mu1-x)+x/(mu2-x)+x/(mu3-x)

#Fig5.2, left
curve(g,0,ga-0.1,ylab='g')
abline(2,0,col='red')
##

curve(g,0.16,0.17,ylab='g')
abline(2,0,col='red')

curve(g,0.164869,0.164870,ylab='g')
abline(2,0,col='red')

1/6
b4<-0.164869 #estimation beta 4

b1<-mu1-b4 #estimation beta 1
b1
1/5

b2<-mu2-b4 #estimation beta 2
b2
1/4

b3<-mu3-b4 #estimation beta 3
b3
1/3

1/b4
1/b1
a1<-1/b4-1/b1
a1 #estimation alpha 1

a2<-1/b4-1/b2
a2 #estimation alpha 2

a3<-1/b4-1/b3 
a3 #estimation alpha 3


mL=max(L)
mU=max(U)
mT=max(T)
plot(L,T,pch=20)
L1<-L[I1]
T1<-T[I1]
U1<-U[I1]

#Fig 5.2, right
#plot(L1,T1,pch=20,xlab='L',ylab='T',xlim=c(0,mL),ylim=c(0,mT))
plot(L1,T1,pch=20,xlab='L',ylab='T')
curve(x+1-1, add=TRUE,col='blue')
curve(x+b1, add=TRUE)
curve(x-b1*log(0.1), add=TRUE,lty=2)

sum(T1<L1-b1*log(0.1))/sum(I1)
sum(T1>L1-b1*log(0.1))
sum(I1)
23/25

#Fig 5.2, right
pdf('Fig2rNew.pdf',width=5,height=5)
plot(L1,T1,pch=20,xlab='L',ylab='T')
curve(x+1-1, add=TRUE,col='blue')
curve(x+b1, add=TRUE)
curve(x-b1*log(0.1), add=TRUE,lty=2)
sum(T1<L1-b1*log(0.1))/sum(I1)
sum(T1>L1-b1*log(0.1))
dev.off()




L2<-L[I2]
T2<-T[I2]
U2<-U[I2]

#Fig5.3, left
pdf('Fig3lNew.pdf',width=5,height=5)
plot(L2,T2,pch=20,xlab='L',ylab='T')
curve(x+1-1, add=TRUE,col='blue')
curve(x+b2, add=TRUE)
curve(x-b2*log(0.1), add=TRUE,lty=2)
sum(T2<L2-b2*log(0.1))/sum(I2)
dev.off()

sum(T2>L2-b2*log(0.1))
sum(I2)
sum(T2<L2-b2*log(0.1))
sum(T2<L2-b2*log(0.1))/sum(I2)
68/74


L3<-L[I3]
T3<-T[I3]
U3<-U[I3]

#Fig5.3, right
pdf('Fig3rNew.pdf',width=5,height=5)
plot(L3,T3,pch=20,xlab='L',ylab='T')
curve(x+1-1, add=TRUE,col='blue')
curve(x+b3, add=TRUE)
curve(x-b3*log(0.1), add=TRUE,lty=2)
dev.off()

sum(T3>L3-b3*log(0.1))
sum(I3)
sum(T3<L3-b3*log(0.1))
sum(T3<L3-b3*log(0.1))/sum(I3)
92/101



######################################################
# Parsimony demonstration, adapted from Bolker 2007
######################################################

#Functions and model fits 
tmpf = function(x,a=0.4,b=0.1,c=2,d=1) {
  (a+b*x+c*x^2)*exp(-d*x)
}
dpars = formals(tmpf)[-1]
set.seed(1005)
npts = 10
x = runif(npts,min=1,max=7)
y_det = tmpf(x)
y = y_det+rnorm(npts,sd=0.35)
y2 = y_det+rnorm(npts,sd=0.35)
ymax=2
n1 = nls(y~tmpf(x,a,b,c,d),start=list(a=0.4,b=0.1,c=2,d=1))
n2 = nls(y~a*x*exp(-b*x),start=list(a=1,b=0.5))
#n3 = nls(y~a, start=list(a=0))
p0 = rep(mean(y),length(y))
p1 = predict(n1)
xvec = seq(0,7,length=150)
p1vec = predict(n1,newdata=list(x=xvec))
p2vec = predict(n2,newdata=list(x=xvec))
p2 = predict(n2)
calc_r2 = function(y) {
  s0 = sum((y-p0)^2)
  s1 = sum((y-p1)^2)
  s2 = sum((y-p2)^2)
  c(1-s1/s0,1-s2/s0,s0,s1,s2,which.min(c(s0,s1,s2)))
}
r2.0 = calc_r2(y)
r2.1 = calc_r2(y2)
r2vec = t(replicate(500,calc_r2(y_det+rnorm(npts,sd=0.35))))
r2vec_mean = colMeans(r2vec)[1:5]
tv = table(r2vec[,6])

#Rsquared 

#Plots
op = par(mfrow=c(1,2))
par(lwd=2,bty="l",las=1,cex=1.5, mgp=c(2.5,1,0))
par(mar=c(5,4,2,0.5)+0.1)
plot(x,y,ylim=c(0,max(c(y,ymax))),xlim=c(0,7),axes=FALSE,
     xlab="",ylab="")
#points(x[7],y[7],pch=16)
axis(side=1,at=c(0,3,6))
axis(side=2)
abline(h=p0)
box()
tcol = "gray"
curve(tmpf,add=TRUE,from=0,col=tcol)
lines(xvec,p1vec,lty=2)
lines(xvec,p2vec,lty=3)
par(xpd=NA)
legend(c(3.1,8.6),c(1.2,2),
       c("constant",
         "Ricker",
         "gen Ricker",
         "true"),
       col=rep(c("black",tcol),c(3,1)),
       lty=c(1,3,2,1),
       bty="n",
       cex=0.75)     
par(xpd=FALSE)
par(mar=c(5,1,2,3.5)+0.1)
plot(x,y2,ylim=c(0,max(c(y,ymax))),xlim=c(0,7),axes=FALSE,
     xlab="",ylab="")
#points(x[7],y2[7],pch=16)
axis(side=1,at=c(0,3,6))
axis(side=2,labels=FALSE)
curve(tmpf,add=TRUE,from=0,col=tcol)
abline(h=p0)
xvec = seq(0,7,length=150)
lines(xvec,predict(n1,newdata=list(x=xvec)),lty=2)
lines(xvec,predict(n2,newdata=list(x=xvec)),lty=3)
box()

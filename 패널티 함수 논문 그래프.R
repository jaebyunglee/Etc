rm(list=ls())

###fun
lasso.pen.fun = function(b.vec,lam,gam,tau){ return(lam*abs(b.vec)) }
scad.pen.fun = function(b.vec,lam,gam,tau){
  ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec)*(ab.vec<lam)
  tem1.vec = ((tau*lam*(ab.vec-lam)-(ab.vec^2-lam^2)/2)/(tau-1)+lam^2)*(ab.vec>=lam)*(ab.vec<tau*lam)
  tem2.vec = ((tau+1)*lam^2/2)*(ab.vec>=tau*lam)
  return(tem0.vec+tem1.vec+tem2.vec)
}
mcp.pen.fun = function(b.vec,lam,gam,tau){
  ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec-ab.vec^2/2/tau)*(ab.vec<tau*lam); 
  tem1.vec = (tau*lam^2/2)*(ab.vec>=tau*lam); return(tem0.vec+tem1.vec)
}
tlp.pen.fun = function(b.vec,lam,gam,tau){
  ab.vec = abs(b.vec); tem0.vec = (lam*ab.vec)*(ab.vec<tau); tem1.vec = (lam*tau)*(ab.vec>=tau)
  return(tem0.vec+tem1.vec)
}
classo.pen.fun = function(b.vec,lam,gam,tau){
  ab.vec = abs(b.vec); tem0.vec = (-ab.vec^2/tau/2+lam*ab.vec)*(ab.vec<tau*(lam-gam))
  tem1.vec = (gam*ab.vec-tau^2*(lam-gam)^2/tau/2+lam*tau*(lam-gam)-tau*gam*(lam-gam))*(ab.vec>=tau*(lam-gam))
  return(tem0.vec+tem1.vec)
}
sridge.pen.fun = function(b.vec,lam,gam,tau){
  ab.vec = abs(b.vec)
  tem0.vec = (-ab.vec^2/2/tau+lam*ab.vec)*(ab.vec<tau*lam/(1+tau*gam))
  tem1.vec = (gam*ab.vec^2/2+tau*lam^2/(1+tau*gam)/2)*(ab.vec>=tau*lam/(1+tau*gam))
  return(tem0.vec+tem1.vec)
}
mbridge.pen.fun = function(b.vec,lam,gam,tau){
  ab.vec = abs(b.vec); pen.vec = rep(0,length(b.vec))
  pen.vec[ab.vec< tau] = lam*ab.vec[ab.vec<tau]
  pen.vec[ab.vec>=tau] = lam*(2*sqrt(tau*ab.vec[ab.vec>=tau])-tau)
  return(pen.vec)
}
mlog.pen.fun = function(b.vec,lam,gam,tau){
  ab.vec = abs(b.vec); pen.vec = rep(0,length(b.vec))
  pen.vec[ab.vec< tau] = lam*ab.vec[ab.vec<tau]
  pen.vec[ab.vec>=tau] = lam*tau*(1+log(ab.vec[ab.vec>=tau]/tau))
  return(pen.vec)
}



####arg
b.vec = seq(-10,10,length.out = 1001);lam = 1;gam = 0.5; tau = 2.1

####graph
par(mfrow=c(1,3))
plot(b.vec,scad.pen.fun(b.vec,lam,gam,tau),xlim = c(-3,3),ylim = c(0,4),lwd=2,pch=4,type="l",col="#8B0000",xlab = "Beta",ylab = "Value")
par(new=T)
plot(b.vec,mcp.pen.fun(b.vec,lam,gam,tau),xlim = c(-3,3),ylim = c(0,4),lwd=2,pch=4,type="l",col="#27408B",xlab = "",ylab = "")
par(new=T)
plot(b.vec,tlp.pen.fun(b.vec,lam,gam,tau),xlim = c(-3,3),ylim = c(0,4),lwd=2,pch=4,type="l",col="#2E8B57",xlab = "",ylab = "")
legend(x=-1,y=4, c("TLP","SCAD","MCP"), cex=1,text.width=0.8,fill=c("#2E8B57","#8B0000","#27408B"))

plot(b.vec,sridge.pen.fun(b.vec,lam,gam,tau),xlim = c(-3,3),ylim = c(0,4),lwd=2,pch=4,type="l",col="#8B0000",xlab = "Beta",ylab = "Value")
par(new=T)
plot(b.vec,classo.pen.fun(b.vec,lam,gam,tau),xlim = c(-3,3),ylim = c(0,4),lwd=2,pch=4,type="l",col="#27408B",xlab = "",ylab = "")
par(new=T)
legend(x=-1,y=4, c("Sridge","Classo"),cex=1,text.width=0.8,fill=c("#8B0000","#27408B"))


plot(b.vec,mbridge.pen.fun(b.vec,lam,gam,tau),xlim = c(-6,6),ylim = c(0,7),lwd=2,pch=4,type="l",col="#8B0000",xlab = "Beta",ylab = "Value")
par(new=T)
plot(b.vec,mlog.pen.fun(b.vec,lam,gam,tau),xlim = c(-6,6),ylim = c(0,7),lwd=2,pch=4,type="l",col="#27408B",xlab = "",ylab = "")
par(new=T)
legend(x=-2,y=7, c("mbridge","Mlog"), cex=1,text.width=1,fill=c("#8B0000","#27408B"),bty="c")
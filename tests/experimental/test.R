

n = 10
R = c(3, 5)
tmp = duct(n = n, R = R, center = c(1,3))
plot.base(xlim = c (-10, 10), ylim = c(-10, 10))
lines(tmp)
#tmp = duct(n = n, R = R)
#lines(tmp)

tmp = ngon.circle(4, N = n, R = R[2], center = c(1,3))
lines(tmp, col = "blue")


n = c(20, 15)
plot.base(xlim=c(-20,20), ylim=c(-20,20))
lines(duct(n[1], c(15,18), phi=pi/2/n[1], fill = "#0048C0"))
lines(duct(n[1], c(9,13), phi=c(0, pi/n[2]), nc.r = NULL,
           fill = "#8080F0"), lwd = 5)
lines(duct(n[1], c(5,8)))
abline(h=0, col="green")

warnings()

dev.off()


plot.base()
star = star(5, R = c(3, 1), center = c(5, 5), fill = 2)
lines(star)

# Virus
N = 10
R = 3

virus = virus(N, R = R, center = c(5, 5))
plot.base()
lines(virus)
tmp = ngon.circle(4, N = N, R = R+1+1/2, center = c(5,5))
lines(tmp, fill = "blue")

# Braces

plot.base(ylim = c(-10,10))
lines(braces.curly(c(0,0)), lwd=3)
lines(braces.curly(c(4,0), left=FALSE), lwd=3)

################

par(mfrow = c(2,2))

### Circular Helix
R = 5; n = 20;
lim = c(-R, R) + c(-1,1);
plot.base(xlim = lim, ylim = lim)
xy = helix.rad(R=R, n=n, N=512)
lines(xy)
xy = helix.rad(R=R, n=n, phi=pi/2, N=512)
lines(xy, col="red")


### Flower 1:
R = 5; n = 11;
lim = c(-R, R) + c(-2,2);
plot.base(xlim = lim, ylim = lim)
xy = helix.rad(R=R, n=n, phi=pi)
lines(xy)
xy = helix.rad(R = R - 1, n=n, phi=pi)
lines(xy, col="red")


### Flower 2:
R = 5; n = 11;
lim = c(-R, R)*2 + c(-1,1);
plot.base(xlim = lim, ylim = lim)
xy = helix.rad(R=R, r=R, n=n, N=256)
lines(xy, col="red")


### Other
R = 5; n = 10;
lim = c(-R, R) + c(-2,2);
plot.base(xlim = lim, ylim = lim)
abline(h=0, col="green", lty=2)
for(r in c(3,4,6,9)) {
  xy = helix.rad(R = R - 2/r, n=n, phi=pi, r = 2/r);
  lines(xy, col="red");
}




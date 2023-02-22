

n = 10
R = c(3, 5)
tmp = duct(n = n, center = c(1,3))
plot.base(xlim = c (-10, 10), ylim = c(-10, 10))
lines(tmp)
tmp = duct(n = n, R = R)
lines(tmp)


n = c(20, 15)
plot.base(xlim=c(-20,20), ylim=c(-20,20))
lines(duct(n[1], c(15,18), phi=pi/2/n[1], fill = "#0048C0"))
lines(duct(n[1], c(9,13), phi=c(0, pi/n[2]), nc.r = NULL,
           fill = "#8080F0"), lwd = 5)
lines(duct(n[1], c(5,8)))
abline(h=0, col="green")

warnings()

dev.off()



star = star(5, R = c(3, 1), center = c(5, 5), fill = 2)
plot.base()
lines(star)



virus = virus(5, R = 3, center = c(5, 5))
plot.base()
lines(virus)
virus

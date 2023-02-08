

n = 10
R = c(3, 5)
tmp = duct(n = n, center = c(1,3))
plot.base(xlim = c (-10, 10), ylim = c(-10, 10))
lines(tmp)
tmp = duct(n = n, R = R)
lines(tmp)


dev.off()

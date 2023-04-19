

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
star = star(3, R = c(3, 1), center = c(5, 5), fill = 2)
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

### Lens

R = 5;
lens = lens(R = R, x = c(1, 2), y = c(0, 4))
plot.base()
lines(lens)

### Example 1:
R = 5;
plot.base()
lens = lens(R = R, x = c(1, 2), y = c(0, 4))
lines(lens)
#
lens = lens(R = R, x = c(5, 3), y = c(0, 5))
lines(lens, col="Red")
# negative R: semi-concave Lens
lens = lens(R = c(4,-7), x = c(5, 0), y = c(1, 0) + 6)
lines(lens, col="#329624")

### Example 2:
scale.x = 1.5
scale.R = c(1.5, 2)
x = c(0, 1); y = c(0, 4);
# does NOT work with concave lenses;
fill = "#6480D0";
plot.base()
lens = lens(x = x, y = y + 2, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x*scale.x + 2, y = y*scale.x + 0.5, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x*scale.x^2 + 4, y = y*scale.x^2 - 1.75, scale=scale.R)
lines(lens, fill=fill)

### Arcs
plot.base()
plot.circle.arc(3, c(3,3), c(pi - pi/3, pi + pi/3), lty=2)

plot.base()
plot.circle.arc(3, c(3,3), c(pi/3, 2*pi/3), lty=2)

plot.base()
plot.circle.arc(3, c(3,3), c(2*pi- pi/3, pi/3), lwd=1, col="#6432B0")


### Neuron
draw_neuron(radius = 1)


n = 5; phi = 2*pi/n;
testFilledCircle(circlesOnCircle(n,2, phi=pi/n))
tmp = sapply(seq(n), function(k) points(
  2 * cos(pi/2 + k * phi) + 3.4 * cos(phi * (k-1) + phi/2),
  2 * sin(pi/2 + k * phi) + 3.4 * sin(phi * (k-1) + phi/2), col = "red"))

phi = 0;
n = 5;
center = c(2, 3)
plot.base()
tmp = neuron_body(n = n, center = center, phi = phi)
lines(tmp)

# For the new function
###
phi = 0;
n = 5;
center = c(2, 3)
plot.base()
tmp = neuron(n = n, center = center, phi = phi)
lines(tmp)

###
phi = -pi/2;
n = 5;
center = c(2, 3)
plot.base()
tmp = neuron(n = n, center = center, phi = phi)
lines(tmp)

###
phi = 2*pi - pi/2;
n = 5;
center = c(2, 3)
plot.base()
tmp = neuron(n = n, center = center, phi = phi)
lines(tmp)

###
phi = pi/2;
n = 5;
center = c(2, 3)
plot.base()
tmp = neuron(n = n, center = center, phi = phi)
lines(tmp)

###
phi = 2*pi - pi/6;
n = 5;
center = c(2, 3)
plot.base()
tmp = neuron(n = n, center = center, phi = phi)
lines(tmp)

### helix for ADN
tmp1 = helix(c(0, 0), c(5, 5), n = 2, phi = 0, A = 3/4)
tmp2 = helix(c(0, 0), c(5, 5), n = 2, phi = pi, A= 3/4)
plot.base()
lines(tmp1, col = "red")
lines(tmp2, col = "green")

tmp = lapply(seq(10), function(id) {
  id = 10*id;
  lines(c(tmp1[[1]]$x[id], tmp2[[1]]$x[id]),
      c(tmp1[[1]]$y[id], tmp2[[1]]$y[id]));
  })

### Trying to draw ADN
tmp1 <- helix(c(0, 0), c(5, 5), n = 2, phi = 0, A = 3/4)
tmp2 <- helix(c(0, 0), c(5, 5), n = 2, phi = pi, A= 3/4)

# Set up the plot with a white background
plot.base()

# Plot the two helices as DNA strands
lines(tmp1, col = "red")
lines(tmp2, col = "green")

# Add horizontal lines connecting the two helices
segments(x0 = -4, y0 = 0, x1 = -3, y1 = 0, col = "black", lwd = 2)
segments(x0 = 8, y0 = 0, x1 = 9, y1 = 0, col = "black", lwd = 2)

# Add base pairs to the plot
for (i in seq(from = -3, to = 7, by = 2)) {
  # Connect base pairs with lines
  segments(x0 = i, y0 = 0, x1 = i + 1, y1 = 0, col = "black", lwd = 2)
}


### for monocyte
R = 20; n = 10;
lim = c(-R, R) + c(-2,2);
plot.base(xlim = lim, ylim = lim)
abline(h=0, col="green", lty=2)
xy = helix.rad(R = R, n=n, phi=pi, r = 2);
lines(xy, col="red")


###TODO 1: Dendrite
###TODO 2: ADN



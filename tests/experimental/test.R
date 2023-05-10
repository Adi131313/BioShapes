

n = 10
R = c(3, 5)
tmp = duct(n = n, R = R, center = c(1,3))
plot.base(xlim = c (-10, 10), ylim = c(-10, 10))
lines(tmp)
#tmp = duct(n = n, R = R)
#lines(tmp)

tmp = ngon.circle(4, N = n, R = R[2], center = c(1,3))
lines(tmp, col = "blue")

# TODO: Function examples.duct
n = c(20, 15)
plot.base(xlim=c(-20,20), ylim=c(-20,20))
lines(duct(n[1], c(15,18), phi=pi/2/n[1], fill = "#0048C0"))
lines(duct(n[1], c(9,13), phi=c(0, pi/n[2]), nc.r = NULL,
           fill = "#8080F0"), lwd = 5)
lines(duct(n[1], c(5,8)))
abline(h=0, col="green")

# TODO: move simple examples to Examples.man
# TODO: create complex examples in R/Examples.R
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

### Various curves
examples.curves()

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

### Example 2: Group of Lenses
pos = c(0, 1/2, 1)
h = c(2, 3, 4)
scale.R = c(1, 1.5, 2)
x = c(0, 6); y = c(3, 2);
# fill: does NOT work with concave lenses;
fill = "#6480D0";

lst = lens.group(x=x, y=y, h=h, pos=pos, l.scale = scale.R, fill=fill)
plot.base()
lines(lst)

### Example: Lens Group
pos = c(0, 1/3, 1)
h = c(2, 1.2, 1.5)
scale.R = c(1, 1.5, 2)
x = c(0, 6); y = c(0, 4);
# fill: does NOT work with concave lenses;
fill = "#6480D0";
#
lst = lens.group(x=x, y=y, h=h, pos=pos, l.scale = scale.R, fill=fill)
plot.base()
lines(lst)
lines(x, y, lty=2, lwd=2, col="green")


### Arcs
plot.base()
plot.circle.arc(3, c(3,3), c(pi - pi/3, pi + pi/3), lty=2)

plot.base()
plot.circle.arc(3, c(3,3), c(pi/3, 2*pi/3), lty=2)

plot.base()
plot.circle.arc(3, c(3,3), c(2*pi- pi/3, pi/3), lwd=1, col="#6432B0")


### Neuron
# old functionality
n = 5; phi = 2*pi/n;
testFilledCircle(circlesOnCircle(n,2, phi=pi/n))
tmp = sapply(seq(n), function(k) points(
  2 * cos(pi/2 + k * phi) + 3.4 * cos(phi * (k-1) + phi/2),
  2 * sin(pi/2 + k * phi) + 3.4 * sin(phi * (k-1) + phi/2), col = "red"))


# For the new function
# TODO: examples with many neurons

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
lines(tmp1, col = "red", lwd = 5)
lines(tmp2, col = "green", lwd = 5)

tmp = lapply(seq(40), function(id) {
  id = 3.5*id;
  lines(c(tmp1[[1]]$x[id], tmp2[[1]]$x[id]),
      c(tmp1[[1]]$y[id], tmp2[[1]]$y[id]), col = "blue");
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


### Function calls
adn()

### Draw two or more cells
radius = 2;
lim = c(-radius, radius) * 4;
plot.base(xlim = lim, ylim = lim)
tmp = draw_blood_cell(radius = radius, center = c(-3, -1))
lines(tmp)
tmp = draw_blood_cell(radius = radius, center = c(3, 1))
lines(tmp)
tmp = draw_blood_cell(radius = radius, center = c(5, -5))
lines(tmp)re

### Group of blood cells
draw_blood_cells = function(radius = 2){
  radius = 2;
  lim = c(-radius, radius) * 4;
  plot.base(xlim = lim, ylim = lim)
  tmp = draw_blood_cell(radius = radius, center = c(-3, -1))
  lines(tmp)
  tmp = draw_blood_cell(radius = radius, center = c(3, 1))
  lines(tmp)
  tmp = draw_blood_cell(radius = radius, center = c(5, -5))
  lines(tmp)
}

draw_blood_cells()

### muscle tissue ###
scale.x = 1.5
scale.R = c(1.5, 2)
x = c(1, 5); y = c(1, 1);
mx = (x[1] + x[2])/2 + 2.7;
my = (y[1] + y[2])/2 + 2.3;
fill = "red";
plot.base()
lens = lens(x = x, y = y + 2, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x*scale.x + 1.5, y = y*scale.x + 1.1, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x + 2.7, y = y + 2.3, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x*scale.x - 4.5, y = y*scale.x + 1.1, scale=scale.R)
lines(lens, fill=fill)

# Centers for lens
lst = list(center = c(mx, my), r = 0.2, fill = "yellow")
class(lst) = c("circle", "list");
lst = as.bioshape(list(lst));
lines(lst)

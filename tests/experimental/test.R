
#TODO: move to examples with other ducts
n = 10
R = c(3, 5)
tmp = duct(n = n, R = R, center = c(1,3))
plot.base(xlim = c (-10, 10), ylim = c(-10, 10))
lines(tmp)
#tmp = duct(n = n, R = R)
#lines(tmp)

tmp = ngon.circle(4, N = n, R = R[2], center = c(1,3))
lines(tmp, col = "blue")

# TODO: move simple examples to Examples.man
# TODO: create complex examples in R/Examples.R

#TODO: create circles on the virus, see picture
# Virus
N = 10
R = 3

virus = virus(N, R = R, center = c(5, 5))
plot.base()
lines(virus)
tmp = ngon.circle(4, N = N, R = R+1+1/2, center = c(5,5))
lines(tmp, fill = "blue")

### Virus 2 spikes

plot.base(xlim = c(-6, 6), ylim = c(-6, 6))
tmp = virus2()
lines(tmp)

################


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
lens = lens(x = x + 2.65, y = y + 2.3, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x*scale.x - 4.5, y = y*scale.x + 1.1, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x -2.7, y = y + 2.3, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x*scale.x - 1.55, y = y*scale.x + 2.27, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x -4.5, y = y + 2.67, scale=scale.R)
lines(lens, fill=fill)
lens = lens(x = x -5.5, y = y + 2.05, scale=scale.R)
lines(lens, fill=fill)

# Centers for lens
lst = list(center = c(mx, my), r = 0.2, fill = "yellow")
class(lst) = c("circle", "list");
lst = as.bioshape(list(lst));
lines(lst)

# dna.new

h2 = dna.new(c(1,5), c(1,6), phi = c(0, pi))
plot.base()
lines(h2)


# TODO: fix labels
description.neuron = function(lbl = c("Axon", "Dendrites", "Nucleus"), title = "Neuron",
    lwd=2, col="#48B000", d=-0.4, cex.title = 1.5, xy.title = c(0, -6.5)){
  # TODO: parameters
  neuron = draw_neuron()
  #plot.base()
  #lines(neuron)

  # Title
  if( ! is.null(title)) text(xy.title[1], xy.title[2], title, cex=cex.title);

  # Labels
  a1 = arrowSimple(x=c(1,2), y=c(7,8), d=-d, lwd=lwd);
  text(3, 9, lbl[[1]])

  a2 = arrowSimple(x=c(1.4, 5), y=c(-2.4,-7), d=d, lwd=lwd);
  text(5, -8, lbl[[2]])

  return(invisible());
}

description.neuron()


### Complex Duct ###

# warning: just if n is even
n = 8
center = c(2, -3)
radius = c(7, 5, 2);
pos = c(0, 1)
h.scale = 1
h = radius[2]*sin(pi/n)*h.scale
h = c(h, h);
R.scale = c(1, 1)
dr = 0.25

# Cell
plot.base(xlim=c(-10,10), ylim=c(-10,10))
tmp = draw_blood_cell(radius = radius[1], center = center, col = "#bf9d9b", fill = "#f0e19e", lwd = 7)
lines(tmp)

plot.circle(r = radius[1], center = center, col = "white", lwd = 5)

# Duct
tmp = duct(n[1], radius[c(2,3)], center = center, phi=pi/2/n[1], fill = "#f0b25b", nc.fill = "#6f618f")
lines(tmp)

# Lens
n2 = n/2;
x = (radius[2] + dr)*cos((seq(n)/n2 - 1/(2*n))*pi) + center[1]
y = (radius[2] + dr)*sin((seq(n)/n2 - 1/(2*n))*pi) + center[2]

for(o in seq(n)){
  if(o <= n2){
    lst = lens.group(x=c(x[o], x[o+n2]), y=c(y[o], y[o+n2]), h=h, pos=pos, l.scale = R.scale, fill=fill)
    lines(lst)
  }
  lst = list(center = c(x[o],y[o]), r = 0.2, fill = "#8a4965")
  class(lst) = c("circle", "list");
  lst = as.bioshape(list(lst));
  lines(lst)
}


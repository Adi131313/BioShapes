# TODO: create complex examples in R/Examples.R

#TODO: colors for spikes
# Virus
N = 10
R = 3

# Virus with ngons
virus = virus(N, R = R, center = c(5, 5))
plot.base()
lines(virus)

# Virus with circles
virus = virus(N, R = R, center = c(5, 5), ngon.spike = 0)
plot.base()
lines(virus)

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

# TODO: function for muscle
### muscle tissue ###
scale.x = 1.5
scale.R = c(1.5, 1.5)
x = c(-2, 2); y = c(1, 1);
dy = 0.4 # 0.36
dx = 2
mx = (x[1] + x[2])/2;
my = (y[1] + y[2])/2;
n = 6 # number of layers
fill = "red";

lst = list()
for(iy in seq(0,n)){
  if(iy%%2 == 0){
    for(idx in seq(-2,2,2)){
      lens = lens(x = x + idx*dx, y = y + iy*dy, scale=scale.R)
      lst = c(lst, lens)
    }}
  else{
    for(idx in seq(-3,3,2)){
      lens = lens(x = x + idx*dx, y = y + iy*dy, scale=scale.R)
      lst = c(lst, lens)
    }
  }}
lst = as.bioshape(lst)
plot.base()
lines(lst, fill=fill)


# #2nd level
# lens = lens(x = x + dx, y = y + dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x + 3*dx, y = y + dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - dx, y = y + dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - 3*dx, y = y + dy, scale=scale.R)
# lines(lens, fill=fill)
# #3rd level
# lens = lens(x = x, y = y + 2*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x + 2*dx, y = y + 2*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - 2*dx, y = y + 2*dy, scale=scale.R)
# lines(lens, fill=fill)
# #4th level
# lens = lens(x = x + dx, y = y + 3*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x + 3*dx, y = y + 3*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - dx, y = y + 3*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - 3*dx, y = y + 3*dy, scale=scale.R)
# lines(lens, fill=fill)
# #5th level
# lens = lens(x = x, y = y + 4*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x + 2*dx, y = y + 4*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - 2*dx, y = y + 4*dy, scale=scale.R)
# lines(lens, fill=fill)
# #6th level
# lens = lens(x = x + dx, y = y + 5*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x + 3*dx, y = y + 5*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - dx, y = y + 5*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - 3*dx, y = y + 5*dy, scale=scale.R)
# lines(lens, fill=fill)
# #7th level
# lens = lens(x = x, y = y + 6*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x + 2*dx, y = y + 6*dy, scale=scale.R)
# lines(lens, fill=fill)
# lens = lens(x = x - 2*dx, y = y + 6*dy, scale=scale.R)
# lines(lens, fill=fill)

# Centers for lens
lst = list(center = c(mx, my), r = 0.15, fill = "yellow")
class(lst) = c("circle", "list");
lst = as.bioshape(list(lst));
lines(lst)
lst = list(center = c(mx + 2*dx, my), r = 0.15, fill = "yellow")
class(lst) = c("circle", "list");
lst = as.bioshape(list(lst));
lines(lst)
lst = list(center = c(mx - 2*dx, my), r = 0.15, fill = "yellow")
class(lst) = c("circle", "list");
lst = as.bioshape(list(lst));
lines(lst)

# dna.new

h2 = dna.new(c(1,5), c(1,6), phi = c(0, pi))
plot.base()
lines(h2)

#test function
# TODO: Also one for Virus
# TODO: Rename all examples to 'example'
description.neuron()


### Test: Bug in Function helix
plot.base();
p1 = c(3,1); p2 = c(1,5)
xy = helix(p1, p2); lines(xy);
points(c(p1[1], p2[1]), c(p1[2], p2[2]), col="red")
#
p1 = c(4,6); p2 = c(1, 5)
xy = helix(p1, p2); lines(xy);
points(c(p1[1], p2[1]), c(p1[2], p2[2]), col="red")
#
p1 = c(4,6); p2 = c(6,1)
xy = helix(p1, p2); lines(xy);
points(c(p1[1], p2[1]), c(p1[2], p2[2]), col="red")
#
p2 = c(4,6); p1 = c(1, 5)
xy = helix(p1, p2); lines(xy);
points(c(p1[1], p2[1]), c(p1[2], p2[2]), col="red")


### Synapse
plot.base()
tmp = neuron(c(0,2), phi=pi/6)
lines(tmp)
lines(synapse(c(tmp[[6]]$x[2], tmp[[6]]$y[2]), slope=tan(pi/6), type="D", l=0.8, alpha=160))

###
plot.base()
tmp = neuron(c(0,2), phi=pi/6)
lines(tmp)
lines(synapse(c(tmp[[6]]$x[2], tmp[[6]]$y[2]), slope=tan(pi/6)))

###
plot.base()
tmp = neuron(c(0,2), phi=pi/6)
lines(tmp)
lines(synapse(c(tmp[[6]]$x[2], tmp[[6]]$y[2]), slope=tan(pi/6), type="Tree", l=1.8, alpha=120))

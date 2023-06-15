# TODO: create complex examples in R/Examples.R

# Example function for virus
example.virus()
# TODO: colors for spikes

# Modified: colors, lwd for virus and spikes

# Virus
N = 12
R = 3
lwd = 10
# Virus with ngons
virus = virus(N, R = R, center = c(5, 5), lwd = lwd)
plot.base()
lines(virus)

# Virus with circles
virus = virus(N, R = R, center = c(5, 5), ngon.spike = 0, lwd = lwd)
plot.base()
lines(virus)

### Virus 2 spikes

# Modified colors, lwd for virus and spikes
# length of spikes c(0, 2), R = 2
plot.base(xlim = c(-6, 6), ylim = c(-6, 6))
tmp = virus2()
lines(tmp)

################


### helix for ADN
tmp1 = helix(c(0, 0), c(5, 5), n = 2, phi = 0, A = 3/4)
tmp2 = helix(c(0, 0), c(5, 5), n = 2, phi = pi, A = 3/4)
plot.base()
lines(tmp1, col = "red", lwd = 5)
lines(tmp2, col = "green", lwd = 5)

tmp = lapply(seq(40), function(id) {
  id = 3.5*id;
  lines(c(tmp1[[1]]$x[id], tmp2[[1]]$x[id]),
      c(tmp1[[1]]$y[id], tmp2[[1]]$y[id]), col = "blue");
})

pp = which.intersect.sin(c(0, pi), 2)
p0 = pp$x0 / (2*2*pi) * 5
abline(v = p0, col="purple")

### for monocyte
R = 20; n = 10;
lim = c(-R, R) + c(-2,2);
plot.base(xlim = lim, ylim = lim)
abline(h=0, col="green", lty=2)
xy = helix.rad(R = R, n=n, phi=pi, r = 2);
lines(xy, col="red")


### TODO 1: Dendrite as tree
### TODO 2: ADN

# TODO: Centers for muscles

# DNA

h2 = dna.new(c(1,5), c(1,6), phi = c(0, pi))
plot.base()
lines(h2)

#test function
# TODO: Also one for Virus
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
tmp = neuron(c(0,2), phi=pi/6, type.syn = "D")
lines(tmp)

###
plot.base()
tmp = neuron(c(0,2), phi=pi/6)
lines(tmp)

###
plot.base()
tmp = neuron(c(0,2), phi=pi/6, type.syn = "Tree")
lines(tmp)
# TODO: BUG for tree
lines(synapse(c(tmp[[6]]$x[2], tmp[[6]]$y[2]), slope=tan(pi/6), type="Tree", l=1.8, alpha=120))

# TODO: example of design for DNA
# Intersection of sine functions
phi = c(0.7, 1.5)
curve(sin(x + phi[1]*pi), -2*pi, 6*pi)
curve(sin(x + phi[2]*pi), add = T, col="red")
pp = which.intersect.sin(phi, 3)
abline(v = pp$x0, col="blue")
points(pp$x0, sin(pp$x1), col="green")

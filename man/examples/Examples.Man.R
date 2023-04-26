###################
#
# Bachelor Thesis
#
# Title: BioShapes
#
# Candidate: Darian Voda
# Faculty of Mathematics and Informatics, UVT
#
# Coordinator:
#   Prof. Daniela Zaharie
#   Dr. med. Leonard Mada (Syonic SRL)
#
# in collaboration with Syonic SRL
#
# GitHub: https://github.com/DarianFlorianVoda/Diagram-Generator

#' @example man/examples/Examples.Man.R

##########################
#### Helper Functions ####

### Example 1:

# Base-Line
# Note: x = (x0, x1); y = (y0, y1);

plot.base()
x = c(0,10); y = c(1, 5);
lines(x, y, lwd=2, col="red")

### Shift point along line:
p = shiftPoint(c(0,1), x, y, d=1)
points(p, col="green")
# only as example:
p = shiftPoint(c(0,2), x, y, d=seq(1, 4, by=0.5))
points(p, col="blue")


### Reflected point:
# points(x, y)!
p0 = c(1, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

#
p0 = c(4, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

#
p0 = c(5, 1)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

### Shift Line
l = shiftLine(x, y, d=2)
len = nrow(l) / 2;
sapply(seq(len), function(id) lines(l[c(id, id+len),1], l[c(id, id+len),2], col="orange"))


### Derivation of Formulas:

### Reflection:
# - slope.orthogonal = - 1 / slope;
# - intersection between base-line and reflexion line:
# y.int = (x.int - x[1])*slope + y[1] # Initial Line
# y.int = - (x.int - p[1])/slope + p[2]; # p[1] = x; p[2] = y;
# =>
# x.int = (x[1]*slope + p[1]/slope - y[1] + p[2]) / (slope + 1/slope);
# y.int = - (x.int - p[1])/slope + p[2];


### Shift:
# x = c(0,10); y = c(1, 5);
# slope = compute_slope(x, y);
# y.sh = - (x.sh - x[1]) / slope + y[1];
# (x.sh - x[1])^2 + (y.sh - y[1])^2 = d^2
# =>
# sl.o = - 1 / slope;
# (sl.o^2 + 1)*x.sh^2 - 2*x1*(sl.o^2 + 1)*x.sh + x1^2*(sl.o^2 + 1) - d^2 # = 0



################
#### Arrows ####



examples.arrows()


####################
#### Bio-Shapes ####

drawBioshapes()


#### Description of a Liposome ####

plot.base(xlim=c(-10,10), ylim=c(-10,10))
diagramLiposome(
  c("Outer lipid layer", "Inner lipid layer", "Lipid bilayer"))


#### Liposome Measurements ####

measureLiposome()


### Arrows with Enzymes ####
plot.base()
enzymeReaction(lbl = c("A2", "B2", "Enz2", "Inhibitor 2"))
enzymeReaction(y = 6, lbl = c("A1", "B1", "Enz1", "Inhibitor 1"))


#### Banded lines ####
plot.base()
lineBanded(c(0,5), c(1, 8), lwd=2.5)
lineBanded(c(1,7), c(0, 5), lwd=2.5)
lineBanded(c(0,6), c(6, 0), lwd=2.5, col="green")


### helix for ADN
n = 2.2;
tmp1 = helix(c(0, 0), c(5, 5), n = n, phi = 0, A = 3/4)
tmp2 = helix(c(0, 0), c(5, 5), n = n, phi = pi, A= 3/4)
plot.base()
col = c("#FF9696", "#F2FF82")
lines(tmp1, col = col[1], lwd = 5)
lines(tmp2, col = col[2], lwd = 5)

N = length(tmp1[[1]]$x);
nL = round(N/3.5);
scale = N/nL;
nc = ceiling(2*n);
# TODO: Proper colors
colL = rep(col[1], nL);
tmp = lapply(seq(nL), function(id) {
  idL = scale*id;
  x = c(tmp1[[1]]$x[idL], tmp2[[1]]$x[idL]);
  y = c(tmp1[[1]]$y[idL], tmp2[[1]]$y[idL]);
  d = dist.xy(x, y);
  if(d < 0.2) return(); # TODO: Parameter
  lines(x, y, col = colL[id]);
})

###################
#
# Thesis
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


### Functions to Generate Objects


# Generates a star polygon
#' @export
star = function(n, R = c(2,1), center = c(0,0), lwd=1, phi = pi/2,
                col=1, fill = NULL) {
  c1 = pointsCircle(n, r=R[1], center=center, phi=phi[1]);
  c2 = pointsCircle(n, r=R[2], center=center, phi=phi[1] + pi/n);
  x = as.vector(rbind(c1$x, c2$x));
  y = as.vector(rbind(c1$y, c2$y));
  lst = list(x = x, y = y, col = col, lwd = lwd);
  if(! is.null(fill)){
    lst$fill = fill;
  }
  class(lst) = c("polygon", "list");
  lst = list(lst);
  class(lst) = c("bioshape", "list");
  return(invisible(lst));
}

#' @export
ngon = function(n, r = 1, center = c(0, 0), phi = 0){
  p = pointsCircle(n=n, r=r, center=center, phi=phi);
  class(p) = c("polygon", "list");
  return (p);
}

#' @export
ngon.circle = function(n, N, R = 2, r = 1/2, center = c(0, 0), phi = 0){
  cp = pointsCircle(n=N, r=R, center=center, phi=phi);
  pphi = seq(0, N-1) *2*pi/N + phi;
  pg = lapply(seq(N), function(id){
    ngon(n, r = r, center = c(cp$x[id], cp$y[id]), phi = pphi[id]);
  })
  class(pg) = c("bioshape", "list");
  invisible(pg);
}

#### Liposomes ####
#' @export
liposomes = function(n, r, center=c(0, 0), phi=c(0, 0), d=0, ...){
  C1 = circlesOnCircle(n=n[1], r=r, center=center, phi=phi[1])
  C2 = circlesOnCircle(n=n[2], r=r, center=center, phi=phi[2])
  R1 = attr(C1, "R")
  R2 = attr(C2, "R")
  R1 = R1 - r
  R2 = R2 + r
  d2 = (R1-R2-d)/2
  p1 = pointsCircle(n=n[1], r=R1, phi=phi[1], center=center)
  p2 = pointsCircle(n=n[2], r=R2, phi=phi[2], center=center)
  fn = function(id, p1, p2, d){
    p1 = c(p1$x[id], p1$y[id])
    slope = slope(x=c(p1[1], p2[1]), y=c(p1[2], p2[2]))
    if(p1[1] > center[1]){
      d = -d;
    }
    pp = shiftPoint(p1, slope=slope, d=d)
    data.frame(x=c(p1[1], pp[1]), y=c(p1[2], pp[2]), id=id)
  }
  # Lines: Side Chains
  l1 = lapply(seq(n[1]), fn, p1, center, d2);
  l2 = lapply(seq(n[2]), fn, p2, center, -d2);
  l1 = do.call(rbind, l1)
  l2 = do.call(rbind, l2)
  l2$id = l2$id + nrow(l1)
  l = rbind(l1, l2);
  # Liposome:
  obj = list(C1=C1, C2=C2, l=l);
  class(obj) = c("liposome", "list");
  return(obj);
}

### Ring ###
#' @export

ring = function(n, R = c(5, 7), center = c(0,0),
                col = 1, fill = NULL, phi = c(0,0)){
  return(duct(n = n, R = R, nc.r = NULL, center = center,
              col = col, fill = fill, phi = phi))
}

### Glandular Duct ###
#' @export
duct = function(n, R = c(5, 7), nc.r=1/2, center=c(0,0),
                col = 1, fill = NULL,  nc.fill = "#E03232", phi=c(0,0)) {

  if(length(phi) == 1) phi = c(phi, phi);
  c1 = pointsCircle(n, r=R[1], center=center, phi=phi[1]);
  c2 = pointsCircle(n, r=R[2], center=center, phi=phi[2]);

  ### X
  cbx = rbind(c2$x, c1$x)
  cbx = cbind(cbx[, -1], cbx[,1])

  cells.x = rbind(c1$x, c2$x, cbx, c1$x)
  cells.x = lapply(seq(ncol(cells.x)), function(nc){
    cells.x[, nc]
  })


  ### Y
  cby = rbind(c2$y, c1$y)
  cby = cbind(cby[, -1], cby[,1])

  cells.y = rbind(c1$y, c2$y, cby, c1$y)
  cells.y = lapply(seq(ncol(cells.y)), function(nc){
    cells.y[, nc]
  })

  cells = lapply(seq(n), function(id){
    l = list(x = cells.x[[id]], y = cells.y[[id]])
    l$col = col; l$fill = fill;
    class(l) = c("polygon", "list")
    return(l)
  })

  # plot.base(xlim=c(-10,10), ylim=c(-10,10))
  # polygon(cells.x, cells.y)

  ### Nuclei:
  if(is.null(nc.r)){
    class(cells) = c("bioshape", "list")
    return(cells);
  }

  shift = function(x) c(x[-1], x[1]);

  mid1.x = (c1$x + shift(c1$x))/2;
  mid2.x = (c2$x + shift(c2$x))/2;
  mid.x = (mid1.x + mid2.x)/2;

  mid1.y = (c1$y + shift(c1$y))/2;
  mid2.y = (c2$y + shift(c2$y))/2;
  mid.y = (mid1.y + mid2.y)/2;
  # Centers:
  nuclei = list(center = cbind(mid.x, mid.y), r = nc.r, fill = nc.fill);
  class(nuclei) = c("circle", "list")
  cells = c(cells, list(nuclei));
  # TODO
  # testFilledCircle(nuclei,r=nc.r, add=TRUE, line=FALSE)
  class(cells) = c("bioshape", "list")
  return(cells);
}

# Generates a virus
#' @export
virus = function(R = 1, center = c(0,0), n.spike = 10, off.spike = c(0, 1),
                 r.spike=0.25, ngon.spike=4, phi.spike = 0, lwd = 1, lwd.spike = 2*lwd,
                 col.spike = "#D06432", col = "#D06432") {
  ### Spikes
  c1 = pointsCircle(n.spike, r = R + off.spike[1], center = center, phi = phi.spike);
  c2 = pointsCircle(n.spike, r = R + off.spike[2], center = center, phi = phi.spike);
  spike = lapply(seq(n.spike), function(id){
    list(x = c(c1$x[id], c2$x[id]), y = c(c1$y[id], c2$y[id]));
  });
  spike$lwd = lwd.spike;
  spike$col = col.spike;
  class(spike) = c("lines.list", "list");
  virus = list(spike);

  ### Envelope
  vc = list(r = R, center = center, lwd = lwd, col = col);
  class(vc) = c("circle", "list");
  c2 = cbind(c2$x, c2$y);
  # TO DO check return value of pointsCircle
  virus$virus = vc;

  ### Head of Spikes
  if(!is.null(r.spike) && r.spike > 0 ){
    if(ngon.spike > 0){
      h = ngon.circle(ngon.spike, N = n.spike, r = r.spike,
          R = R + off.spike[2] + r.spike, center = center,
          phi = phi.spike);
      virus$head = h;
    }
  }
  class(virus) = c("bioshape", "list");
  return(invisible(virus));
}

### Convex lens
#' @export
lens = function(x, y, R = NULL, scale = c(1,1),
                lwd=1, col=1) {
  d2 = (x[2] - x[1])^2 + (y[2] - y[1])^2;
  d  = sqrt(d2);
  ### Lens Radius
  if(is.null(R)) {
    R = d;
    R = c(R, R);
  } else if(length(R) == 1) R = c(R, R);
  R = R * scale;

  ### Center of Base
  mid.x = (x[1] + x[2]) / 2;
  mid.y = (y[1] + y[2]) / 2;

  ### Step 3
  dc = R^2 - d2/4;
  if(any(dc < 0)) stop("Invalid Lens Radius: too small!");
  dc = sqrt(dc) * sign(R);
  # TODO: Proper sign of d1 and d2
  d1 = dc[1];
  d2 = - dc[2];

  # Step 4: Circle Centers
  slope = slope(x, y);
  c1 = shiftLine(mid.x, mid.y, slope = slope, d = d1);
  c2 = shiftLine(mid.x, mid.y, slope = slope, d = d2);

  # Step 5: Circle Segments
  R = abs(R);
  alpha = asin(d/(2*R));
  phi1 = atan2((mid.y - c1$y), (mid.x - c1$x));
  phi2 = atan2((mid.y - c2$y), (mid.x - c2$x));
  clock = - c(-1,1);
  phi1 = phi1 + clock*alpha[1];
  phi2 = phi2 + clock*alpha[2];
  phi1 = rev(phi1);
  phi2 = rev(phi2);

  # TODO: handle data.frame?
  c1 = unlist(c1[1, c(1, 2)]);
  c2 = unlist(c2[1, c(1, 2)]);
  lst1 = list(r = R[1], center = c1, phi = phi1);
  lst2 = list(r = R[2], center = c2, phi = phi2);
  class(lst1) = c("circle.arc", "list");
  class(lst2) = c("circle.arc", "list");

  lst = list(lst1, lst2);
  class(lst) = c("bioshape", "list");
  return(lst);
}


#' @export
draw_blood_cell = function(radius = 1) {
  plot.window(xlim = c(-radius, radius), ylim = c(-radius, radius))
  circle <- seq(0, 2 * pi, length.out = 80)
  x <- radius * sin(circle)
  y <- radius * cos(circle)
  polygon(x, y, col = "red", border = "#901000", lwd = 20)
}

shiftPoint <- function(p, d, slope) {
  if (slope == Inf) {
    return(c(p[1], p[2] + d));
  } else {
    a = atan(slope);
    dx = d * cos(a);
    dy = d * sin(a);
    return(c(p[1] + dx, p[2] + dy));
  }
}

### Neuron body
#' @export
neuron = function(center = c(0, 0), n = 5, r = 2, phi = 0,
                  axon.length = 3 * r, dendrite.length = ~ r/2, r.nucl = ~ (R - r)/2,
                  col.nucl = 1, fill.nucl = NULL) {
  body = neuron.body(center = center, n = n, r = r, phi = phi);
  ### Init
  axon.length = axon.length; # force = scale * r;
  R = r;
  r = attr(body, "r");
  phin = 2 * pi/n;
  phi0 = - phin/2;
  pi20 = pi/2; pi32 = 3*pi/2;
  phiD = seq(n) * phin + phi;
  phiR = phiD - (2*pi) * floor(phiD / (2*pi));
  x0 = r * cos(phiD + pi20) + R * cos(phiD + phi0);
  y0 = r * sin(phiD + pi20) + R * sin(phiD + phi0);
  x0 = x0 + center[1];
  y0 = y0 + center[2];
  sg = ifelse(phiR > pi/2 & phiR <= pi32, - 1, 1);
  ### Axon
  if(axon.length != 0) {
    xy = shiftPoint(c(x0[n], y0[n]), d = sg[n] * axon.length, slope = tan(phiD[n]));
    axon = list(x = c(x0[n], xy[1]), y = c(y0[n], xy[2]));
    axon = list(axon);
    lenDendites = n - 1;
  } else {
    axon = NULL;
    lenDendites = n;
  }
  ### Dendrites
  eval.formula = function(x) {
    xx = if(inherits(x, "formula")) {
      eval(x[[2]]);
    } else x;
  }
  dendrite.len = eval.formula(dendrite.length);
  dendrite.len = sg * dendrite.len;
  dend = lapply(seq(lenDendites), function(k) {
    tree(c(x0[k], y0[k]), d = dendrite.len[k], slope = tan(phiD[k]),
         n = 2, levels = 2); # TODO
  })
  ### Nucleus
  r.nucl = eval.formula(r.nucl);
  if(r.nucl > 0) {
    nucleus = list(r = r.nucl, center = center, col = col.nucl, fill = fill.nucl);
    class(nucleus) = c("circle", "list");
    nucleus = list(nucleus);
  } else nucleus = NULL;
  ### Neuron
  neuron = c(body, axon, dend, nucleus);
  if( ! inherits(neuron, "bioshape")) {
    class(neuron) = c("bioshape", "list");
  }
  return(neuron);
}
#' @export
tree = function(p, d, slope, n=2, levels=2) {
  xy = shiftPoint(p, d=d, slope = slope);
  xy = list(x = c(p[1], xy[1]), y = c(p[2], xy[2]));
  return(xy);
#
  xy = shiftPoint(p, d=d, slope = slope)
  x = c(p[1], xy[1])
  y = c(p[2], xy[2])
  if (n > 2) {
    control_points = matrix(nrow = n-2, ncol = 2)
    for (i in 2:(n-1)) {
      control_points[i-1,] = (p + xy)/2 + c(-1, 1)*abs((xy-p)[2]/10)*(-1)^i
    }
    bezier_points = bezier(x, y, control_points = control_points)
    x = bezier_points$x
    y = bezier_points$y
  }
  xy = list(x = x, y = y)
  return(xy)
}

#' @export
neuron.body = function(center = c(0, 0), n = 5, r = 3, phi = 0){
  phi0 = phi + pi/n;
  cc = circlesOnFixedCircle(n = n, r = r, center = center, phi = phi0);
  R = r;
  r = attr(cc, "r");
  phin = 2*pi/n;
  id = seq(n);
  a1 = pi/2 + phi + id * phin;
  a2 = pi/2 + phi + (id + (n - 2)/2) * phin;
  lst = lapply(id, function(id){
    lst = list(
      r = r, center = c(x = cc$x[id], y = cc$y[id]),
      phi = c(a1[id], a2[id]) );
    class(lst) = c("circle.arc", "list");
    return(lst);
  })
  attr(lst, "r") = r;
  class(lst) = c("bioshape", "list");
  return(lst);
}


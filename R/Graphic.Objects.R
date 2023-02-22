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
    slope = compute_slope(x=c(p1[1], p2[1]), y=c(p1[2], p2[2]))
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

### Virus
#' @export

virus = function(n, R = 5, sp.r = 1/2, center = c(0,0),
                col = 1, fill = NULL,  sp.fill = "#E03232", phi = 0) {

}

# n = number of loops;
# N = number of points to draw curve;
# A = amplitude;
# phi = phase shift of sinusoid;
#' @export
helix = function(p1, p2, n=3, A=1, phi=0, N=128, slope=NULL) {
  if(is.null(slope)) {
    x = c(p1[1], p2[1]);
    y = c(p1[2], p2[2]);
    slope = compute_slope(x, y);
    l = sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2);
  } else {
    if(length(p2) > 1) stop("Provide either: length and slope, or the 2 endpoints!")
    l = p2;
  }
  #
  n = 2*n*pi;
  ninv = 1 / n;
  t  = seq(0, n, length.out=N);
  x  = l*t*ninv;
  y  = A*sin(t + phi);
  #
  if(abs(slope) == Inf) {
    sgn = sign(slope);
    dx = y; dy = x * sgn;
    lst = list(x = p1[1] + dx, y = p1[2] + dy);
    lst = list(lst);
    class(lst) = c("bioshape", class(lst));
    return(lst);
  }
  sdiv = 1 / sqrt(slope^2 + 1);
  if(p1[2] > p2[2] && slope > 0) { x = -x; }
  # Rotation matrix: by column
  # rotm = matrix(sdiv * c(1, s, -s, 1), ncol=2, nrow=2);
  dx = (x - slope*y) * sdiv; # + p1[1];
  dy = (slope*x + y) * sdiv; # + p1[2];
  lst = list(x = p1[1] + dx, y = p1[2] + dy);
  lst = list(lst);
  class(lst) = c("bioshape", class(lst));
  return(lst);
}

#' @export
spirals = function(p1, p2, n=5.5, A=1, phi=0, N=128, slope=NULL) {
  if(is.null(slope)) {
    x = c(p1[1], p2[1]);
    y = c(p1[2], p2[2]);
    slope = compute_slope(x, y);
    l = sqrt((p1[1]- p2[1])^2 + (p1[2]- p2[2])^2);
  } else {
    if(length(p2) > 1) stop("Provide either: length and slope, or the 2 endpoints!")
    l = p2;
  }
  #
  n = 2*n*pi;
  ninv = 1 / n;
  v = l * ninv;
  t = seq(0, n, length.out=N);
  # Rotation matrix: by column
  # rotm = matrix(sdiv * c(1, s, -s, 1), ncol=2, nrow=2);
  if(slope == -Inf || (slope != Inf && p1[2] > p2[1])){
    v = - v;
  } else { phi = phi + pi; }
  x  = v*t;
  y  = A*sin(t + phi);
  xc = x + cos(t + phi);
  #
  if(abs(slope) == Inf) {
    sgn = sign(slope);
    dx  = y;
    dy  = xc;
    lst = list(x = p1[1] + dx, y = p1[2] + dy);
    lst = list(lst);
    class(lst) = c("bioshape", class(lst));
    return(lst);
  }
  sdiv = 1 / sqrt(slope^2 + 1);
  #
  dx = (xc - slope*y) * sdiv; # + p1[1];
  dy = (slope*xc + y) * sdiv; # + p1[2];
  lst = list(x = p1[1] + dx, y = p1[2] + dy);
  lst = list(lst);
  class(lst) = c("bioshape", class(lst));
  return(lst);
}


###########################

#### Cell-like Objects ####

#### Cells resembling a smooth muscle cell ####
#' @export
cellSmooth = function(x, y, r=1, slope=NULL, lwd=1, N=128, phi=pi) {
  if(is.null(slope)) slope = compute_slope(x, y);
  d = sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2);
  dx = d / N;
  pL = seq(0, d, by=dx);
  pp = shiftPoint(c(x[1], y[1]), d=pL, slope=slope);
  pL = pL + x[1];
  px = pp[,1] - x[1]; px = px * pi / max(abs(px));
  # Margin 1:
  pS = r * sin(px) + pp[,2];
  lst = list(x = pL, y = pS);
  # Margin 2:
  pS = r * sin(px + phi) + pp[,2];
  lst = list(lst, list(x = pL, y = pS));
  #
  lst$lwd = lwd;
  return(lst)
}


#### brush-Border Cells ####
# p1 = Base of Cell, Point 1;
#' @export
cellBrushBorder = function(p1, w, h, n=6.5, A=1, slope=0, lwd=1, N=128, phi=0) {
  # Cell:
  p11 = p1;
  p12 = shiftPoint(p1,  d=w, slope=slope);
  p21 = shiftLine( p1,  d=h, slope=slope);
  p21 = unlist(p21);
  p22 = shiftPoint(p21, d=w, slope=slope);
  # Brush-Border:
  brush = helix(p21, p22, n=n, A=A, phi=phi, N=N);
  brush[[1]]$x = c(brush[[1]]$x, p22[1], p12[1], p11[1], p21[1], brush[[1]]$x[[1]]);
  brush[[1]]$y = c(brush[[1]]$y, p22[2], p12[2], p11[2], p21[2], brush[[1]]$y[[1]]);
  brush$lwd = lwd;
  return(brush);
}

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

virus = function(R = 1, center = c(0,0), n.spike = 10, off.spike = c(-0.1, 1),
                 r.spike=0.25, phi.spike = 0, lwd = 1, lwd.spike = 2*lwd,
                 col.spike = "#D06432", col = "#D06432") {

  c1 = pointsCircle(n.spike, r = R + off.spike[1], center = center, phi = phi.spike);
  c2 = pointsCircle(n.spike, r = R + off.spike[2], center = center, phi = phi.spike);
  spike = lapply(seq(n.spike), function(id){
    list(x = c(c1$x[id], c2$x[id]), y = c(c1$y[id], c2$y[id]));
  });
  spike$lwd = lwd.spike;
  spike$col = col.spike;
  virus = list(spike);
  vc = list(r = R, center = center, lwd = lwd, col = col);
  class(vc) = c("circle", "list");
  c2 = cbind(c2$x, c2$y);
  # TO DO check return value of pointsCircle
  virus$virus = vc;
  class(virus) = c("bioshape", "list");
  return(invisible(virus));
}

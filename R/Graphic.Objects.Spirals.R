# p1, p2 = endpoints of spiral;
#  OR if slope specified = starting point + length;

#' @export
spirals = function(p1, p2, n=5.5, A=1, phi=0, N=128, slope=NULL) {
  if(is.null(slope)) {
    x = c(p1[1], p2[1]);
    y = c(p1[2], p2[2]);
    slope = slope(x, y);
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


# n = number of loops;
# N = number of points to draw curve;
# A = amplitude;
# phi = phase shift of sinusoid;
#' @export
helix = function(p1, p2, n=3, A=1, phi=0, N=128, slope=NULL) {
  if(is.null(slope)) {
    x = c(p1[1], p2[1]);
    y = c(p1[2], p2[2]);
    slope = slope(x, y);
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
helix.rad = function(R=3, n=8, center=c(0,0), r=1, phi=0, N=257) {
  id = seq(0, 1, length.out = N)
  rr = r * cos(2*pi*n*id + phi);
  RR = exp(2i*pi*id);
  RR = R*RR + rr*RR;
  x  = Re(RR) + center[1];
  y  = Im(RR) + center[2];
  xy = list(x=x, y=y);
  class(xy) = c("polygon", "list");
  xy = list(xy);
  class(xy) = "bioshape";
  return(xy);
}

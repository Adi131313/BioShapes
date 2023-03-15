###########################

#### Cell-like Objects ####

#### Cells resembling a smooth muscle cell ####
#' @export
cellSmooth = function(x, y, r=1, slope=NULL, lwd=1, N=128, phi=pi) {
  if(is.null(slope)) slope = slope(x, y);
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

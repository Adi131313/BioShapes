###################
#
# Bachelor Thesis
#
# Title: BioShapes
#
# Candidate: Adrian Cotoc
# Faculty of Mathematics and Informatics, UVT
#
# Coordinator:
#   Prof. Daniela Zaharie
#   Dr. med. Leonard Mada (Syonic SRL)
#
# in collaboration with Syonic SRL
# continous the work of Darian Voda
#
# GitHub: https://github.com/Adi131313/BioShapes

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

### Muscle tissue ###
#' @export
muscle = function(scale.x = 1.5, scale.R = c(1.5, 1.5),
                  x = c(-2, 2), y = c(1, 1), dy = 0.4, dx = 2, n = 6, fill = "red"){

  lst = list()
  for(iy in seq(0,n)){
    if(iy%%2 == 0){
      for(idx in seq(-2,2,2)){
        lens = lens(x = x + idx*dx, y = y + iy*dy, scale=scale.R, fill = fill)
        lst = c(lst, lens)
      }}
    else{
      for(idx in seq(-3,3,2)){
        lens = lens(x = x + idx*dx, y = y + iy*dy, scale=scale.R, fill = fill)
        lst = c(lst, lens)
      }
    }}
  lst = as.bioshape(lst)
  return (invisible(lst))
}

### Red blood cells
#' @export
draw_blood_cell = function(center = c(0, 0),
                           radius = 1, lwd = 10, col = "#901000", fill = "red"){
  #lim = c(-radius, radius) * 2;
  #plot.base(xlim = lim, ylim = lim)
  #circle <- seq(0, 2 * pi, length.out = 80)
  #x <- radius * sin(circle)
  #y <- radius * cos(circle)
  #polygon(x, y, col = "red", border = "#901000", lwd = lwd)
  lst = list(center = center, r = radius,
             fill = fill, col = col, lwd = lwd);
  class(lst) = c("circle", "list");
  lst = list(lst);
  class(lst) = c("bioshape", "list");
  return(lst)

}

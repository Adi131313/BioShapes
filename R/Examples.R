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


##########################
#### Helper Functions ####
#### Demo / Examples  ####

### Various BioShapes
#' @export
drawBioshapes = function(col = list("#48B000", 1, 1, c("blue", "red"), c("purple", "orange")),
		lwd=2, y.txt = c(6, 0), axt=c(1,2)) {
	if(length(lwd) == 1) lwd = rep(lwd, 5);
	# Plot
	plot.base(xlim=c(-1,10), ylim=c(-1,10), axt=axt);
	
	### Row 1:
	
	### Ex 1: Liposome
	lst = liposomes(c(30, 17), r=0.15, phi=c(0, pi/34), d=0.1, center=c(0.5, 8));
	lines(lst, lwd=lwd, col=col[[1]]);
	text(0.5, y.txt[1], "Liposome");
	
	
	### Ex 2: Brush-Border Cell
	p1 = c(3, 6.5)
	cell = cellBrushBorder(p1, w=2, h=2.5, A=1/2);
	lines(cell, lwd=lwd[2], col=col[[2]]);
	text(4, y.txt[1], "Brush-Border Cell");
	
	
	### Ex 3: Smooth Muscles / Connective Tissue
	# Cell 1:
	lst = cellSmooth(c(6, 9), c(8, 9), r=0.3);
	lines.object.base(lst, lwd=lwd[3], col=col[[3]]);
	# Cell 2:
	lst = cellSmooth(c(6, 9), c(6.5, 8), r=0.4);
	lines.object.base(lst, lwd=lwd[3], col=col[[3]]);
	text(7.5, y.txt[1], "Smooth Muscles");
	
	
	### Row 2:
	
	### Ex 4: Vertical Helix
	p1 = c(2.5, 0.5); p2 = c(p1[1], 5);
	lst1 = helix(p1, p2);
	lst2 = helix(p1, p2, phi=-pi/2);
	lines(lst1, col=col[[4]][1], lwd=lwd[4])
	lines(lst2, col=col[[4]][2], lwd=lwd[4])
	text(2.5, y.txt[2], "Helix/DNA");
	
	### Ex 5: Vertical Spirals
	p1 = c(5.5, 1.5); p2 = c(p1[1], 4); dx = c(2.25, 0);
	lst1 = spirals(p1, p2)
	lst2 = spirals(p2 + dx, p1 + dx)
	lines(lst1, col=col[[5]][1], lwd=lwd[5])
	lines(lst2, col=col[[5]][2], lwd=lwd[5])
	text(6.5, y.txt[2], "Spirals/Coils");
}


#### Diagram of a Liposome ####
# d = Dimensions of ArrowHead;
#' @export
diagramLiposome = function(lbl = "", title = "Liposome",
		lwd=2, d=-0.4, n = c(30, 17), col="#48B000", cex.title = 1.5, xy.title = c(0, -6.5)) {
  if(length(lbl) == 1) lbl = rep(lbl, 3);
  # Liposome
  lst = liposomes(n, r=0.5, phi=c(0, pi/(2*n[[2]])), d=0.2)
  lines(lst)

  # Title
  if( ! is.null(title)) text(xy.title[1], xy.title[2], title, cex=cex.title);

  # Left arrow
  # TODO: fix (-d)!
  a1 = arrowSimple(x=c(-2.7,-5), y=c(-4.6,-8), d=-d, lwd=lwd);
  text(-5.5, -9, lbl[[1]])

  # Right arrow
  a2 = arrowSimple(x=c(1.4, 5), y=c(-2.4,-7), d=d, lwd=lwd);
  text(5, -8, lbl[[2]])

  # Upper arrow
  a3 = arrowSimple(x=c(0.08, 1), y=c(3.5,8), d=d, lwd=lwd);
  text(1, 9, lbl[[3]])
}


#### Liposome Measurements ####
#' @export
measureLiposome = function(lbl = c("D = 60 nm", "d=50"), center=c(0,0), add=FALSE,
		lwd.arrow=2, lwd=1.5, title="Liposome", xy.title = c(0,-6.5), cex.title=1.5, ...) {
  if( ! add) plot.base(xlim=c(-10,10), ylim=c(-10,10));
  
  # Liposome
  lst = liposomes(c(30, 17), r=0.5, phi=c(0, pi/34), d=0.2, center=center);
  lines(lst, lwd=lwd, ...);
  text(xy.title[1], xy.title[2], title, cex=cex.title);

  # Measurement arrow D1
  x = center[1] + c(6,6); y = center[2] + c(-5, 5);
  measure(x, y, lwd=lwd.arrow);
  xy = c(7, 0) + center;
  text(xy[1], xy[2], srt=-90, lbl[1]);

  # Measurement arrow d2
  x = center[1] + c(-2,2); y = center[2] + c(0, 0);
  measure(x, y, lwd=lwd.arrow, d=c(-0.5, 0.5));
  xy = c(0,1) + center;
  text(xy[1], xy[2], lbl[2]);
}

### Example Enzyme ####
#' @export
enzymeReaction = function(x = c(2,5), y = c(1,1),
                          lbl = c("A", "B", "Enzyme", "Inhibitor"),
                          col = c("black", "black", "black", "red", "red"),
                          dx=1, dy=c(0.1, 0.1, 0.5), dI= - c(2, 0.75, 2.4), dH=0.5,
                          lwd=c(1, 2), scale=1) {
  if(length(y) == 1) y = c(y, y);
  slope = compute_slope(x, y);
  l1 = shiftLine(x, y, d = dy[[1]], slope=slope, scale=scale);
  l2 = shiftLine(rev(x), rev(y), d = - dy[[2]], slope=slope, scale=scale);
  #
  arrowSimple(l1$x, l1$y, d = -dH, d.head=c(0, 0.5), col=col[[1]], lwd=lwd[[1]]);
  arrowSimple(l2$x, l2$y, d = dH, d.head=c(0, - 0.5), col=col[[1]], lwd=lwd[[1]]);
  text(x[1] - dx, y[1], lbl[[1]], col=col[[2]]);
  text(x[2] + dx, y[2], lbl[[2]], col=col[[2]]);
  # Enzyme
  midx = sum(x)/2;
  midy = sum(y)/2;
  mid  = shiftLine(midx, midy, d = dy[[3]], slope=slope);
  text(mid$x, mid$y, lbl[[3]], col=col[[3]]);
  # Inhibitor
  if(length(lbl) > 3) {
    slopeT = -1/slope;
    pI = shiftPoint(c(midx, midy), d=dI, slope=slopeT, scale=scale);
    arrowT(pI[1:2, 1], pI[1:2, 2], col=col[[4]], lwd=lwd[[2]]);
    text(pI[3,1], pI[3,2], lbl[[4]], col=col[[5]]);
  }
}
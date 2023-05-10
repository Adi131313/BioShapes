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


#' @example man/examples/Examples.Man.R




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


### DNA Structure ###

adn()


### Blood Cells ###

draw_blood_cells()


### Neuron ###

draw_neuron()


### Muscle Tissue ###




###

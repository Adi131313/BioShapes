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
diagramLiposome()

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

### Creation of the neuron ###

draw_neuron_design()

### Neuron ###

draw_neuron()

### Multiple neurons ###
# TODO: move neurons around
draw_neurons()

### Muscle Tissue ###



### Star Shape ###

draw_star(n=8)

### Simple Braces ###

draw_braces()

### Various Curves ###

examples.curves()

### Arcs Examples ###
# TODO: create the 4th lens
draw_arcs()

### Lens Examples ###

draw_examples_lens()

### Duct Examples ###

examples.ducts()

### Complex Duct ###

draw_complex_duct(n = 12)

###

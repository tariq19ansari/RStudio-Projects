#################### Chernoff faces of the first 20 obs ################
install.packages("aplpack")
library(aplpack)

## Loading required package: tcltk
## Loading Tcl/Tk interface ...
## done
faces(iris_sample_dataset[1:20, 1:4], main = "Faces for Iris Data", label = iris_sample_dataset$Species)

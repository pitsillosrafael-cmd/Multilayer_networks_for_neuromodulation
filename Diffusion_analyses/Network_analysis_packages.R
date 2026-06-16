#  for ggseg: Neuroimaging analyses produce region-level results – cortical thickness, 
#  p-values, network assignments – that need to end up on a brain figure. ggseg stores 
#  brain atlas geometries as simple features and plots them as ggplot2 layers, so you get
#  publication-ready brain figures with the same code you’d use for any other ggplot.
install.packages("ggseg")

#  for ggseg3D:Interactive 3D brain atlas visualization in R. Plot brain parcellations
#  as WebGL meshes powered by Three.js, or render publication-quality static images through
#  rgl and rayshader. A pipe-friendly API lets you map data onto brain regions, control camera
#  angles, toggle region edges, overlay glass brains, and snapshot the result.

options(
  repos = c(
    ggsegverse = "https://ggsegverse.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
install.packages("ggseg3d")

#  graph theory analyses of brain MRI data. It is most useful in atlas-based analyses 
#  (e.g., using an atlas such as AAL, or one from Freesurfer); however, many of the computations 
#  (e.g., the GLM-based functions and the network-based statistic) will work with any graph that 
#  is compatible with igraph. The package will perform analyses for structural covariance networks 
#  (SCN), DTI tractography (I use probtrackx2 from FSL)
install.packages('brainGraph')
install.packages('brainGraph', dependencies=TRUE)


# BrainConn package, no updates available (older version)
install.packages("remotes")
remotes::install_github("sidchop/brainconn")

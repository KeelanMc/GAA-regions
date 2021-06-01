# GAA-regions
Analysis of the spatial distributions of Hurling and Gaelic Football clubs in Ireland

By scrapping data from gaapitchlocator.net and Wikipedia using beautiful soup in Pyhton, I created maps of the succesful GAA clubs in R.

The intial data must be scrapped using the python script "Scarping club data.ipynb" 

The required r libraries are contained in "0_source.R"
The maps can then be created using "MapMaking.R"
The spatial statistical analysis can be created using "Moran_plots.R", "Random_dist_test.R" and "Spatial_model_fit.R"

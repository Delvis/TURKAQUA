# TURKAQUA
Geospatial paleoecology: estimating aquatic and terrestrial fossil abundance in East Lake Turkana

## Available Files

- `0-LoadData.R`
The code performs preprocessing for environmental and geographical data, including reading and cropping satellite imagery, reconstructing DEM data, calculating vegetation indices, and preparing datasets for modeling by incorporating geological and bone walk data for the East Turkana region.

- `1-Model-A_BetaReg_bone_walks.R`
The code generates a bar plot (Figure 1) depicting the percentage of aquatic fauna across different bonewalks, conducts beta regression modeling using environmental and geographical data, evaluates model performance through leave-one-out cross-validation, and assesses significance by generating and analyzing p-values from t-tests on random samples. The results are visualized through figures and statistical summaries.
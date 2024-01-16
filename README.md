# TURKAQUA
Geospatial paleoecology: estimating aquatic and terrestrial fossil abundance in East Lake Turkana

## Available Files

- `0-LoadData.R`
The code performs preprocessing for environmental and geographical data, including reading and cropping satellite imagery, reconstructing DEM data, calculating vegetation indices, and preparing datasets for modeling by incorporating geological and bone walk data for the East Turkana region.

- `1-Model-A_BetaReg_bone_walks.R`
The code generates a bar plot (Figure 1) depicting the percentage of aquatic fauna across different bonewalks, conducts beta regression modeling using environmental and geographical data, evaluates model performance through leave-one-out cross-validation, and assesses significance by generating and analyzing p-values from t-tests on random samples. The results are visualized through figures and statistical summaries.

- `2-Model-B_MaxEnt_filter.R`
The provided R code involves MaxEnt modeling and filtering for fossiliferous areas in East Turkana. It includes building a MaxEnt model, visualizing global predictions, filtering non-fossiliferous regions, and comparing MaxEnt and Beta Regression predictions. Additionally, it performs MaxEnt model validation using presence and absence data, computes evaluation metrics, and generates ROC curves. The results are saved as multiple figures, offering insights into the spatial distribution of fossiliferous areas and model performance.

- `3-Stats_Visualizations.R`
The code generates summary stats and visualizations comparing the MaxEnt and Beta Regression models in predicting fossil distribution. It includes boxplots of aquatic percentages across study areas and bar charts showing predictor contributions. The final multi-plot figure integrates these visualizations for model comparison.




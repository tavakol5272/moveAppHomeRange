# Home range (Kernel Utilization Distribution)

MoveApps

Github repository: https://github.com/chlobeau/moveAppHomeRange
## Description
This app estimates home ranges using kernel density estimation to derive the utilization distribution (UD). 
It generates home-range area estimates, polygon files, and an interactive map of kernel-based home ranges at a user-defined percent level, 
using the adehabitatHR R package (Calenge, 2006). Results are provided both at the population level and for each individual in the dataset.

## Documentation

This app estimates home ranges using kernel density estimation to derive the utilization distribution (UD). 
For background on this method, see Worton (1989). The estimation is implemented with the adehabitatHR R package (Calenge, 2006).

The app generates kernel home-range polygons at a user-defined percent level. 
It produces both population-level results, based on all individuals in the dataset, and individual-level results for each track. Outputs include an interactive map, spatial files, and a table of home-range areas.

To reduce the effects of autocorrelation in the data, Preparing the workflow: To reduce autocorrelation in the data, we recommend thinning the input to one location per animal per week before running the analysis. 
This can be done using the Movebank Location app or the Thin Data by Time app.


At least 5 locations per animal are required to calculate individual KUDs. 
If necessary, you can use the Filter by Track Duration app to identify and remove tracks with fewer than 5 occurrences.

To calculate KUDs for each animal, the input tracks should represent individual animals. 
If one animal has multiple tracks, such as separate deployments or season-year segments, the app will estimate a separate KUD for each track.

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. 

#### Required data properties
The App should work for any kind of (location) data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
This app provides the following downloadable artefacts:

`homerange_kud_perc_hest_res_ext_areas.csv`: KUD Areas Table; A table containing the estimated home-range area for each individual track, together with the selected KUD percentage level, grid resolution, and extent.

`homerange_kud_perc_hest_res_ext.html`: Interactive Map; An interactive Leaflet map showing the movement tracks, individual kernel home-range polygons, and the population-level KUD polygon on selectable background maps.

`homerange_kud_perc_hest_res_ext.png`: Map Export; A static image export of the current interactive map view.

`homerange_kud_perc_hest_res_ext.kmz`: KMZ File; A spatial file for viewing the estimated home-range polygons in applications such as Google Earth.

`homerange_kud_perc_hest_res_ext.gpkg`: GeoPackage; A spatial file containing the estimated home-range polygons, suitable for use in GIS software such as QGIS or ArcGIS.

### Settings

**"Tracks"**: select one or multiple individuals to include in the home-range estimation. Buttons are available to select or unselect all tracks. Only tracks with at least 5 locations are available for analysis. If no track is selected, a red warning (“No track selected”) is shown and the map is not updated.

**"% of points included in KUD"**: defines the home-range contour level extracted from the utilization distribution, for example 95 for a 95% KUD.

**"Bandwidth selection (hest)"**: chooses the smoothing parameter `h` used in `kernelUD()` for kernel density estimation. Available options are `href`, `LSCV`, and `custom`.

* **`href`**: uses the reference bandwidth (ad hoc method), which generally produces a smoother utilization distribution. For `href`, a smaller extent and lower grid resolution are usually sufficient. A practical starting point is an extent of `0.5–1.0` and a grid resolution of `100–200`.
* **`LSCV`**: estimates the smoothing parameter using Least Square Cross Validation. This is more data-driven, but in some cases the minimization may be unstable. Because it often leads to a smaller bandwidth, it may require a larger extent and higher grid resolution. A practical starting point is an extent of `0.7–1.2` and a grid resolution of `300–500`.
* **`custom`**: allows the user to provide a numeric bandwidth value manually. Smaller custom bandwidth values usually require a higher resolution and larger extent, whereas larger custom bandwidth values usually work with a lower resolution and smaller extent.

**"Custom bandwidth value"**: shown only when custom is selected. This number defines how strongly the home-range estimation is smoothed.

**"Grid resolution (res)"**: `grid` argument in `kernelUD()`. It controls the size of the grid on which the utilization distribution is estimated. Higher values produce a finer, more detailed surface, but may increase computation time. If calculation errors occur, increasing the grid resolution may help for small areas, whereas lower resolution may be more suitable for wide-ranging movements.

**"Extent (space around data area)"**: corresponds to the `extent` argument in `kernelUD()`. It controls how much extra space around the observed locations is included in the grid used for estimation. Larger values include more surrounding area in the KUD calculation. If calculation errors occur, adjusting the extent together with the resolution may improve the estimation.

**"Apply Changes"**: updates the map and all downloadable outputs using the currently selected tracks and parameter settings. Until this button is clicked, changes in the sidebar do not update the displayed results.

**"Download"**:
Save map as HTML: locally downloads the current map in HTML format.
Save map as PNG: locally downloads the current map in PNG format.
Download as KMZ: locally downloads the home-range polygons in KMZ format for viewing in Google Earth.
Download as GPKG: locally downloads the home-range polygons in GeoPackage format for use in GIS software such as QGIS or ArcGIS.
Download KUD Areas Table: locally downloads a CSV table with the estimated home-range area for each individual track.


### Changes in output data

The input data remains unchanged and is passed on as output.

### Null or error handling

**Empty input**: If the input data are NULL or contain zero rows, the app returns NULL.  
**Track selection**: If no track is selected, a red warning (“No track selected”) is shown and the map is not updated.  
**Number of locations**: Tracks with fewer than 5 locations are excluded from the analysis. If no tracks remain after filtering, the app shows “No tracks with at least 5 locations.  
**KUD estimation failure**: Some combinations of bandwidth, grid resolution, and extent may cause the kernel estimation to fail or produce unstable results. In such cases, adjusting hest, res, or ext may help.  
**Large datasets**: Very large input datasets may reduce Shiny UI performance and increase computation time.  
# Home range (Kernel Utilization Distribution)

MoveApps

Github repository: https://github.com/chlobeau/moveAppHomeRange
## Description
This app estimates home ranges using kernel density estimation to derive the utilization distribution (UD). 
It generates home-range area estimates, polygon files, and an interactive map of kernel-based home ranges at a user-defined percent level, 
using the adehabitatHR R package (Calenge, 2006). Results are provided both at the population level and for each individual in the dataset.

To reduce the effects of autocorrelation in the data, we recommend filtering the input to one location per week before running the analysis.

## Documentation

This app estimates home ranges using kernel density estimation to derive the utilization distribution (UD). 
For background on this method, see Worton (1989). The estimation is implemented with the adehabitatHR R package (Calenge, 2006).

The app generates kernel home-range polygons at a user-defined percent level. 
It produces both population-level results, based on all individuals in the dataset, and individual-level results for each track. Outputs include an interactive map, spatial files, and a table of home-range areas.

Preparing the workflow: To reduce autocorrelation in the data, we recommend thinning the input to one location per animal per week before running the analysis. 
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

`homerange_kud_..._areas.csv`: KUD Areas Table; A table containing the estimated home-range area for each individual track, together with the selected KUD percentage level, grid resolution, and extent.

`homerange_kud_....html`: Interactive Map; An interactive Leaflet map showing the movement tracks, individual kernel home-range polygons, and the population-level KUD polygon on selectable background maps.

`homerange_kud_....png`: Map Export; A static image export of the current interactive map view.

`homerange_kud_....kmz`: KMZ File; A spatial file for viewing the estimated home-range polygons in applications such as Google Earth.

`homerange_kud_....gpkg`: GeoPackage; A spatial file containing the estimated home-range polygons, suitable for use in GIS software such as QGIS or ArcGIS.

### Settings 
**"Tracks"**: select one or multiple individuals to include in the home-range estimation. Buttons are available to select or unselect all tracks. Only tracks with at least 5 locations are available for analysis. If no track is selected, a red warning (“No track selected”) is shown and the map is not updated.

**"% of points included in KUD"**: defines the utilization distribution contour level to display and export, for example 95 for a 95% KUD.

**"Bandwidth selection (hest)"**: choose the smoothing parameter used for kernel density estimation. Available options are href, LSCV, and custom. If custom is selected, a numeric bandwidth value must be entered manually.

**"Custom bandwidth value"**: shown only when custom is selected under bandwidth selection. This value is used as the kernel smoothing parameter.

**"Grid resolution (res)"**: controls the raster resolution used for KUD estimation. Higher values produce finer output but may increase computation time.

**"Extent (space around data area)"**: controls how much space around the observed locations is included in the KUD calculation.

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

**Big data:** If the input data set exceeds 200,000 locations the Shiny UI does not perform properly. Please thin your data for visualisation with this App or use another App to visualize your data.   
**Track Selection** If no track is selected, a red warning (“No track selected”) is shown and the map is not updated.  
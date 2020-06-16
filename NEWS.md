# edbuildmapr 0.2.0

* added new function `<state_shapepull()>` to import state shapefile that matches school district borders
* added parameter to `<sd_map()>` to map a county instead of a state
* corrected legend display size in `<sd_map()>` and `<sd_neighbor_map()>`
* updated all functions with 2018 data

# edbuildmapr 0.1.1

* Changed import of `st_make_valid` from `lwgeom` to `sf` as object `st_make_valid` is exported by `sf` version 0.9.1.

# edbuildmapr 0.1.0

* Initial release of `edbuildmapr` to automate the downloading and analysis of school district spatial data. 

# ASGA 3D Viewer
<p align="center">
<img src="https://github.com/RRobert92/ASGA_3DViewer/blob/main/img/ASGA_3D_logo.png" width="20%" height="20%">
</p>

![GitHub release (latest by date)](https://img.shields.io/github/v/release/RRobert92/ASGA_3DViewer)
![GitHub top language](https://img.shields.io/github/languages/top/RRobert92/ASGA_3DViewer)
![GitHub](https://img.shields.io/github/license/RRobert92/ASGA_3DViewer)
![GitHub Release Date](https://img.shields.io/github/release-date/RRobert92/ASGA_3DViewer)
![GitHub contributors](https://img.shields.io/github/contributors/RRobert92/ASGA_3DViewer)
![Status](https://img.shields.io/badge/lifecycle-maturating-blue.svg)
[![CodeFactor](https://www.codefactor.io/repository/github/rrobert92/ASGA_3DViewer/badge)](https://www.codefactor.io/repository/github/rrobert92/ASGA_3DViewer)

**Open-Source 3D visualization platform** is an open-source ASGA module for publishing 3D data of microtubules in mitotic spindles. Key futures:
  
1. Online access

2. Simple and intuitive to use with implemented UI.

3. Recquired only submiting of your data in special folder without additional coding.

# Table of Contents

* [Quick start](#Quick_start)
  * [Dependency](#Dependency)
  * [Usage](#Quick_start)
* [Contributing](#Contributing)
* [Copyright](#Copyright)
  
<a name="Quick_start"></a>
# Quick startS
You can run the software in an online-version under the [Shinyapp.io](https://kiewisz.shinyapps.io/ASGA_3DViewer/).

<a name="Dependency"></a>
### Dependency for local use
```
R v3.5.3 or newer
Rstudio v1.2 or newer
Java SE 11 (LTS)

R library
- shiny
- shinycssloaders
- shinyWidgets
- shinyBS
- shinyalert
- colourpicker
- readxl
- tidyverse
- rgl
```

<a name="Quick_start"></a>

To use 3D viewer simply refere to [Shinyapp.io](https://kiewisz.shinyapps.io/ASGA_3DViewer/).

To add and publish new data:
```
app
├── bin                                          # Folder containing all functions needed by ASGA_3DViewer
├── wwww                                         # Folder containing all html and css code
├── demo                                         # This name will be display on the website page as a name of publication
│   └── Data_Points_1.RDS
    └── Data_Segments_1.RDS
├── Data
│   └── Publication_Name_1                       # This name will be display on the website page as a name of publication
│       ├── Raw
│       │   ├── DataName_Number_Points.RDS       # This DataName will be display on the website page as a name of dataset
│       │   ├── DataName_Number_Nodes.RDS        # "Number" indicate the order in which data will be display
│       │   ├── DataName_Number_Segments.RDS     # e.g. Hela_#1_Points.RDS; Hela_#1_Nodes.RDS; Hela_#1_Segments.RDS etc.
│       └── Analysis
│       │   ├── Data_Number_AnalysisName.xlsx    # Analysis datasets connected to DataName_Number in Raw folder obtained from ASGA
│       |   ├── ...                              # "Number" is corelated with number of data in Raw folder
│       |   ├── ...                              # e.g. Data_1_LD.xlsx; Data_1_SMT_Ends.xlsx etc.
│   └── Publication_Name_2
│       ├── Raw
│       |   ├── ...
│       └── Analysis 
│       |   ├── ...
│   └── Publication_Name_3
│       ├── ...
├── globa.R
├── ui.R
├── server.R
```

<a href="https://sourcerer.io/rrobert92"><img src="https://avatars0.githubusercontent.com/u/56911280?v=4" height="50px" width="50px" alt=""/></a>

<a name="Copyright"></a>
## Copyright
This project is distributed under the General Public License (GPL) version 3.0 - see the [LICENSE.md](LICENSE.md) file for details.

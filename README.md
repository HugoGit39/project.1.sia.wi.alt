[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<p align="center">
  <img src="https://stress-in-action.nl/wp-content/uploads/2023/04/sia-logo.png" alt="SIA Logo">
</p>

## About

<p align="justify">
  Stress in Action capitalizes on the fast advances in technology and big data analytics to move stress research from the lab to daily life. The Consortium enables synergistic collaborations to discover 1) how responses to daily life stress arise from the temporal, dynamic interplay between context and person-specific factors, 2) how daily life stress can be reliably measured in a specific individual in real-time, and 3) how and when potential beneficial stress-response mechanisms turn into detrimental effects on mental and cardiometabolic health. This enables the development of novel monitoring and intervention strategies to track and reduce daily life stress and its health impact.
</p>

<p align="center">
  <img src="https://stress-in-action.nl/wp-content/uploads/2023/05/Measurements_3.jpg" alt="Measurements Image" width="50%">
</p>

<p align="justify">
  For research theme (RT) 2:  how daily life stress can be reliably measured in a specific individual in real-time using daily life stress assessment tools, selection and validation pipelines in our design field labs will establish their ethical and legal feasibility, user-acceptance, reliability and validity, and potential application in just-in-time adaptive stress interventions.
</p>

## How to use the SiA Wearable Database Shiny App

<strong>Product Filter</strong>

<p align="justify"> The <strong>Product Filter</strong> lets you compare up to three wearables side by side. Each product is selected in two steps: first choose a <em>Manufacturer</em>, then pick a <em>Model</em> from the available options for that brand. </p> <p align="justify"> Once at least one product is selected, a comparison table is generated that transposes features into rows and selected products into columns. This makes it easy to directly compare specs, usability, and validation metrics. </p> <p align="justify"> The third product slot can be cleared using the <code>Reset Product 3</code> button. The filtered comparison can also be downloaded via the <code>Download Filtered Products</code> button. </p> <ul> <li>Product 1: <ul> <li>Manufacturer (dropdown)</li> <li>Model (dropdown)</li> </ul> </li> <li>Product 2: <ul> <li>Manufacturer (dropdown)</li> <li>Model (dropdown)</li> </ul> </li> <li>Product 3 (optional): <ul> <li>Manufacturer (dropdown)</li> <li>Model (dropdown)</li> </ul> </li> <li>Reset Product 3 (button)</li> <li>Download Filtered Products (button)</li> </ul>

<strong>Feature Filter</strong>

<p align="justify"> The <strong>Feature Filter</strong> is a powerful, modular interface that allows filtering wearables on 44 different variables. These filters are grouped into six well-defined categories. The app dynamically updates all options to show only compatible selections. You can download both the filtered results and your filter settings. </p>
1. SiA Expert Score (3 filters)
<ul> <li>Long-Term Expert Score (slider: 0–10)</li> <li>Short-Term Expert Score (slider: 0–10)</li> <li>Exclude missing SiA scores (YES/NO switch)</li> </ul>
2. General Device Information (10 filters)
<ul> <li>Manufacturer (multi-select dropdown)</li> <li>Model (multi-select dropdown)</li> <li>Release Year Range (date range slider)</li> <li>Market Status (multi-select dropdown)</li> <li>Main Use (multi-select dropdown)</li> <li>Device Cost (€) (slider)</li> <li>Type (multi-select dropdown)</li> <li>Location (multi-select dropdown)</li> <li>Weight (g) (slider)</li> <li>Size (multi-select dropdown)</li> </ul>
3. Technical Specifications (6 filters)
<ul> <li>Water Resistant (checkbox)</li> <li>Battery Life (min) (slider)</li> <li>Charging Method (multi-select dropdown)</li> <li>Charging Duration (min) (slider)</li> <li>Bio Cueing (checkbox)</li> <li>Bio Feedback (checkbox)</li> </ul>
4. Signals Measured (13 filters)
<ul> <li>Photoplethysmogram (PPG) (checkbox)</li> <li>Electrocardiogram (ECG) (checkbox)</li> <li>Impedance Cardiography (ICG) (checkbox)</li> <li>Electromyography (EMG) (checkbox)</li> <li>Respiration (checkbox)</li> <li>Electrodermal Activity (EDA) (checkbox)</li> <li>Electroencephalography (EEG) (checkbox)</li> <li>Blood Pressure (BP) (checkbox)</li> <li>Accelerometer (checkbox)</li> <li>Gyroscope (checkbox)</li> <li>GPS (checkbox)</li> <li>Skin Temperature (checkbox)</li> <li>Other Signals (multi-select dropdown)</li> </ul>
5. Data Access (9 filters)
<ul> <li>Raw Data Available (checkbox)</li> <li>Data Transmission Method (multi-select dropdown)</li> <li>Internal Storage (checkbox)</li> <li>Server Storage (checkbox)</li> <li>Storage Capacity (MB) (slider)</li> <li>Storage Capacity (hrs) (slider)</li> <li>GDPR Compliant (checkbox)</li> <li>FDA Approved (checkbox)</li> <li>CE Label (checkbox)</li> </ul>
6. Validation, Reliability & Usability (3 filters)
<ul> <li>Validation Level (multi-select dropdown)</li> <li>Number of Validation/Reliability Studies (slider)</li> <li>Number of Usability Studies (slider)</li> </ul>
<p align="justify"> Every filter selection instantly updates the available options in other dropdowns and sliders to reflect valid combinations. This guarantees a coherent, data-consistent exploration experience. Filtered tables and filter settings can be downloaded using dedicated buttons in the interface. </p>

## Live version

<p align="justify"> 
	<strong>Live Wearable Shiny App website:</strong> <a href="https://wearables.stress-in-action.nl/" target="_blank">	https://wearables.stress-in-action.nl/</a> 
</p>

## Run the app

<p align="justify"> The SiA Wearable Database Shiny App can be run in two environments: </p> <ul> <li> <strong>Test: Local Environment</strong> <br> This version can be run directly on your local machine using R. It includes the full user interface, filtering logic, and data display features for development and testing purposes. </li> <br> <li> <strong>Production: Shiny Server Environment</strong> <br> This version is deployed on a Shiny Server and is intended for official use. It includes access to the full wearable validation pipeline and is optimized for multi-user interaction. </li> </ul> <p align="justify"> <strong>Note:</strong> For <em>both</em> the test and production versions, the email functionality is disabled unless the correct SMTP credentials are present in a private <code>.Renviron</code> file. These credentials are <strong>not</strong> included in the public repository, as they contain confidential DISC email server information. </p> <p align="justify"> To run the app locally for testing purposes, use the following R code: </p>

```r
shiny::runApp("path/to/your/app")`
```

## Support

<p align="justify">
  If you encounter a bug, please <a href="https://github.com/HugoGit39/project.1.sia.wi/issues">submit an issue</a>.
</p>

<p align="justify">
  If you run into any problems, feel free to get in touch with the DISC via <a href="mailto:disc@stress-in-action.nl">disc@stressin-action.nl</a>.
</p>

<p align="justify">
  For detailed questions about the wearable validation pipeline database, please get in touch with a member of RT2</a>.
</p>

## Authors and Citation

<p align="justify"> This application is based on the following publication: <br> <strong> Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. <em>Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information.</em> Behav Res Methods. 2025 May 13;57(6):171. </strong> <br> <a href="https://link.springer.com/article/10.3758/s13428-025-02685-4" target="_blank"> https://link.springer.com/article/10.3758/s13428-025-02685-4 </a> </p> <p align="justify"> Please cite this paper when publishing or sharing any analysis that uses outputs from the filters in this app. </p>

- <a href="https://stress-in-action.nl/artemis-stefani/">A. Stefani</a> - Author RT2
- <a href="https://stress-in-action.nl/myrte-schoenmakers/">M.A. Schoenmakers</a> - Author RT2
- <a href="https://stress-in-action.nl/melisa-saygin/">M. Saygin</a> - Author RT2
- <a href="https://stress-in-action.nl/magdalena-sikora/">M. Sikora</a> - Author RT2
- <a href="https://stress-in-action.nl/matthijs-noordzij/">M. Noordzij</a> - Author RT2
- <a href="https://stress-in-action.nl/eco-de-geus/">J.C.N. de Geus</a> - Author RT2

- <a href="https://stress-in-action.nl/hugo-klarenberg/">H. Klarenberg</a> - Author, app maintainer

## Resources

`Stress in Action` website: [https://stress-in-action.nl](https://stress-in-action.nl)

- <a href="https://stress-in-action.nl/terms-of-use/">Stress in Action</a> - Copyright holder 

- {bs4Dash} - [https://github.com/RinteRface/bs4Dash](https://github.com/RinteRface/bs4Dash)  
- {dplyr} - [https://github.com/tidyverse/dplyr](https://github.com/tidyverse/dplyr)  
- {DT} - [https://github.com/rstudio/DT](https://github.com/rstudio/DT)  
- {emayili} - [https://github.com/datawookie/emayili](https://github.com/datawookie/emayili)  
- {fresh} - [https://github.com/dreamRs/fresh](https://github.com/dreamRs/fresh)  
- {here} - [https://github.com/r-lib/here](https://github.com/r-lib/here)  
- {htmltools} - [https://github.com/rstudio/htmltools](https://github.com/rstudio/htmltools)  
- {htmlwidgets} - [https://github.com/ramnathv/htmlwidgets](https://github.com/ramnathv/htmlwidgets)  
- {lubridate} - [https://github.com/tidyverse/lubridate](https://github.com/tidyverse/lubridate)  
- {reactable} - [https://github.com/glin/reactable](https://github.com/glin/reactable)  
- {reactablefmtr} - [https://github.com/kcuilla/reactablefmtr](https://github.com/kcuilla/reactablefmtr)  
- {rlang} - [https://github.com/r-lib/rlang](https://github.com/r-lib/rlang)  
- {scales} - [https://github.com/r-lib/scales](https://github.com/r-lib/scales)  
- {sever} - [https://github.com/JohnCoene/sever](https://github.com/JohnCoene/sever)  
- {shiny} - [https://github.com/rstudio/shiny](https://github.com/rstudio/shiny)  
- {shinycssloaders} - [https://github.com/andrewsali/shinycssloaders](https://github.com/andrewsali/shinycssloaders)  
- {shinyjs} - [https://github.com/daattali/shinyjs](https://github.com/daattali/shinyjs)  
- {shinySearchbar} - [https://github.com/jsdnrs/shiny-searchbar](https://github.com/jsdnrs/shiny-searchbar)  
- {shinyWidgets} - [https://github.com/dreamRs/shinyWidgets](https://github.com/dreamRs/shinyWidgets)









# SPBAND (Scottish Pregnancy, Births and Neonatal Data) dashboard

## Instructions for use

* Run the app by opening app.R and clicking 'Run' in the top right hand corner
* `global.R` contains required packages and is where any data should be read in
* `data` is a folder for storing data to be read in - no data is in the GitHub version so this folder will need to be created and the correct data added to it
* `admin` is a folder required for storing the password credentials - this is not stored in the GitHub version - the "live" dashboard does not have password protection but a PRA version will need it
* `www` contains the app stylesheet and PHS icon images
* Indicator folders contain R scripts for each tab in this app with the content of that tab. This is linked back to the ui in app.R
* `functions` contains R scripts with functions for the app

# Mars Rhino Dashboard - Overview

This repository contains the Shiny app built in the Rhino framework for Thomas Fungenzi at Mars by EcoData.

This ReadMe is a WIP and will be fleshed out prior to full hand-over.

## Table of Contents

The repo is structured as follows:

- **/app/** contains all code and data used by the Shiny app.
- **/app/main.R** is the core app file which builds the global variables and calls modules that add functionality to the app.
- **/app/logic/** contains non-Shiny code, including sub-functions for the farm simulation (sim_functions.R) and the main run function for the simulation (sim_run.R), KPI functions (kpis.R), and functions for handling file importing (file_io.R).
- **/app/view/** contains Shiny modules, self-contained Shiny scripts providing both the UI and Server code for a specific function. These include:
 - **dashboard.R** module for the main dashboard page, which itself calls on shiny modules in the **dashboard_widgets** folder for each individual visualisation on the dashboard.
 - **param_browser.R** module for the config tab.
 - **raw_outputs.R** module for the raw outputs tab.
- **/app/data/** contains the raw data (crop calendars and parameterisation files) used as the default parameter set on app load.

## Setup

Rhino recommends you [install node.js](https://nodejs.org/en/download) for some core functionality, although strictly speaking it's not required.

Be aware that on Windows machines, the Chocolatey stage of the installation (in Powershell) is notorious for providing warnings and then appearing to hang. Just leave it, it can appear frozen for >20 mins but is actually still installing.

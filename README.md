# azmet-cgdd-graphs
R project to create time series graphs of cumulative growing degree days for meteorological stations in the University of Arizona - Arizona Meteorological (AZMET) network.

A variable that helps us understand and predict the timing of plant and insect growth stages is the accumulation of heat during the year, often quantified by the sum of growing degree days, that is, cumulative growing degree days (CGDDs).

To create the below Plotly-based graph within RStudio, run the 'azmet_download_calc_graph_cgdds.R' script. Original use of this script was to generate and update such graphs on a daily basis with a Windows batch file, and export these graphs to a Plotly account from where a now-defunct website would grab and embed (exemplified in 'azmet_export_viz_CGDD_trace.R' script).

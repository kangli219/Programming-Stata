# Programming-Stata
The repository stores all interesting Stata script I write.

1. eventstudy_DIY.do
  - Self-made script to plot event study results.
  - Made for the subprime lending project (data sources: FDIC/NCUA call reports, HMDA).
  - The graph is stored as eventstudy_DIY.png.

2. evenystudy_coefplot.do
  - plot event study results using coefplot command.
  - Made for the highway corruption project (data sources: USAID survey).
  - The graphs are stored as eventstudy_coefplot1.png and eventstudy_coefplot2.png
  
3. regtable.do
  - It generates a regression table.
  - Features of the table: 
    - add indicator $\times$ for inclusion of variables;
    - mlabel and mgroup
    - booktabs command
    - add outcome mean
  - The table looks like regtable.png. Extra latex code can help make it more complete. 
   
4. Things to be added:
  - mlogit

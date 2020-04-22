## Improvements

Areas that I want to fix/improve **[I]** and areas I need help improving **[H]**. The improvements will be discussed by each tab.

- Clustering: 
  - Fix 'Gene Clustering Distance' that causes it to make changes to Kmeans calculations **[I]**.
  - Create Kmeans cluster plot **[I]**.
  - Allow web hosting by making changes related to output file reading. Currently the app creates cluster outputs in the working directory and uses those files to create visualisations. Approach of creating files and reading from working directory does not allow for hosting on shinyapps.io **[H]**.
- Heatmap
  - Heatmaps files are not created for Kmeans clustering so the tab is redudant in this instance **[I]**.
  - Removal of squished sample names on the x-axis. **[H]**

- Clinical Integration
  - Allow visualisation of a single cluster **[I]**.
- Data Summary 
  - Introduce pathway analysis and add button to select n of top pathways to show **[I]**.
  - Include table of top pathways **[I]**.
- Filtered data
  - Insert table of results for search **[I]**.
  - Introduce t-tests **[I]**
  - Introduce slope plots of expression changes **[H]**
- Sidebar 
  - Remove personal notes on sidebar titles.
  - Update tab descriptions.


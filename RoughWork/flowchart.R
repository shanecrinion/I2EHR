library(DiagrammeR)
grViz("digraph flowchart {

      
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = diamond, fontsize = 8]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']

      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3 -> tab4;
      }

      [1]: 'Read records & provide summary statistics'
      [2]: 'Perform predictive analytics from clinical data'
      [3]: 'Integrate gene expression profiles from GEO'
      [4]: 'Provide proof-of-concept for combined analytics'

      ")
create_flowchart <- function(combinedDataset){
  ################################
  ## Data/numbers for flowchart ##
  ################################
  nTraumaRegistry <- nrow(combinedDataset)
  underFifteen <- combinedDataset$pt_age_yrs <= 14 & !is.na(combinedDataset$ofi)
  nUnderFifteen <- sum(underFifteen)
  notScreenedForOFI <- is.na(combinedDataset$ofi)
  nNotScreenedForOFI <- sum(notScreenedForOFI)
  # EXCLUDE
  includedData <- combinedDataset[!(underFifteen | notScreenedForOFI),]
  nIncludedInStudy <- nrow(includedData)
  #INCLUDE
  
  ### Yes and No from mortality conferance
  nYesOFI <- sum(includedData$ofi == "Yes", na.rm=TRUE)
  nNoOFI <- sum(includedData$ofi == "No", na.rm=TRUE)
  DiagrammeR::grViz("
digraph graph2 {
fontname=Helvetica
graph [layout = dot, splines=ortho, nodesep=0.3, fontname = \"helvetica\"]
node [shape=rounded, fontsize = 15, width = 3.3, fontname = \"helvetica\"]
edge [arrowsize = 0.7, arrowhead=vee, labelfontsize=13, fontname = \"helvetica\"]
swetrau [label = 'Trauma Registry (n=@@1)']
excluded [label = <
Excluded (n=@@7)<br ALIGN='LEFT'/>
&#8226; Not screened for OFI (n=@@5)<br ALIGN='LEFT'/>
&#8226; Under the age of 15 (n=@@6)<br ALIGN='LEFT'/>
>]
eligible [label = 'Included in analysis (n=@@2)']
ofi [label = 'OFI (n=@@3)']
noofi [label = 'no OFI (n=@@4)']
'1' [shape = point, width = 0, height = 0]
swetrau -> '1' [arrowhead = none]
'1' -> excluded [minlen=1.35]
'1' -> eligible [arrowhead = none]
eligible -> ofi
eligible -> noofi
  subgraph {
    rank = same; excluded; '1';
  }
}
[1]: nTraumaRegistry  
[2]: nIncludedInStudy
[3]: nYesOFI
[4]: nNoOFI
[5]: nNotScreenedForOFI
[6]: nUnderFifteen
[7]: nNotScreenedForOFI + nUnderFifteen
") %>% DiagrammeRsvg::export_svg() %>% charToRaw() %>% rsvg::rsvg_pdf("ofi-flowchart.pdf")
}
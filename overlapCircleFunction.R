overlapCircle <- function(response,categories = c("Self", "Other")){
  library(VennDiagram)
  grid.newpage()
  scale = (c(1,2,3,4,5,6,7))
  overlap = (c(0,10,20,30,55,65,85))
  po <- round(approx(scale, overlap, xout = response)$y, digits=2) # po = Percentage overlap
  VennDiagram::draw.pairwise.venn(area1 = 100,
                                  area2 = 100,
                                  cross.area = po,
                                  category = categories,
                                  cat.cex = 4,
                                  cex = 2,
                                  cat.pos = c(330, 30),
                                  cat.dist = -.09,
                                  lwd = 10,
                                  ext.pos = 0,
                                  ext.dist = -5,
                                  sep.dist = 0.02,
                                  label.col = c("white","black","white"))  
}

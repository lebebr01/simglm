##Function to extract IDs to use for missing data
IDtoExtract <- function(IDs, n, p){
  mIDs <- matrix(IDs, ncol=500, nrow=n)
  if(nrow(mIDs) == 25){
    if(p == 6){
      list(mIDs[1:8,],mIDs[9:14,],mIDs[15:18,],mIDs[19:21,])
    } else {
      list(mIDs[1:7,],mIDs[8:12,],mIDs[13:16,],mIDs[17:19,],mIDs[20:22,],mIDs[23:24,])
    } 
  } else {
    if(p==6) {
      list(mIDs[1:16,],mIDs[17:27,],mIDs[28:35,],mIDs[36:41,])
    } else {
      list(mIDs[1:14,],mIDs[15:24,],mIDs[25:32,],mIDs[33:38,],mIDs[39:43,],mIDs[44:47,])
    }
  }
}
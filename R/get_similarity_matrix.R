get_similarity_matrix <- function(tree_file_lst) {
    tre_similarity_df <- data.frame()
    
    treedata_lst <- lapply(tree_file_lst, read.iqtree)
    phylo_lst <- lapply(treedata_lst, as.phylo)
    
    count =0
    for (num1 in seq_along(tree_file_lst)) {
        tre1_label <- tree_file_lst[num1]
        tre1_phylo <- phylo_lst[[num1]]
        for (num2 in seq_along(tree_file_lst)) {
            tre2_label <- tree_file_lst[num2]
            tre2_phylo <- phylo_lst[[num2]]
            
            count <- count + 1
            #print(count)
            
            intersection <- intersect(tre1_phylo$tip.label,tre2_phylo$tip.label)
            
            if (length(intersection) >= 2) {
                similarity_value <- MutualClusteringInfo(phylo_lst[[num1]], phylo_lst[[num2]], normalize = TRUE)}
            else {
                similarity_value <- 0
            }
            
            
            row_values <- c(tre1_label, tre2_label, similarity_value)
            row_data <- as.data.frame(t(row_values))
            rownames(row_data) <- NULL
            tre_similarity_df <- rbind(tre_similarity_df, row_data)
        }
    }
    
    colnames(tre_similarity_df) <- c('Tree1', 'Tree2', 'Similarity')
    return(tre_similarity_df)
}
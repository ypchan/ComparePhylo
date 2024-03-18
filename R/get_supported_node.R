get_supported_node - function(tre_file_lst) {
    support_df - data.frame()
    for (num in seq_along(tre_file_lst)){
        treefile - tre_file_lst[[num]]
        values - c(treefile)
        tre - read.iqtree(file = treefile)
        
        tre %% fortify() %% 
            filter(isTip != TRUE) %% 
            count() %% pull() - internal_node_num
        tre %% fortify() %% 
            filter(isTip != TRUE & SH_aLRT = 95 & UFboot = 95) %% 
            count() %% pull()- high_supported_num
        values - c(values, internal_node_num, 
                   high_supported_num, 
                   round(high_supported_numinternal_node_num, 2))
        raw_line - as.data.frame(t(values))
        rownames(raw_line ) - NULL
        support_df - rbind(support_df,raw_line)
        
    }
    colnames(support_df) - c(tree,node_total,high_support, percentage)
    return(support_df)   
}
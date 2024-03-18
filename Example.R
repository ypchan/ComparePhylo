library(ComparePhylo)

#  get parsimony analysis
log_lst <- list.files('09_parsimony/05_iqtree/', pattern = "_iqtree.log")
log_lst <- paste0('09_parsimony/05_iqtree/',log_lst)
parsinomy_df <- get_parsinomy_stat(log_lst)

parsinomy_df %>% 
    mutate(file_name = str_replace(file_name, "^09_parsimony/05_iqtree/", "")) %>%
    mutate(file_name = str_replace(file_name, "_iqtree\\.log$", "")) -> parsinomy_df

parsinomy_df %>% separate(.,file_name, into = c("Barcode", "proportion"), sep = "_") %>%
    arrange(sites) %>% pull(Barcode) %>% unique() ->order_barcode_length

parsinomy_df %>% separate(.,file_name, into = c("Barcode", "proportion"), sep = "_") -> parsinomy_df
parsinomy_df$Barcode <- factor(parsinomy_df$Barcode, levels = order_barcode_length)
parsinomy_df %>% arrange(Barcode, proportion) %>% 
    mutate(xlab=paste(Barcode,proportion,sep = "_")) %>%
    mutate(sites=as.numeric(sites),
           parsinomy_sites=as.numeric(parsinomy_sites))-> parsinomy_df
plot_order <- parsinomy_df$xlab

parsinomy_df_long <- parsinomy_df %>%
    pivot_longer(cols = c(sites, parsinomy_sites), names_to = "variable", values_to = "value")
parsinomy_df_long$xlab <- factor(parsinomy_df_long$xlab, levels = plot_order)

# 绘制连线点图
ggplot(parsinomy_df_long, aes(x = xlab, y = value, group = interaction(Barcode, variable))) +
    geom_line() +  # 为线条指定颜色映射
    geom_point(aes(color = Barcode, shape=variable), size = 2.5) +  # 为点指定颜色映射
    labs(x = NULL, y = "位点数", color = "Variable/Barcode") + 
    scale_color_d3("category20")  + theme_minimal() +  
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1)) 
# support --------------------------------------------------------------------------------

tre_file_lst <- list.files('09_parsimony/05_iqtree/', pattern = "_iqtree.treefile")
tre_file_lst <- paste0('09_parsimony/05_iqtree/',tre_file_lst)

support_nodes_df <- get_supported_node(tre_file_lst)
    
support_nodes_df %>% mutate(
    file_name=str_replace(tree,'^09_parsimony/05_iqtree/',""),
    file_name=str_replace(file_name,"_iqtree.treefile","")) %>%
    select(-tree) -> support_nodes_df

parsinomy_df %>% left_join(support_nodes_df, by = c ("xlab" = "file_name")) -> support_site_df
support_site_df$xlab <- factor(support_site_df$xlab, levels=plot_order)

ggplot(support_site_df, aes(x=xlab, y=as.numeric(percentage) * 100)) +
    geom_line(aes(group = Barcode)) +  # 为线条指定颜色映射
    geom_point(aes(color = Barcode), size = 2.5) +  # 为点指定颜色映射
    labs(x = NULL, y = "高支持率节点占比(%)") + 
    scale_color_d3("category20")  + theme_minimal() +  
    theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) 


    
    write.xlsx(., file = 'D:\\D_BACKUP/PhD_Thesis/chapter3/chapter2_barcode_parsimony.xlsx')

    


# concatenated order <- 
# topology similarity
getwd()
tre_file_lst <- list.files('08_barcode/10_randam_concatenation/05_barcode_combinations/', 
                           pattern = "_iqtree.treefile")
tre_file__path_lst <- paste0('08_barcode/10_randam_concatenation/05_barcode_combinations/',tre_file_lst)

similarity_df <- get_similarity_matrix(tre_file__path_lst)
similarity_df %>%
    mutate(Tree1 = str_replace(Tree1, ".concatenation_iqtree.treefile", ""),
           Tree1 = str_replace(Tree1, ".*random", "Combination"),.keep='all', 
           Tree2 = str_replace(Tree2, ".concatenation_iqtree.treefile", ""),
           Tree2 = str_replace(Tree2, ".*random", "Combination"),
           Tree1 = str_replace(Tree1, ".concatenation_d1_iqtree.treefile", "_r1"),
           Tree1 = str_replace(Tree1, ".concatenation_d2_iqtree.treefile", "_r2"),
           Tree2 = str_replace(Tree2, ".concatenation_d1_iqtree.treefile", "_r1"),
           Tree2 = str_replace(Tree2, ".concatenation_d2_iqtree.treefile", "_r2"),
           Tree1=str_remove(Tree1,"08_barcode/10_randam_concatenation/05_barcode_combinations/"),
           Tree2=str_remove(Tree2,"08_barcode/10_randam_concatenation/05_barcode_combinations/"),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", ""),
           Tree2 = str_replace(Tree2, "_iqtree.treefile", ""),
           ) %>%
    spread(key = Tree2, value = Similarity) %>% column_to_rownames(.,"Tree1") %>% 
    mutate(across(everything(), as.numeric)) %>% as.matrix() -> wide_df


# 创建显示数字的矩阵
display_numbers <- matrix(ifelse(wide_df >= 0.95, "+++",""), 
                          nrow = nrow(wide_df))

# 使用pheatmap绘制，关闭行列聚类，调整列名角度
pheatmap(wide_df,
         display_numbers =  matrix(ifelse(wide_df >= 0.95, "+++", 
                                          ifelse(wide_df >= 0.90, "++", 
                                                 ifelse(wide_df >= 0.5, "+", "-"))), 
                                   nrow = nrow(wide_df)),
         cluster_cols = FALSE, 
         cluster_rows = F,
         angle_col = 90,
         na_col = "white") # 设置NA值的颜色为白色，使其不可见

random_combinations_support_df <- get_supported_node(tre_file__path_lst)
random_combinations_support_df %>% 
    mutate(tree = str_remove(tree,"08_barcode/10_randam_concatenation/05_barcode_combinations/random"),
           tree = str_replace(tree,"_d2_iqtree.treefile","_r2"),
           tree = str_replace(tree,"_d1_iqtree.treefile","_r1"),
           tree = str_replace(tree,"_iqtree.treefile",""),
           tree = str_replace(tree,".concatenation",""),
           tree = str_replace(tree,"^","C"),) %>% 
    mutate(xlab=tree) %>%
    separate(tree, into=c("order", "duplicate"), sep = "_") %>%
    ggplot(.) + geom_col(aes(x=xlab,y=as.numeric(percentage)* 100, fill=order)) + 
    scale_fill_d3() + theme_minimal() +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1)) 
    
   

write.xlsx(random_combinations_support_df,file="../../../chapter3_random_barcode_combinations.xlsx")    

# genome_barcode_statistics

tre_file_lst <- list.files('08_barcode/08_single_genome_barcode_ml/04_modelfinder_iqtree/', 
                           pattern = "iqtree.treefile")
print(tre_file_lst)
tre_file_lst <- paste0("08_barcode/08_single_genome_barcode_ml/04_modelfinder_iqtree/",tre_file_lst)
tre_file_lst
similarity_df <- get_similarity_matrix(tre_file_lst)
similarity_df %>%
    mutate(Tree1 = str_remove(Tree1, "08_barcode/08_single_genome_barcode_ml/04_modelfinder_iqtree/"),.keep='all', 
           Tree2 = str_remove(Tree2, "08_barcode/08_single_genome_barcode_ml/04_modelfinder_iqtree/"),
           Tree2 = str_replace(Tree2, "random", "Combination"),
           Tree2 = str_replace(Tree2, "_iqtree.treefile", ""),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", ""),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", "_r1"),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", "_r2"),
           Tree2 = str_replace(Tree2, ".concatenation_d1_iqtree.treefile", "_r1"),
           Tree2 = str_replace(Tree2, ".concatenation_d2_iqtree.treefile", "_r2")) %>%
    spread(key = Tree2, value = Similarity) %>% column_to_rownames(.,"Tree1") %>% 
    mutate(across(everything(), as.numeric)) %>% as.matrix() -> wide_df
wide_df %>% as.data.frame() %>% arrange(desc(Genome)) %>% rownames_to_column("barcode") %>%
    mutate(out=paste0(barcode, "(",round(Genome,2),")")) %>% pull(out)
pheatmap(wide_df,
         display_numbers = matrix(ifelse(wide_df >= 0.95, "+++", 
                                         ifelse(wide_df >= 0.90, "++", 
                                                ifelse(wide_df >= 0.5, "+", "-"))), 
                                  nrow = nrow(wide_df)),
         cluster_cols = F, angle_col = 90,
         cluster_rows = F)

single_barcode_support_df <- get_supported_node(tre_file_lst) 
write.xlsx(wide_df,"chapter3_single_barcode_with_genome_compare.xlsx")
write.xlsx(single_barcode_support_df,"chapter3_single_barcode_support_values.xlsx") 

## common used barcode combinations
tre_file_lst <- list.files('08_barcode/09_barcode_combinations/05_barcode_combinations/', 
                           pattern = "_iqtree.treefile")
print(tre_file_lst)
tre_file_path_lst <- paste0('08_barcode/09_barcode_combinations/05_barcode_combinations/',tre_file_lst)

similarity_df <- get_similarity_matrix(tre_file_path_lst)
similarity_df %>%
    mutate(Tree1 = str_replace(Tree1, ".*e_combinations/", ""),
           Tree1 = str_replace(Tree1, "random", "Combination"),.keep='all', 
           Tree2 = str_replace(Tree2, ".*e_combinations/", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree1 = str_replace(Tree1, ".concatenation", ""),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", ""),
           Tree2 = str_replace(Tree2, "_iqtree.treefile", "")) %>%
    spread(key = Tree2, value = Similarity) %>% column_to_rownames(.,"Tree1") %>% 
    mutate(across(everything(), as.numeric)) %>% as.matrix() -> wide_df








pheatmap(wide_df,
         display_numbers = matrix(ifelse(wide_df >= 0.9, "+++", 
                                         ifelse(wide_df >= 0.75, "++", 
                                                ifelse(wide_df >= 0.5, "+", "-"))), 
                                  nrow = nrow(wide_df)),
         cluster_cols = TRUE, angle_col = 90,
         cluster_rows = TRUE)

common_combinations_support_df <- get_supported_node(tre_file_path_lst)
common_combinations_support_df %>% 
    mutate(tree = str_remove(tree,"08_barcode/09_barcode_combinations/05_barcode_combinations/"),
           tree = str_replace(tree,"_iqtree.treefile",""),
           tree = str_replace(tree,".concatenation",""),
           tree = str_replace(tree,"^s","S"),) %>% 
    mutate(xlab=tree) %>% arrange(desc(percentage)) -> common_combinations_support_df



common_combinations_support_df$xlab <- factor(common_combinations_support_df$xlab, levels=common_combinations_support_df$xlab)

common_combinations_support_df %>%ggplot(.) + geom_col(aes(x=xlab,y=as.numeric(percentage)* 100, fill=xlab)) + 
    scale_fill_d3("category20") + theme_minimal() +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(x=xlab,y=as.numeric(percentage)* 100, label=as.numeric(percentage)* 100), size = 2.5)

similarity_df %>%
    mutate(Tree1 = str_replace(Tree1, "08_barcode/09_barcode_combinations/05_barcode_combinations/", ""),
           Tree1 = str_replace(Tree1, "random", "Combination"),.keep='all', 
           Tree2 = str_replace(Tree2, "test_best_combination/find_best_combinations/", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree1 = str_replace(Tree1, ".concatenation", ""),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", ""),
           Tree2 = str_replace(Tree2, "_iqtree.treefile", "")) %>% filter( grepl("Genome", Tree2)) %>% 
    filter(!grepl("Genome",Tree1)) %>% select(Tree1, Similarity)-> common_combination_df
common_combination_df %>% mutate(xlab=Tree1) %>% separate(Tree1, into=c('a',"order"), sep = "set") %>%
    mutate(order=as.numeric(order)) %>% arrange(order) -> common_combination_df

common_combination_df$xlab <- factor(common_combination_df$xlab, levels = common_combination_df$xlab)
common_combination_df$Similarity <- as.numeric(common_combination_df$Similarity)

common_combination_df %>% ggplot(.,aes(x=xlab, y=Similarity, group=a)) +
    geom_point(color="blue") + geom_line() + theme_minimal() +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1)) 




# find best combinations
getwd()
tre_file_lst <- list.files('test_best_combination/find_best_combinations/', 
                           pattern = "_iqtree.treefile")
print(tre_file_lst)
tre_file_path_lst <- paste0('test_best_combination/find_best_combinations/',tre_file_lst)

similarity_df <- get_similarity_matrix(tre_file_path_lst)
similarity_df %>%
    mutate(Tree1 = str_replace(Tree1, "test_best_combination/find_best_combinations/", ""),
           Tree1 = str_replace(Tree1, "random", "Combination"),.keep='all', 
           Tree2 = str_replace(Tree2, "test_best_combination/find_best_combinations/", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree1 = str_replace(Tree1, ".concatenation", ""),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", ""),
           Tree2 = str_replace(Tree2, "_iqtree.treefile", "")) %>%
    spread(key = Tree2, value = Similarity) %>% column_to_rownames(.,"Tree1") %>% 
    mutate(across(everything(), as.numeric)) %>% as.matrix() -> wide_df

pheatmap(wide_df,
         display_numbers = T,
         cluster_cols = TRUE, angle_col = 90,
         cluster_rows = TRUE)

common_combinations_support_df <- get_supported_node(tre_file_path_lst)
common_combinations_support_df %>% 
    mutate(tree = str_remove(tree,"test_best_combination/find_best_combinations/"),
           tree = str_replace(tree,"_iqtree.treefile",""),
           tree = str_replace(tree,".concatenation",""),
           tree = str_replace(tree,"^s","S"),) %>% 
    mutate(xlab=tree) %>% arrange(desc(percentage)) -> common_combinations_support_df
common_combinations_support_df$xlab <- factor(common_combinations_support_df$xlab, levels=common_combinations_support_df$xlab)

common_combinations_support_df %>%ggplot(.) + geom_col(aes(x=xlab,y=as.numeric(percentage)* 100, fill=xlab)) + 
    scale_fill_d3("category20") + theme_minimal() +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_text(aes(x=xlab,y=as.numeric(percentage)* 100, label=as.numeric(percentage)* 100), size = 2.5)

similarity_df %>%
    mutate(Tree1 = str_replace(Tree1, "test_best_combination/find_best_combinations/", ""),
           Tree1 = str_replace(Tree1, "random", "Combination"),.keep='all', 
           Tree2 = str_replace(Tree2, "test_best_combination/find_best_combinations/", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree2 = str_replace(Tree2, ".concatenation", ""),
           Tree1 = str_replace(Tree1, ".concatenation", ""),
           Tree1 = str_replace(Tree1, "_iqtree.treefile", ""),
           Tree2 = str_replace(Tree2, "_iqtree.treefile", "")) %>% filter(Tree1 == "Genome") %>% 
    filter(Tree2 != "Genome") -> find_best_df
find_best_df
find_best_df$Tree2 <- factor(find_best_df$Tree2, levels = find_best_df$Tree2)
find_best_df$Similarity <- as.numeric(find_best_df$Similarity)
find_best_df %>% ggplot(.,aes(x=Tree2, y=Similarity, group=Tree1)) +
    geom_point() + geom_line() + theme_minimal() +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1)) 

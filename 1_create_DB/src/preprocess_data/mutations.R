source("setpath.R") 
source("../helper/mutation/unify_names_of_mutatoin.R")


input_file = file.path(data_path,"mutation_before_unification.csv")
output_file = file.path(out_data_path,"mutation","mutation_unified.csv")
ref_file = file.path(ref_path,"MGgenelist.csv")

unify_names_of_mutatoin(input_file,output_file, ref_file)

source("../helper/mutation/replace_synonym_with_gene_name.R")
input_file = output_file
output_file = file.path(out_data_path,"mutation","synonym_replaced.csv")
ref_file = file.path(ref_path,"gtf_synonym_list.csv")
# replace_synonym_with_gene_name(input_file,output_file,ref_file)


source("../helper/mutation/sort_out_replicate.R")
input_file = output_file
output_file = file.path(out_data_path,"mutation","sort_replicate.csv")
sort_out_replicate(input_file, output_file)


source("../helper/mutation/sort_out_delete_mutation.R")
input_file = output_file
output_file = file.path(out_data_path,"mutation","sort_delete_mutation.csv")
sort_out_delete_mutation(input_file, output_file)



source("../helper/mutation/mut_one_hot_encode.R")
input_file = output_file
output_file = file.path(out_data_path,"mutation","one_hot.csv")
mut_one_hot_encode(input_file, output_file)
# # Post-processing raw data
# 
# # write filepath here
# data = readRDS("~/Downloads/broken_window_MC.Rds")
# 
# # dataframe of concept accuracy
# concept_accuracy = bind_rows(data$concept_accuracy)
# 
# # function to organie list of sentence selection data
# get_sentence_data <- function(rds_list){
# a = rds_list
# newlist = list()
# for(i in 1:length(a)){
# 
#   newlist[[i]] = expand_grid(
#     concept = i,
#     sentences = unlist(a[[i]])
# 
#     )
# }
# return(bind_rows(newlist))
# }
# 
# # get sentence selection data
# sentence_data = get_sentence_data(data$selected_sentences)
# 
# # save files:
# 
# write.csv(concept_accuracy, "myfilename.csv")
# write.csv(sentence_data, "myfilename2.csv")
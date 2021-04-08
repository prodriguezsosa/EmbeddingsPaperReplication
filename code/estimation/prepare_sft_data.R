
## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

out_file <- "./data/mturk/semantic_fluency_task/input_data/"
task_data <- c("democracy", "freedom", "equality", "justice", "immigration", "abortion", "welfare", "taxes", "republican", "democrat")
training_data <- c("computer")


# screening data
screening_data <- list("food" = c("eat", "meat", "nutrition", "cooking", "bread", "rations"),
                       "drinks" = c("beverages", "sodas", "beer", "carbonated", "juice", "alcoholic"),
                       "forest" = c("woodland", "pine", "wilderness", "trees", "fires", "rainforest"),
                       "sea" = c("ocean", "mediterranean", "coast", "ships", "water", "island"))

# save data
saveRDS(task_data, paste0(out_file, "task_data.rds"))
saveRDS(training_data, paste0(out_file, "training_data.rds"))
saveRDS(screening_data, paste0(out_file, "screening_data.rds"))

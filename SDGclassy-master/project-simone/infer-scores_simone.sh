#!/bin/bash
set -e          #needed to ignore errors

d='/Users/simonemordue/OneDrive - Newcastle University/SDG_Classifier/SDGclassy-master' 	#specify root path
p='project-simone'                  			#project subdirectory
c='cl_base_new'                       	#select classifier name
f='full_texts_test'                      			#select target files

cd "${d}" 
rm "${d}"/"${p}"/"${f}"/.DS_Store || true   #Clean up. We ignore any errors here

#Import target data
mallet/bin/mallet import-dir --input "${d}"/"${p}"/"${f}"  --output "${d}"/"${p}"/workshop/inferring-"${f}".mallet --keep-sequence --remove-stopwords --extra-stopwords "${d}"/workshop/extra-exclude-words_new.txt --keep-sequence-bigrams --gram-sizes 1,2  --use-pipe-from "${d}"/classifier/results/"${c}".mallet

#Apply topic model and get SDG scores
mallet/bin/mallet infer-topics --input "${d}"/"${p}"/workshop/inferring-"${f}".mallet --inferencer "${d}"/classifier/results/"${c}"-inferencer.mallet --output-doc-topics "${d}"/"${p}"/output/scores-"${f}"-"${c}".txt   

echo "Command sequence finished successfully"

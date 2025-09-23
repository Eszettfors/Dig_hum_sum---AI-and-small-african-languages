# Dig_hum_sum---AI-and-small-african-languages

This repository contains the code written to analyze linguistic representation on Huggingface for the Student project "AI and small African languages" as part of the summer school on Digital Humanism in Vienna 2025.

### script description

- get_hugging_face_data.py: extracts language tags from Hugging Face and stores in raw
- processing.R: cleans the data to create the analysis-ready .csv in processed
- descriptive_analysis.R: reproduces the plots used in our paper.

### data description
- hugging_face_language_data.csv: raw data extracted from hugging_face
- hugging_face_models.csv: cleaned analysis-ready csv with each row corresponding to a language, uniquely identified with an ISO639-3 code.


### dependencies
See requirements.txt for python
See renv.lock for R


# Hello JP!
Instructions on how to run this project are inside load.py -> here they are again:
## Instructions to run the project
1) Change input path to your data file (in load.py at the top of the file called INPUT_PATH)
2) cd data 
3) python3 load.py
4) cd ..
5) python3 model/chrisbugs_competition.py
6) python3 model/evaluate_final_model.py {your csv path here}

#### Example call for step 6:
python3 evaluate_final_model.py /Users/chrisbugs/summer-lab/2025/labs/data/19_spotify-train.csv.gz
### Why python3?
I use python3 due to the interpreter I have set up, you can use python in place of python3 if you have that set up
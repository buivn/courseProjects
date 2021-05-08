Homework 1 - CS584
Author: Hoang Dung Bui

There are four functions and one main program in the code.
1. remove_words(paragraph, word_list) function: remove the neutral words from the review which does not affect to review's attitude.

2. create_label_and_wordlist(file) function: processes the train data file, which separates labels, splits the review string into words and save it to a list. This list and label list are then processed in the main program.

3. create_wordlist(file) function: processes the test data file. It splits the test review strings into words and saves them into a list. This list will be processed in the main program.

4. similarity_function(words_label, words_test) function: calculate thes distance or similarity between two reviews from train data and test data, respectively. The similarity is calculated by consine approach.

In the main program, it reads the train and test data files, deploys function 2 and 3 to splits the reviews into words lists. There are two loops which calculates the similarity value betwen two reviews (from train data and test data). If the value lies in top k_nn similarity value set, the train review will substitute the review with lowest similarity value in the set. At the end, the top k_nn similarity values will be used to evaluate the test review by majority vote. The voting result will be saved on the out_save.date file.

There is a parameter which needs to change, that is the k_nn. It should be set to 1, 3, 5, or 7 to run the program.

The program run by python3, and requires some packages to run such as re, math and collections/Counter

# homework 1 - CS584 - Introduction to Data Mining - GMU
# author: Hoang-Dung Bui, G01301478

import re
from collections import Counter
import math

def remove_words(paragraph, word_list):
    para = paragraph
    for x in word_list:
        para = para.replace(x, " ")
    return para

def create_label_and_wordlist(file):
    label_set = []
    review_word_set = []
    for line in file:
        p = line.lower() 
        # i += 1
        label = p[0:2]
        word_list = [" are ", " is ", " that ", " the ", " be ", " this ", " for ", " to ", " or ",
        " the ", "\"", ",", ".", " of ", " at ", " and ", " a ", " an ", " i ", " was ", " were ", 
        "!", "(", ")", " our ", " it ", " we ", " in ", " on ", "it's ", " you ", " we'll ", " i'm ",
         "+1", "-1"]
        p1 = remove_words(p, word_list)
        words = p1.split()
        review_word_set.append(words)
        label_set.append(label)
    return label_set, review_word_set

# create a word list of a testing review
def create_wordlist(file):
    test_word_set = []
    i = 0
    for line in file:
        p = line.lower() 
        word_list = [" are ", " is ", " that ", " the ", " be ", " this ", " for ", " to ", " or ",
        " the ", "\"", ",", ".", " of ", " at ", " and ", " a ", " an ", " i ", " was ", " were ", 
        "!", "(", ")", " our ", " it ", " we ", " in ", " on ", "it's ", " you ", " we'll ", " i'm "]
        p1 = remove_words(p, word_list)
        words = p1.split()
        test_word_set.append(words)
        i += 1
    return test_word_set

# determine the distance between two reviews
def similarity_function(words_label, words_test):
    # intersection = 0
    dict2 = Counter(words_test)
    dict1 = Counter(words_label)
    dict1Set = set(dict1)
    dict2Set = set(dict2)
    dot_product = 0.0   
    for word in dict1Set.intersection(dict2Set):
        dot_product += float(dict2[word])*float(dict1[word])
        # print(word, dict2[word], dict1[word])
    mag_dict1 = 0.0
    mag_dict2 = 0.0
    for i in dict1.keys():
        # print(list(dict1.keys()).index(i))
        mag_dict1 += float(dict1[i])*float(dict1[i]) 
    mag_dict1 = math.sqrt(mag_dict1)
    # print("magnitude of dict1: ", mag_dict1)
    for j in dict2.keys():
        mag_dict2 += float(dict2[j])*float(dict2[j]) 
    mag_dict2 = math.sqrt(mag_dict2)
    if (mag_dict1 == 0.0) or (mag_dict2 == 0):
        return 0.0
    else:
        return dot_product/(mag_dict1*mag_dict2)

if __name__ == '__main__':
    train_file = open('1611766549_6456213_train_file.dat', 'r')
    test_file = open('1611766549_7170458_test.dat', 'r')
    label_set, word_list_set = create_label_and_wordlist(train_file)
    test_word_list_set = create_wordlist(test_file) 

    k_nn = 3
    result = []  
    outfile = open('out_save.dat', 'w')   
    for word_test in test_word_list_set:
        # reset for a new review
        i = 0
        n_label_set = []
        similarity_set = []
        similarity_min = 0
        min_index = 0
        for word_list in word_list_set:
            # determine the distance between two reviews
            similarity = similarity_function(word_list, word_test)
            if len(similarity_set) >= k_nn:
                # if find a similarity larger than one of the similarity in neighbor set
                if similarity > similarity_min:
                    similarity_min = similarity
                    similarity_set[min_index] = similarity
                    n_label_set[min_index] = label_set[i]
                    # find the current smallest similarity and get its index
                    for k in range(len(similarity_set)):
                        if similarity_min > similarity_set[k]:
                            similarity_min = similarity_set[k]
                            min_index = k
            else:   # if the neighbor set is not full
                similarity_set.append(similarity)
                n_label_set.append(label_set[i])
                if i==0:
                    similarity_min = similarity
                    min_index = 0
                else:
                    if similarity_min > similarity:
                        similarity_min = similarity
                        min_index = i
            # move to the next review
            i += 1
        # evaluate the current review based on its neighbors
        positive = 0
        negative = 0
        for lab in n_label_set:
            if lab == '+1':
                positive += 1
            else:
                negative += 1
        if positive > negative:
            outfile.write('+1\n')
        else:
            outfile.write('-1\n')
    outfile.close()
import csv
import math
import numpy as np
import os
from random import shuffle
import re
import subprocess
import scipy.sparse.linalg
from sklearn.datasets import load_svmlight_file, dump_svmlight_file
import tempfile
import texttable
import time

START = time.time()
def gettime():
    now = time.time() - START
    return format(now, '.2f') + "s"

w_out = open('../data/results/w.out','w')
w_out_sorted = open('../data/results/w_sorted.out','w')
w_out_anotated = open('../data/results/w_anotated.out','w')
w_writer = csv.writer(w_out_anotated)
complete_output = open('../data/results/output.csv', 'w')
output_writer = csv.writer(complete_output)
# TODO better name for class
class Instance():
    def __init__(self, train_dat_num, val_dat_num, test_dat_num, c=None,j=None, t=None, b=None, half=False, binary=False):
        self.train_data_num = train_dat_num
        self.test_data_num = test_dat_num
        self.val_dat_num = val_dat_num
        self.c = c
        self.j = j
        self.t = t
        self.b = b
        in_str = '_fiftyfifty_' if half else ('_bin_' if binary else '_')
        in_folder =  '../data/svm' + in_str + 'norm/dat_'
        self.train_file = in_folder + str(train_dat_num) +'.txt'
        self.val_file = in_folder + str(train_dat_num) +'.txt'
        self.test_file = in_folder + str(test_dat_num) +'.txt'
        out_folder =  '../data/svm'+ in_str +'out/dat_'
        prefix = out_folder  + str(train_dat_num)
        if c != None:
            prefix += '_c_' + str(c).replace('.','-')
        if j != None:
            prefix += '_j_' + str(j).replace('.','-')
        if t != None:
            prefix += '_t_' + str(t).replace('.','-')
        if b != None:
            prefix += '_b_' + str(b).replace('.','-')
        self.model = prefix + '.model'
        self.prediction = prefix + '.prediction'
        self.classify_out = prefix +'_classify.out'
        self.kernel = None
        self.accuracy = None
        self.fp = None
        self.np = None
        self.tp = None
        self.f1 = None
        self.f_half = None
        self.precision = None
        self.recall = None
        self.w = None
        self.sorted_w = None

class Kernel():
    def __init__(self, type, pd, pg, ps, pr, pu, hfi, num_t_docs, num_sup_vec_plus_1, threshold):
        self.type = type
        self.pd = pd
        self.pg = pg
        self.ps = ps
        self.pr = pr
        self.pu = pu
        self.hfi = hfi
        self.num_t_docs = num_t_docs
        self.num_sup_vec_plus_1 = int(num_sup_vec_plus_1)
        self.b = float(threshold)

def learn(instance):
    if os.path.isfile(instance.model):
        print "Skipped training " + instance.model
        return
    args_list = ['./svm_light/svm_learn']
    if instance.c is not None:
        args_list.append('-c')
        args_list.append(str(instance.c))
    if instance.j is not None:
        args_list.append('-j')
        args_list.append(str(instance.j))
    if instance.t is not None:
        args_list.append('-t')
        args_list.append(str(instance.t))
    if instance.b is not None:
        args_list.append('-b')
        args_list.append(str(instance.b))
    args_list.append(instance.train_file)
    args_list.append(instance.model)
    args = tuple(args_list)
    f_tmp = tempfile.TemporaryFile()
    popen = subprocess.Popen(args, stdout=f_tmp)
    popen.wait()
    f_tmp.close()

def classify(instance):
    if os.path.isfile(instance.prediction):
        print "Skipping classifying " + instance.classify_out
        return
    output = open(instance.classify_out, 'w')
    args = ('./svm_light/svm_classify', instance.test_file, instance.model, instance.prediction)
    popen = subprocess.Popen(args, stdout=output)
    popen.wait()
    output.close()

def read_model(instance):
    f = open(instance.model, 'r')
    lines = f.readlines()
    f.close()
    args = [x.split()[0] for x in lines[1:11]]
    instance.kernel = Kernel(*args)
    f_tmp = tempfile.TemporaryFile()
    f_tmp.writelines(lines[11:])
    f_tmp.flush()
    f_tmp.seek(0)
    x_data, y_data = load_svmlight_file(f_tmp)
    f_tmp.close()
    return x_data, y_data


def find_w(instance):
    x_data, y_data = read_model(instance)
    x_data = x_data.transpose()
    x_mat = scipy.sparse.linalg.aslinearoperator(x_data)
    w = x_mat.matmat(y_data)
    instance.w = [i for i in w if i <> 0]
    w_out.write(str(w))
    w_out.flush() #TODO delete this (only for early testing)
    sorted_w = sorted(range(len(w)), key=lambda x : w[x], reverse=True)
    instance.sorted_w = sorted_w
    w_out_sorted.write(str(sorted_w))
    w_out_sorted.flush()#TODO delete this (only for early testing)

def find_results(instance):
    f = open(instance.prediction, 'r')
    _, y_data = load_svmlight_file(instance.test_file)
    predictions = [float(x.split()[0]) for x in f.readlines()]
    instance.fn = sum([1 for (t, p) in zip(y_data, predictions) if t > 0 and p <= 0])
    instance.fp = sum([1 for (t, p) in zip(y_data, predictions) if t <= 0 and p > 0])
    instance.tp = sum([1 for (t, p) in zip(y_data, predictions) if t > 0 and p > 0])
    instance.accuracy = 1 - float(instance.fn + instance.fp)/len(y_data)
    instance.precision = instance.tp / float(instance.tp+instance.fp)  if instance.tp + instance.fp <> 0 else -1#TODO prevent div by 0 errors
    instance.recall = instance.tp / float(instance.tp + instance.fn) if instance.tp + instance.fp <> 0 else -1
    instance.f1 = 2 * instance.precision * instance.recall / float(instance.precision + instance.recall) if instance.precision + instance.recall <> 0 else -1
    instance.f_half = 1.25 * instance.precision * instance.recall / float(.25*instance.precision + instance.recall) if instance.precision + instance.recall <> 0 else -1
    f.close()

def process(instance):
    learn(instance)
    classify(instance)
    find_results(instance)
    find_w(instance)

def run(train_test_pairs, j_vals=[None], c_vals=[None], t_vals=[None], b=None, half=False, binary=False):
#    outfile = '../data/results/output_'+ str(int(math.floor(time.time()))) + '.csv'
#    outfile = '../data/results/output_t_'+ str(t) + '.csv'
#    output = open(outfile, 'wb')
#    writer = csv.writer(output)

#    table = texttable.Texttable()
    header = ['Train File', 'Test File', 'j', 'c', 't', 'b',  'FN ', 'FP', 'Accuracy','Precision','Recall','F1','F.5']
#    table.header(header)
#    writer.writerow(header)
    output_writer.writerow(header)
    print header
    header.append('w')
    header.append('sorted_w')
    w_writer.writerow(header)
    for (train, val, test) in train_test_pairs:
        for t in t_vals:
            for j in j_vals:
                print "j: " + str(j) + "\t" + gettime()
                for c in c_vals:
                    print "c: " + str(c) + "\t" + gettime()
                    instance = Instance(train, val, test, c=c, j=j, t=t, b=b, half=half,binary=binary)
                    process(instance)
                    row= [instance.train_file, instance.test_file, instance.j, instance.c, instance.t, instance.b, instance.fn, instance.fp, instance.accuracy, instance.precision, instance.recall, instance.f1,instance.f_half]
    #                table.add_row(row)
#                    writer.writerow(row)
                    output_writer.writerow(row)
    #                print row
                    row.append(instance.w)
                    row.append(instance.sorted_w)
                    w_writer.writerow(row)
#    output.close()
#    print "---------------------------"
#    print
#    print table.draw()



DAT_NUMS = range(1,11) #TODO zero index the data files

J_VALS = [.6,.7,.8]
C_VALS = [50, 100, 1000]
T_VALS = [1,2]
B_VALS = [0,None]
pairs = [(2,9,10)]


run(pairs,c_vals=C_VALS,j_vals=J_VALS,t_vals=T_VALS, b=0,binary=False)

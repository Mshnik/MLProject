import csv
import os
import subprocess
import scipy.sparse.linalg
from sklearn.datasets import load_svmlight_file
import tempfile
import time

START = time.time()
def gettime():
    now = time.time() - START
    return format(now, '.2f') + "s"

#TODO write this prettier but seriously doesnt matter at all so dont
def get_output(binary, half):
    if half and binary:
        in_str = '_fiftyfifty_bin_'
    elif half:
        in_str = '_fiftyfifty_'
    elif binary:
        in_str = '_bin_'
    else:
        in_str = '_'
    outfile = '../data/results/output' + in_str[:-1] + '.csv'
    return outfile


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
        if half and binary:
            in_str = '_fiftyfifty_bin_'
        elif half:
            in_str = '_fiftyfifty_'
        elif binary:
            in_str = '_bin_'
        else:
            in_str = '_'
        self.outfile = '../data/results/output' + in_str[:-1] + '.csv'
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
        self.prediction = prefix + '_dat_'+ str(test_dat_num) + '.prediction'
        self.classify_out = prefix+ '_dat_'+ str(test_dat_num)  +'_classify.out'
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
    sorted_w = sorted(range(len(w)), key=lambda x : w[x], reverse=True)
    instance.sorted_w = sorted_w

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

def run(train_test_pairs, j_vals=[None], c_vals=[None], t_vals=[None], b=None, half=False, binary=False, ONLY_EXISTING_MODELS=False):
    output = open(get_output(binary,half), 'ab')
    writer = csv.writer(output)
    for (train, val, test) in train_test_pairs:
        print "--------------------" + str(train)
        for t in t_vals:
            for j in j_vals:
                print "j: " + str(j) + "\t" + gettime()
                for c in c_vals:
                    print "c: " + str(c) + "\t" + gettime()
                    instance = Instance(train, val, test, c=c, j=j, t=t, b=b, half=half,binary=binary)
                    if ONLY_EXISTING_MODELS and not os.path.isfile(instance.model):
                        continue
                    process(instance)
                    row = [instance.train_file, instance.test_file, instance.j, instance.c, instance.t, instance.b, instance.fn, instance.fp, instance.accuracy, instance.precision, instance.recall, instance.f1,instance.f_half, instance.w, instance.sorted_w]
                    writer.writerow(row)
    output.close()



DAT_NUMS = range(1,11) #TODO zero index the data files

J_VALS = [.5,.6,.7,.75,.8,9]
C_VALS = [1,10,100,500,1000]
T_VALS = [0,1,2,3]
B_VALS = [0,None]
pairs = [(i,9,j) for i in range(1,4) for j in range(8,10)]


run(pairs,c_vals=C_VALS,j_vals=J_VALS,t_vals=T_VALS, b=0,binary=True, ONLY_EXISTING_MODELS=True)

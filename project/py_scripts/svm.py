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
def get_in_str(binary,ff):
    if ff and binary:
        in_str = '_bin_ff_'
    elif ff:
        in_str = '_ff_'
    elif binary:
        in_str = '_bin_'
    else:
        in_str = '_'
    return in_str

def get_validation(binary, ff, test_ff):
    in_str = get_in_str(binary, ff)
    if ff <> test_ff:
        in_str += "tested_on" + get_in_str(binary,test_ff)
    outfile = '../data/results/validation' + in_str[:-1] + '.csv'
    return outfile

def get_mecnemar_filename(binary, ff):
    in_str = get_in_str(binary, ff)
    outfile = '../data/results/mecnemar_' + in_str[:-1] + '.csv'
    return outfile


# TODO better name for class
class Instance():
    def __init__(self, train_dat_num, val_dat_num, test_dat_num, c=None,j=None, t=None, b=None, ff=False, binary=False, test_ff=None,test_binary=None):
        if test_binary is None:
            self.test_binary = binary
        if test_ff is None:
            self.test_ff = ff
        self.binary = binary
        self.ff = ff
        self.train_dat_num = train_dat_num
        self.test_dat_num = test_dat_num
        self.val_dat_num = val_dat_num
        self.c = c
        self.j = j
        self.t = t
        self.b = b
        in_str = get_in_str(binary, ff)
        test_in_str = get_in_str(self.test_binary,test_ff)
        self.outfile = '../data/results/validation' + in_str[:-1] + '.csv'
        in_folder =  '../data/svm' + in_str + 'norm_data/dat_'
        self.train_file = in_folder + str(self.train_dat_num) +'.txt'
        self.val_file = in_folder + str(self.train_dat_num) +'.txt'
        self.test_file = '../data/svm' + test_in_str + 'norm_data/dat_' + str(self.test_dat_num) +'.txt'
        out_folder =  '../data/svm'+ in_str +'out/dat_'
        prefix = out_folder  + str(self.train_dat_num)
        if c != None:
            prefix += '_c_' + str(c).replace('.','-')
        if j != None:
            prefix += '_j_' + str(j).replace('.','-')
        if t != None:
            prefix += '_t_' + str(t).replace('.','-')
        if b != None:
            prefix += '_b_' + str(b).replace('.','-')
        self.model = prefix + '.model'
        self.prediction = prefix + '_dat_'+ str(self.test_dat_num) + '.prediction'
        self.classify_out = prefix+ '_dat_'+ str(self.test_dat_num)  +'_classify.out'
        self.kernel = None
        self.accuracy = None
        self.fp = None
        self.np = None
        self.tp = None
        self.f1 = None
        self.f_ff = None
        self.precision = None
        self.recall = None
        self.w = None
        self.sorted_w = None

#This is a terrible class and should not exist
class Training_Instance():
    def __init__(self, train_dat_num, model, prediction, c,j, t, b, ff, binary):
        self.train_dat_num = train_dat_num
        self.model = model
        self.prediction = prediction
        self.c = c
        self.j = j
        self.t = t
        self.b = b
        self.ff = ff
        self.binary = binary
        self.accuracy = 0
        self.num_instances = 0

    def get_accuracy(self):
        if self.num_instances == 0:
            return -1
        return self.accuracy/self.num_instances

    def add_value(self, accuracy):
        self.accuracy += accuracy
        self.num_instances += 1

    def __eq__(self, other):
        return self.train_dat_num == other.train_dat_num and self.c == other.c and self.j == other.j and self.t == other.t \
               and self.b == other.b and self.binary == other.binary and self.ff == other.ff

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
#    if os.path.isfile(instance.prediction):
#        print "Skipping classifying " + instance.classify_out
#        return
    validation = open(instance.classify_out, 'w')
    args = ('./svm_light/svm_classify', instance.test_file, instance.model, instance.prediction)
    popen = subprocess.Popen(args, stdout=validation)
    popen.wait()
    validation.close()

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
    instance.sorted_w = [i for i in sorted_w if w[i] <> 0]

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
    instance.f_ff = 1.25 * instance.precision * instance.recall / float(.25*instance.precision + instance.recall) if instance.precision + instance.recall <> 0 else -1
    f.close()

def process(instance):
    learn(instance)
    classify(instance)
    find_results(instance)
    find_w(instance)

def run(train_test_pairs, j_vals=[None], c_vals=[None], t_vals=[None], b=None, ff=False, binary=False, test_ff = None, ONLY_EXISTING_MODELS=False):
    if test_ff is None:
        test_ff = ff
    validation = open(get_validation(binary,ff,test_ff), 'ab')
    writer = csv.writer(validation)
    for (train, val, test) in train_test_pairs:
        print "---------------------------------------------------------------------" + str(train) + "---------------" + str(test)
        for t in t_vals:
            for j in j_vals:
                print "j: " + str(j) + "\t" + gettime()
                for c in c_vals:
                    print "c: " + str(c) + "\t" + gettime()
                    instance = Instance(train, val, test, c=c, j=j, t=t, b=b, ff=ff,binary=binary,test_ff=test_ff)
                    if ONLY_EXISTING_MODELS and not os.path.isfile(instance.model):
                        continue
                    process(instance)
                    row = [instance.train_file, instance.test_file, instance.j, instance.c, instance.t, instance.b, instance.fn, instance.fp, instance.accuracy, instance.precision, instance.recall, instance.f1,instance.f_ff, instance.w, instance.sorted_w]
                    writer.writerow(row)
                    INSTANCE_LIST.append(instance)  
    validation.close()


def test(writer, binary, ff):
    best = max([i for i in INSTANCE_LIST if i.binary == binary and i.ff == ff], key=lambda x:x.accuracy)
    instance = Instance(best.train_dat_num,0,6,c=best.c,j=best.j,t=best.t,b=best.b,ff=ff,binary=binary)
    process(instance)
    row = [instance.binary, instance.ff, instance.train_file, instance.test_file, instance.j, instance.c, instance.t, instance.b, instance.fn, instance.fp, instance.accuracy, instance.precision, instance.recall, instance.f1,instance.f_ff, instance.w, instance.sorted_w]
    writer.writerow(row)

def test_average(writer, binary, ff):
    filtered = [i for i in INSTANCE_LIST if i.binary == binary and i.ff == ff]
    m = {}
    for i in filtered:
        if m.get((i.train_dat_num, i.c,i.j, i.t, i.b, i.ff, i.binary)) is None:
            m[(i.train_dat_num, i.c,i.j, i.t, i.b, i.ff, i.binary)] = Training_Instance(i.train_dat_num,i.model,i.prediction,i.c,i.j,i.t,i.b,i.ff,i.binary)
        else:
            m[(i.train_dat_num, i.c,i.j, i.t, i.b, i.ff, i.binary)].add_value(i.accuracy)
    best = max(m.values(), key=lambda x: x.get_accuracy())
    instance = Instance(best.train_dat_num,0,VALIDATION,c=best.c,j=best.j,t=best.t,b=best.b,ff=ff,binary=binary)
    process(instance)
    row = [instance.binary, instance.ff, instance.train_file, instance.test_file, instance.j, instance.c, instance.t, instance.b, instance.fn, instance.fp, instance.accuracy, instance.precision, instance.recall, instance.f1,instance.f_ff, instance.w, instance.sorted_w, "AVERAGE"]
    writer.writerow(row)
    macnemar_test(best,instance.test_file)

def macnemar_test(instance,test_file):
    f_out = open(get_mecnemar_filename(instance.binary,instance.ff),'wb')
    writer = csv.writer(f_out)
    f = open(instance.prediction, 'r')
    _, y_data = load_svmlight_file(test_file)
    predictions = [float(x.split()[0]) for x in f.readlines()]
    row = ['actual','assigned']
    writer.writerow(row)
    for (t,p) in zip(y_data,predictions):
        row = [t,p]
        writer.writerow(row)
    f_out.close()

INSTANCE_LIST = []
DAT_NUMS = range(1,11) #TODO zero index the data files

results_file = open('../data/results/results.csv','ab')
results_writer = csv.writer(results_file)

J_VALS = [.75]
C_VALS = [1, 10, 50, 100, 200, 1000]
T_VALS = [1,2]
B_VALS = [0,None]
pairs = [(i,9,j) for i in range(1,2) for j in range(8,11)]
VALIDATION = 6


run(pairs,c_vals=C_VALS,j_vals=J_VALS,t_vals=T_VALS, b=0,binary=False, ff= False,ONLY_EXISTING_MODELS=True)
print "**************************************************************** A "
run(pairs,c_vals=C_VALS,j_vals=J_VALS,t_vals=T_VALS, b=0,binary=True, ff=False, ONLY_EXISTING_MODELS=True)
print "**************************************************************** B "
run(pairs,c_vals=C_VALS,j_vals=J_VALS,t_vals=T_VALS, b=0, binary=True, ff=True,  ONLY_EXISTING_MODELS=True)
print "**************************************************************** C "
run(pairs,c_vals=C_VALS,j_vals=J_VALS,t_vals=T_VALS, b=0,ff=True, binary=False,  ONLY_EXISTING_MODELS=True)
print "**************************************************************** D "

test_average(results_writer,True,True)
test_average(results_writer,True,False)
test_average(results_writer,False,True)
test_average(results_writer,False,False)

results_file.close()
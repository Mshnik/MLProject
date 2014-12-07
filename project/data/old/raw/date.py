import csv

def isOne(s):
	if len(s) == 1:
		return "0"+s
	else:
		return s

def fixDate(date):
	dateChunks = date.split("/")
	if len(dateChunks) != 3:
		return date
	year = "20"+dateChunks[2]
	day = isOne(dateChunks[1])
	month = isOne(dateChunks[0])
	return year + "-" + month + "-" + day

def read_file(file):
    with open(file, 'r') as f:
        data = [row for row in csv.reader(f.read().splitlines())]
    return data

data = read_file('comined_1To5_modified2.csv')
output = open('fixedDates.csv', 'wb')
writer = csv.writer(output)
i = 0
for row in data:
	writer.writerow(row[0:33] + [fixDate(row[34])] + row[35:])
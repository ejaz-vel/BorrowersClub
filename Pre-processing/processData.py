f = open('data.csv')
output = open('temp.csv', 'w')

lineNum = 0
defaultCount = 0
for line in iter(f):
	lineNum += 1
	if lineNum == 1:
		output.write(line.strip() + "\n")
		continue
	data = line.strip().split(',')
	loanStatus = data[17].lower()
	if 'paid' not in loanStatus:
		defaultCount += 1
		data[17] = '1'
	else:
		data[17] = '0'
	output.write(','.join(data) + '\n')
f.close()
output.close()
print defaultCount

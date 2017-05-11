f = open('WithCreditScore/lending_club_loans.csv')
outFile = open('WithCreditScore/data.csv', 'w')
curruptedData = open('WithCreditScore/errorData.csv', 'w')
count = 0
for line in iter(f):
	newLine = line.strip().replace('\"', '')
	processedFields = []
	data = newLine.split(',')
	data = map(str.strip, data)
	if len(data) != 115:
		curruptedData.write(newLine + '\n')
		continue
	for field in data:
		field = field.strip()
		if len(field) > 0 and field[len(field) - 1] == '%':
			try:
				num = float(field[:len(field) - 1]) / 100.0
				processedFields.append(str(num))
			except ValueError:
				processedFields.append(field)
		else:
			processedFields.append(field)
	outFile.write(','.join(processedFields) + '\n')
outFile.close()
f.close()
curruptedData.close()
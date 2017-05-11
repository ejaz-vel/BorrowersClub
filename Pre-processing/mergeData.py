output = open("data.csv", "w")
f = open("trainData.csv")
for line in iter(f):
	output.write(line.strip() + "\n")
f.close()
f = open("testData.csv")
for line in iter(f):
	output.write(line.strip() + "\n")
output.close()

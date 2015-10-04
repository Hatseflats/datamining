data = []

with open('2.txt', 'r') as f:
	for line in f:
		parts = line.split(" ")
		parts = map(float, parts)

		data.append(parts)


data = sorted(data, key=lambda x: x[2])

for i in range(10):
	print(data[i])



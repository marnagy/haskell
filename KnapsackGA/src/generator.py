from random import randint

n = int(input("Enter amount of random values: "))

with open("weights.txt", mode='w') as fw:
    with open("values.txt", mode='w') as fv:
        for i in range(n):
            fw.write(str(randint(1,15)) + '\n')
            fv.write(str(randint(1,15)) + '\n')

print("Files with weights and values have been created.")
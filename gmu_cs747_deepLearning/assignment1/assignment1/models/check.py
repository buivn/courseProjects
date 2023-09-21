import numpy as np
import math

a = np.array([[2.0, 2.5], [23.2, 5.6], [15.3, 8.7], [7.1,8.8], [9.5,4.5], [11.2,2.3], [17.4,6.4], [19.2,6.6]])
ad = np.array([1.0, -2.0])
# a[0:2] += ad
# a[5:7] -= ad

# print(a)
# np.random.shuffle(a)
# print(a)
# np.random.shuffle(a)
# print(a)

rand1 =  np.squeeze(np.random.randint(10, size=1))
# print(rand1)
while True:
    rand2 = np.squeeze(np.random.randint(10, size=1))
    if rand2 != rand1:
      break
# print(rand2)

start = 0
end = 2
# print(a[start:end])

start = end
end += 2

# print(a[start:8])
# a1 = 4.3
# print(math.ceil(a1/2))

results = np.empty((10), dtype=int)

# results[5] = 5
# print(results)
# index = np.argmax(results)
# print(index)
# print(a)
# np.where(np.logical_and(a>=6, a<=10))

# b = np.argwhere(a[:,1] > 5)
# b = np.squeeze(b)
# print(b)
# c = a[b, :]
# print(c)

d = np.array([7.1, 9.5, 11.2, 17.4, 19.2, 5.1, 9.5])
d1 = np.array([7.1, 9.5, 11.2, 17.4, 19.2, 5.1, 9.5])
ad = np.array([1.0, -2.0])
# print(d*d1)
# print(np.sum(d*d1))
list1 = np.argwhere(d == 9.5)
list1 = np.squeeze(list1)
print(np.argwhere(d == 9.5))

print(d[list1])

idx = np.random.randint(100, size=5)
# print(idx)

# print(d+5.0)
# d[0:2] += 5.0
# d[3:5] -= 3.0
# print(d)
# print()

# b = np.where(np.logical_and(d>=6.0, d<=11.5))
# b = np.squeeze(b)
# print(d[b])
# b = np.argwhere(d[:] ==9.5)
# c = d(d==9.5)
# print(len(d[b]))
# a= np.concatenate((a,d), axis=0)
# print(a)
# b = np.zeros((len(a), 1), dtype=int)
# print(b)
# for i in range(len(a)):
#     if (a[i,0] >= 0.0) and (a[i,0] < 10.0):
#         b[i,0] = 0
#     if (a[i,0] >= 10.0) and (a[i,0] < 20.0):
#         b[i,0] = 1
#     if (a[i,0] >= 20.0) and (a[i,0] < 30.0):
#         b[i,0] = 2
# print(b)
# a = np.where(a > 10.0 and a < 12.0, 0.0, a)

# a[a>10.0 and a <13.0] = 0.0
# print(a)


# a = np.where(a < 3.0, 3.0, a)
# print(a)
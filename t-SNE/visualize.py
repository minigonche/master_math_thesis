#Script for visualizong the MNIST data base using t-SNE

#Imports the data and converts it to numpy arrays
mndata = MNIST('../python-mnist/data')
X_train, y_train = mndata.load_training()
X_test, y_test = mndata.load_testing()

#converts to numpy array
X_train = np.array(X_train)
X_test = np.array(X_test)
y_train = np.array(y_train)
y_test = np.array(y_test)

#Train size and dimension size
N, D = x_train.shape



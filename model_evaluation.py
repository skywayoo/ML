# -*- coding: utf-8 -*-
"""
Created on Sat Jan  4 22:25:52 2020

@author: skywayoo
"""

import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.datasets import load_iris
from sklearn.model_selection import KFold
from sklearn.metrics import mean_absolute_error

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

data = load_iris()

data.data
data.target[[10, 25, 50]]


X = np.array([[1, 1], [1, 2], [2, 2], [2, 3]])
# y = 1 * x_0 + 2 * x_1 + 3
y = np.dot(X, np.array([1, 2])) + 3
reg = LinearRegression()
reg.fit(X, y)
reg.score(X, y)

reg.coef_

reg.intercept_

reg.predict(np.array([[3, 5]]))


model_evaluation(data.data, data.target).cross_validation(5,reg)

class model


class model_evaluation:
    
    def __init__(self, X, y):
        self.X = X
        self.y = y
    
    def cross_validation(self, K, model):
        
        self.model = model
        kf = KFold(n_splits=K, shuffle=True, random_state=5)
        fold_idx = kf.split(self.X)
        
        score = []
        for train_index, test_index in fold_idx:
            X_train, X_test = self.X[train_index], self.X[test_index]
            y_train, y_test = self.y[train_index], self.y[test_index]
            self.model.fit(X_train,y_train)
            y_pred = self.model.predict(X_test)
            print (y_pred, y_test)
            score.append([mean_absolute_error(y_test, y_pred),mean_absolute_percentage_error(y_test, y_pred)])
        return score
    
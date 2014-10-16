clear all; close all; clc

x=load('ex2x.dat');
y=load('ex2y.dat');

% 因此，还需要在数据集x的坐标统一加上1，相当于x0=1

m = length(y); % store the number of training examples
x = [ones(m, 1), x]; % Add a column of ones to x
% 增加了这一列以后，要特别注意，现在年龄这一变量已经移动到了第二列上。

theta=zeros(1,2)


% 计算迭代一次时的权重向量：
trTheta=theta'
delta=(x*trTheta)-y;
sum=delta'*x;
delta_theta=sum*0.07/m;
theta1=theta - delta_theta








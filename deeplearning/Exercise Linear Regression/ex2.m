clear all; close all; clc

x=load('ex2x.dat');
y=load('ex2y.dat');

% ��ˣ�����Ҫ�����ݼ�x������ͳһ����1���൱��x0=1

m = length(y); % store the number of training examples
x = [ones(m, 1), x]; % Add a column of ones to x
% ��������һ���Ժ�Ҫ�ر�ע�⣬����������һ�����Ѿ��ƶ����˵ڶ����ϡ�

theta=zeros(1,2)


% �������һ��ʱ��Ȩ��������
trTheta=theta'
delta=(x*trTheta)-y;
sum=delta'*x;
delta_theta=sum*0.07/m;
theta1=theta - delta_theta








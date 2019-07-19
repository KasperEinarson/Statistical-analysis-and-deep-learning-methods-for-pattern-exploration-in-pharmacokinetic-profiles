%%Load data

%PK_train = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Train_PK_UP.txt');
PK_train = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_train.txt');
PK_val = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_val.txt');
PK_test = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_test.txt');

PK_TV = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_TV.txt');
    
%BG_train = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Train_BG_UP.txt');
BG_train = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_train.txt');
BG_val = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_val.txt');
BG_test = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_test.txt');

BG_TV = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_TV.txt');

PK_AB = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK.up_AB.txt');
BG_AB = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG.up_AB.txt');
%% Load fake random data:
%PK_train = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_random_train.txt')
%PK_val = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_random_val.txt')
%PK_test = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/PK_random_test.txt')

%BG_train = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_random_train.txt')
%BG_val = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_random_val.txt')
%BG_test = importdata('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/BG_random_test.txt')

%%
%matmat = load('MNIST.mat')
%% Give correct names
X1 = PK_train.data;X2 = BG_train.data; XV1 = PK_val.data;XV2 = BG_val.data;XTe1 = PK_test.data;XTe2 = BG_test.data; 
PK_TV1 = PK_TV.data; BG_TV1 = BG_TV.data; 
PK_AB1 = PK_AB.data;BG_AB1 = BG_AB.data;

%%



%% Hyperparameters for DCCA network architecture.
% output dim
K=3;
% Regularizations for each view.
%rcov1=0.00155; rcov2=0.0154;
batchsize=500;
eta0=0.87;
momentum=0.8;
maxepoch=3000;
rcov1=2.4e-2; 
rcov2=0.4e-3;
l2penalty=7.3e-3;
hiddentype='sigmoid';
%rcov1=0; rcov2=0;
% dropout rate between each layer
drop1 = [0 0 0 0 0];
drop2 = [0 0 0 0 0];
% Hidden activation type.

% Architecture (hidden layer sizes) for view 1 neural network.
NN1=[8 K];
% Architecture (hidden layer sizes)  for view 2 neural network.
NN2=[8 K];
% Weight decay parameter.

%% 126
%% Run DCCA with SGD. No pretraining is used.
% Minibatchsize.
gpuDeviceCount=0;

% Learning rate.

% Rate in which learning rate decays over iterations.
% 1 means constant learning rate.
decay=1;
% Momentum.

% How many passes of the data you run SGD with.

%addpath ./deepnet/
% DCCA train takes training data and split it into mini batches (around 3
% or 4 in order to have data enough in each mini batch to estimate the
% covariance matrix properly). Then mini batch gradient decent is carried
% out for each mini batch (3-4 steps) by minimizing the objective function
% c = -correlation + regularization. 
%% Kasper version
[F1opt,F2opt,CORR_train,CORR_tune,CORR_test]=DCCA_train1(PK_TV1,BG_TV1,XTe1,XTe2,K,hiddentype,NN1,NN2, ...
  rcov1,rcov2,l2penalty,batchsize,eta0,decay,momentum,maxepoch,3,drop1,drop2,PK_AB1,BG_AB1);

%% original
[F1opt,F2opt,CORR_train,CORR_tune,CORR_test]=DCCA_train2(X1,X2,XV1,XV2,XTe1,XTe2,K,hiddentype,NN1,NN2, ...
  rcov1,rcov2,l2penalty,batchsize,eta0,decay,momentum,maxepoch,5,drop1,drop2);

%%
[F1opt,F2opt,CORR_train,CORR_tune,CORR_test,VV_total]=DCCA_train20(X1,X2,XV1,XV2,XTe1,XTe2,K,hiddentype,NN1,NN2, ...
  rcov1,rcov2,l2penalty,batchsize,eta0,decay,momentum,maxepoch,5,drop1,drop2);

%% check train vs tune on epoch
plot(CORR_train)
hold on 
plot(CORR_tune)
%hold on

%plot(CORR_test)
legend('train','validation')


%%
CORR_final_val =DCCA_corr(deepnetfwd(XV1,F1opt),deepnetfwd(XV2,F2opt),K,rcov1,rcov2)
CORR_final_train =DCCA_corr(deepnetfwd(X1,F1opt),deepnetfwd(X2,F2opt),K,rcov1,rcov2)

%%
corr1=DCCA_corr(deepnetfwd(XTe1,F1opt),deepnetfwd(XTe2,F2opt),K,rcov1,rcov2)

%%
X1proj=gather(deepnetfwd(X1,F1opt)); 
XV1proj=gather(deepnetfwd(XV1,F1opt));
XTe1proj=gather(deepnetfwd(XTe1,F1opt));

X2proj=gather(deepnetfwd(X2,F2opt)); 
XV2proj=gather(deepnetfwd(XV2,F2opt));
XTe2proj=gather(deepnetfwd(XTe2,F2opt));


%%
[corr1,grad1,grad2]=DCCA_corr(deepnetfwd(XV1,F1opt),deepnetfwd(XV2,F2opt),K,rcov1,rcov2)
%%
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe1_big.txt',XTe1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XTe2_big.txt',XTe2proj)


%%
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_smallK.txt',X1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV1_smallK.txt',XV1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Xte1_smallK.txt',XTe1proj)%
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_smallK.txt',X2proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV2_smallK.txt',XV2proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Xte2_smallK.txt',XTe2proj)

%% 
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_proj_test.txt',X1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV1_proj_test.txt',XV1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Xte1_proj_test.txt',XTe1proj)%
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_proj_test.txt',X2proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV2_proj_test.txt',XV2proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Xte2_proj_test.txt',XTe2proj)

%%
[corr1,grad1,grad2,xcoef,ycoef,S11,S12,T,K11,D] =DCCA_corr(deepnetfwd(XTe1,F1opt),deepnetfwd(XTe2,F2opt),K,rcov1,rcov2)

%%
%%
corr(X1proj(:,2),X2proj(:,2))
%%
xscores = XTe1(:,1:K)*xcoef;
yscores = XTe2(:,1:K)*ycoef;

corr(xscores(:,1),yscores(:,1))
%%
corrXxscores = corr(XTe1(:,1:K),xscores);
corrYxscores = corr(XTe2(:,1:K),xscores);
corrXyscores = corr(XTe1(:,1:K),yscores);
corrYyscores = corr(XTe2(:,1:K),yscores);
scatter(xscores(:,1),xscores(:,2))

%% loadings
scatter(corrXxscores(:,1),corrXxscores(:,2))
hold on
scatter(corrYxscores(:,1),corrYxscores(:,2))

%% 
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/xscores.txt',xscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/yscores.txt',yscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrXxscores.txt',corrXxscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrYxscores.txt',corrYxscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrXyscores.txt',corrXyscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrYyscores.txt',corrYyscores)

%% 
K = 12;
%good = load('result_K=14_rcov1=0.13104_rcov2=0.069964_l2penalty=0.0001_batchsize=475_eta0=0.082167_decay=1_momentum=0.99_maxepoch=1000.mat')
%good = load('result_K=12_rcov1=0.00155_rcov2=0.0154_l2penalty=0.0001_batchsize=500_eta0=0.01_decay=1_momentum=0.99_maxepoch=2000.mat')
%good = load('D:\Matlab\bin\Good Results\result_K=14_rcov1=0.16928_rcov2=0.00091295_l2penalty=0.00025764_batchsize=404_eta0=0.60795_decay=1_momentum=0.80488_maxepoch=1983.mat')
plot(good.CORR_train)
hold on 
plot(good.CORR_tune)
%hold on

%plot(CORR_test)
legend('train','validation')
%%
[corr1,grad1,grad2,xcoef,ycoef,S11,S12,T,K11] =DCCA_corr(deepnetfwd(XTe1,good.F1opt),deepnetfwd(XTe2,good.F2opt),12,0.00155,0.0154)
K = 14;
xscores = XTe1(:,1:K)*xcoef;
yscores = XTe2(:,1:K)*ycoef;

corrXxscores = corr(XTe1(:,1:K),xscores);
corrYxscores = corr(XTe2(:,1:K),xscores);
corrXyscores = corr(XTe1(:,1:K),yscores);
corrYyscores = corr(XTe2(:,1:K),yscores);
scatter(xscores(:,1),xscores(:,2))

%% loadings
scatter(corrXxscores(:,1),corrXxscores(:,2))
hold on
scatter(corrYxscores(:,1),corrYxscores(:,2))

%% Write 
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/xscores.txt',xscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/yscores.txt',yscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrXxscores.txt',corrXxscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrYxscores.txt',corrYxscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrXyscores.txt',corrXyscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrYyscores.txt',corrYyscores)
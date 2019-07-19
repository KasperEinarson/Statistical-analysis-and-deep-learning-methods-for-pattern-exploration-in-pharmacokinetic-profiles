function optimal =DCCA_train6( ...
  X1,X2,XV1,XV2,XTe1,XTe2,K,NN1,NN2, ...
  randseed,drop1,drop2,decay,batchsize,eta0,momentum,maxepoch,rcov1,rcov2,l2penalty)
%batchsize=XX(1);
%eta0 = XX(2);
%momentum = XX(3)
%maxepoch= XX(4);
%rcov1=XX(5);
%rcov2 =XX(6);
%l2penalty = XX(7);
%hiddentype = XX(8);
% [F1opt,F2opt,CORR_train,CORR_tune,CORR_test]=DCCA_train( ...
%   X1,X2,XV1,XV2,XTe1,XTe2,K,hiddentype,NN1,NN2,rcov1,rcov2,l2penalty, ...
%   batchsize,eta0,decay,momentum,maxepoch,randseed) trains the DCCA model
%   using stochastic gradient descent.
%
% Inputs
%   X1: training data for view 1, each row contains a data sample.
%   X2: training data for view 2, each row contains a data sample.
%   XV1: tuning data for view 1.
%   XV2: tuning data for view 2.
%   XTe1: optional testing data for view 1, can be empty.
%   XTe2: optional testing data for view 2, can be empty.
%   K: dimension of DCCA projection.
%   hiddentype: type of hidden units for the networks. Can be 'linear',
%     'sigmoid','tanh','relu','cubic'.
%   NN1: vector of hidden layer sizes for view 1 network.
%   NN2: vector of hidden layer sizes for view 2 network.
%   rcov1: regularization parameter for view 1.
%   rcov2: regularization parameter for view 2.
%   l2penalty: weight decay parameters for all weight parameters.
%   batchsize: number of samples in a minibatch for estimating gradient.
%   eta0: initial learning rate.
%   decay: the geometric rate in which the learning rate decays.
%   momentum: momentum parameter for SGD, with value in [0 1).
%   maxepoch: number of passes over the training set.
%   randseed: random seed for initializing weights and shuffling training
%     set at each epoch.
% Outputs
%   F1opt: trained network for view 1, it is a cell array containing each
%     all layers of the network. Each layer has a field 'type' indicating
%     the type of hidden activation, a field 'units' indicating the output
%     dimension of the layer, a filed 'l' indicating the weight decay
%     parameter, and a field 'W' containing the weight matrix.
%   F2opt: trained network for view 2, same structure as F1opt.
%   CORR_train: training correlation at each epoch.
%   CORR_tune: tuning correlation at each epoch.
%   CORR_test: testing correlation at each epoch if test data is given.
hiddentype = 'sigmoid';
maxepoch = 3000;
batchsize = 500;
if isempty(XTe1) || isempty(XTe2)
  CORR_test=[];
end
if ~exist('randseed','var') || isempty(randseed)
  randseed=0;
end
rng(randseed);

  
  % fprintf('Result will be saved in %s\n',filename);
  [N,D1]=size(X1); [~,D2]=size(X2);
  %% Set view1 architecture.
  Layersizes1=[D1 NN1];  Layertypes1={};
  for nn1=1:length(NN1)-1;
    Layertypes1=[Layertypes1, {hiddentype}];
  end
  % I choose to set the last layer to be linear.
  Layertypes1{end+1}='linear';
  %% Set view2 architecture.
  Layersizes2=[D2 NN2];  Layertypes2={};
  for nn2=1:length(NN2)-1;
    Layertypes2=[Layertypes2, {hiddentype}];
  end
  Layertypes2{end+1}='linear';
  %% Random initialization of weights.
  % Create two networks F1 and F2 based on the desired structure but with random
  % iniltialized weights.
  F1=deepnetinit(Layersizes1,Layertypes1);
  F2=deepnetinit(Layersizes2,Layertypes2);
  
  % % %   %% Pretrained network.
  % % %   load ./F1G1_RBMPRETRAIN.mat F1;
  
  %% L2 penalty on weights is used for DCCA training.
  for j=1:length(F1)  F1{j}.l=l2penalty;  end
  for j=1:length(F2)  F2{j}.l=l2penalty;  end
  
  %% Compute canonical correlations at the outputs.
  % Run NN with input train data for both views with random weights. return
  % the output layer. 
  FX1=deepnetfwd(X1,F1); FX2=deepnetfwd(X2,F2);
  % calculate linear CCA on the output layer from both networks. A and B are the solutions using SVD.  
  [A,B,m1,m2,D]=linCCA(FX1,FX2,K,rcov1,rcov2);  %clear FX1 FX2;
  SIGN=sign(A(1,:)+eps); A=bsxfun(@times,A,SIGN); B=bsxfun(@times,B,SIGN);
  
  % construct a last layer on F1 (to be F1tmp) that has the weights from a linear CCA.
  f1.type='linear'; f1.units=K; f1.W=[A;-m1*A]; F1tmp=F1; F1tmp{end+1}=f1;
  f2.type='linear'; f2.units=K; f2.W=[B;-m2*B]; F2tmp=F2; F2tmp{end+1}=f2;
  %clear A B m1 m2 D;
  %% train network with the linear CCA weights on the last hidden linear layer. 
  X_train=deepnetfwd(X1,F1tmp);
  % calculate correlation between network outputs X_train1 and X_train2 
 % networks. 
 CORR_train=DCCA_corr(X_train,deepnetfwd(X2,F2tmp),K); %clear X_train;
 
  X_tune=deepnetfwd(XV1,F1tmp);
  CORR_tune=DCCA_corr(X_tune,deepnetfwd(XV2,F2tmp),K); %clear X_tune;
  if ~isempty(XTe1) && ~isempty(XTe2)
    X_test=deepnetfwd(XTe1,F1tmp);
    CORR_test=DCCA_corr(X_test,deepnetfwd(XTe2,F2tmp),K); %clear X_test;
  end
  
  its=0; TIME=0; delta=0; eta=eta0;
  optvalid=CORR_tune; F1opt=F1tmp; F2opt=F2tmp;
%% Concatenate the weights in a long vector.
VV=[];
Nlayers=length(F1); net1=cell(1,Nlayers);
for k=1:Nlayers
  VV=[VV; F1{k}.W(:)];  net1{k}=rmfield(F1{k},'W');
end
Nlayers=length(F2); net2=cell(1,Nlayers);
for k=1:Nlayers
  VV=[VV; F2{k}.W(:)];  net2{k}=rmfield(F2{k},'W');
end
fprintf('Number of weight parameters: %d\n',length(VV));

%% Use GPU if equipped. GPU significantly speeds up optimization.
%if gpuDeviceCount>0
 % fprintf('GPU detected. Trying to use it ...\n');
  %try
   % VV=gpuArray(VV);
    %X1=gpuArray(X1);
    %X2=gpuArray(X2);
    %fprintf('Using GPU ...\n');
  %catch
  %end
%end

%% Start batch stochastic gradient descent.
numbatches=ceil(N/batchsize);

while its<maxepoch
  
  eta=eta0*decay^its; % Reduce learning rate.
  t0=tic;
  rp=randperm(N);   % Shuffle the data set.
  for i=1:numbatches
    idx1=(i-1)*batchsize+1;  % this is 1,301,601,901
    idx2=min(i*batchsize,N); % for i = 1 this is 300,600,900
    idx=[rp(idx1:idx2),rp(1:max(0,i*batchsize-N))]; % 
    X1batch=X1(idx,:);  X2batch=X2(idx,:);
    
    % Evaluate stochastic gradient.
    [E,grad]=DCCA_grad(VV,X1batch,X2batch,net1,net2,K,rcov1,rcov2,drop1,drop2);
    delta=momentum*delta - eta*grad;  % Momentum.
    VV=VV + delta;
  end
  
  %% Record the time spent for each epoch.
  its=its+1; TIME=[TIME, toc(t0)];
  %% Assemble the networks.
  idx=0;
  D=size(X1,2);
  for j=1:length(F1)
    if strcmp(F1{j}.type,'conv')
      convdin=F1{j}.filternumrows*F1{j}.filternumcols*F1{j}.numinputmaps;
      convdout=F1{j}.numoutputmaps;
      W_seg=VV(idx+1:idx+(convdin+1)*convdout);
      F1{j}.W=reshape(W_seg,convdin+1,convdout);
      idx=idx+(convdin+1)*convdout;
      D=F1{j}.units;
    else
      units=F1{j}.units;
      W_seg=VV(idx+1:idx+(D+1)*units);
      F1{j}.W=reshape(W_seg,D+1,units);
      idx=idx+(D+1)*units; D=units;
    end
  end
  
  D=size(X2,2);
  for j=1:length(F2)
    if strcmp(F2{j}.type,'conv')
      convdin=F2{j}.filternumrows*F2{j}.filternumcols*F2{j}.numinputmaps;
      convdout=F2{j}.numoutputmaps;
      W_seg=VV(idx+1:idx+(convdin+1)*convdout);
      F2{j}.W=reshape(W_seg,convdin+1,convdout);
      idx=idx+(convdin+1)*convdout;
      D=F2{j}.units;
    else
      units=F2{j}.units;
      W_seg=VV(idx+1:idx+(D+1)*units);
      F2{j}.W=reshape(W_seg,D+1,units);
      idx=idx+(D+1)*units; D=units;
    end
  end
  
  %% Compute correlations and errors.
  FX1=deepnetfwd(X1,F1);  FX2=deepnetfwd(X2,F2);
  [A,B,m1,m2,D]=linCCA(FX1,FX2,K,rcov1,rcov2);  %clear FX1 FX2;
  SIGN=sign(A(1,:)+eps); A=bsxfun(@times,A,SIGN); B=bsxfun(@times,B,SIGN);
  f1.type='linear'; f1.units=K; f1.W=[A;-m1*A]; F1tmp=F1; F1tmp{end+1}=f1;
  f2.type='linear'; f2.units=K; f2.W=[B;-m2*B]; F2tmp=F2; F2tmp{end+1}=f2;
  %clear A B m1 m2 D;
  %
  X_train=deepnetfwd(X1,F1tmp);
  CORR_train=[CORR_train, DCCA_corr(X_train,deepnetfwd(X2,F2tmp),K)]; %clear X_train;
  train_port(its) = CORR_train(end);
  
  X_tune=deepnetfwd(XV1,F1tmp);
  
  CORR_tune=[CORR_tune, DCCA_corr(X_tune,deepnetfwd(XV2,F2tmp),K)]; %clear X_tune;
  
  if ~isempty(XTe1) && ~isempty(XTe2)
    X_test=deepnetfwd(XTe1,F1tmp);
    CORR_test=[CORR_test, DCCA_corr(X_test,deepnetfwd(XTe2,F2tmp),K)]; clear X_test;
  end
  %test_port(its) = CORR_test(end);
  if CORR_tune(end)>optvalid
    optvalid=CORR_tune(end);
    %fprintf('Epoch %d: getting better tuning correlation %f!\n',its,optvalid);
    F1opt=F1tmp;  F2opt=F2tmp;
  end
end
optimal = -optvalid;

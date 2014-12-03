clc
clear all
close all
nTraining = 10;
nClasses = 13;
nTest = 10 - nTraining;
image_train = [];
image_test = [];
covMat = [];
eVec = [];
eVal = [];

%Path to database
pathToImages = 'D:\Users\Antti\Dropbox\Progemine\MATLAB\pca\small';

%read training set images
index = 1;
for i = 1:nClasses
    for j = 1:nTraining       
        if j == 10
            [img, map] = imread([pathToImages '\f_' num2str(i) '_10.jpg']);
        else
            [img, map] = imread([pathToImages '\f_' num2str(i) '_0' num2str(j) '.jpg']);
        end
        %Convert to grayscale
        gray_img = rgb2gray(img);
        [x,y]=size(gray_img);
        %Convert to vector
        vector = reshape(gray_img, x*y, 1);
        %Create training images matrix
        image_train(:,index) = vector;
        index = index + 1;
    end 
end

%Vector for calculating normalized image
train_mean = mean(image_train');

%Normalize image
for i=1:nClasses*nTraining
    image_train_norm(:,i) = image_train(:,i) - train_mean'; 
end

%Calculate eigenvectors and eigenvalues
[eVec, eVal] = eig(image_train_norm'*image_train_norm);
Y1 = image_train_norm*eVec;
%Calculate training matrix
trainVec = Y1'*image_train_norm;

image_path = 'D:\Users\Antti\Dropbox\Progemine\MATLAB\pca\small\img1.jpg'
%Read test images
%index = 1;
% for i = 1:nClasses
%     for j = 1:nTest
%         j=j+nTraining;
%         if j == 10
             [img, map] = imread(image_path);
%         else
%             [img, map] = imread([pathToImages '\f_' num2str(i) '_0' num2str(j) '.jpg']);
%         end 
%         %Convert to grayscale
         gray_img = rgb2gray(img);
         [x,y]=size(gray_img);
         %Convert to vector
         vector = reshape(gray_img, x*y, 1);
%         %Put vectors in matrix
%         image_test(:,index) = vector;
%         index = index + 1;
%     end
% end



%Normalize test images
% for i=1:nClasses*nTest
%     image_test(:,i) = image_test(:,i) - train_mean'; 
% end
image_test = double(vector) - train_mean';

%Calculate testing matrix
testVec = Y1'*image_test;

minDist = [];
minIndex = [];
i = 1;
%Find the minimum distatces between vectors and determine to which class an
%image belongs
% for i = 1:nClasses*nTest    
     minDist(i) = 1e50;
     minIndex(i) = 0;
     for j = 1:nClasses*nTraining   
         %Distance between vectors
         newDist = norm(trainVec(:,j) - testVec);
         
         %If new distance is smaller than the old one, replace the old one
         if newDist < minDist(i)
             %disp(num2str(newDist))
             minDist(i) = newDist;
             %Calculate the corresponding class to test image
             minIndex(i) = floor((j-1)/(nTraining))+1;
         end
     end
% end

%Calculate performnace
 recog = 0;
 for i = 1:nClasses*nTest
     if minIndex(i) == floor((i-1)/nTest)+1
        recog = recog + 1;
     end
 end
 
 %performance = recog / (nClasses*nTest)
disp(num2str(minIndex));




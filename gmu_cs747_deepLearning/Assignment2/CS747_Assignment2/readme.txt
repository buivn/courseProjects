Assignment 2 - CS747 
Hoang-Dung Bui
G01301478
Team Name on Kaggle Leaderboard: Hoang-Dung Bui
Highest score: 0.5782

I have built a R-CNN to classify the Pascal VOC 2007 dataset.

From the idea of R-CNN, I constructed a block of layers with a residual layer.
Inside the block, there are two conv layers with Batch normalization and relu activation function.
The block are stacked two times, then it is connected to the 1 conv layer from the classifier class. At the output, it connects with AdaptiveAvgPool2D layer, and then a linear layer.


My R-CNN has a problem of overfitting. I simplify the model, change the step size, but still overfitting. It is an interesting issue which I should learn in the next step.


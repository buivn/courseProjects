import torch
from torch.autograd import Variable
from torch.nn.functional import binary_cross_entropy_with_logits as bce_loss
from torch.nn.functional import mse_loss

def discriminator_loss(logits_real, logits_fake):
    """
    Computes the discriminator loss.
    
    You should use the stable torch.nn.functional.binary_cross_entropy_with_logits 
    loss rather than using a separate softmax function followed by the binary cross
    entropy loss.
    
    Inputs:
    - logits_real: PyTorch Tensor of shape (N,) giving scores for the real data.
    - logits_fake: PyTorch Tensor of shape (N,) giving scores for the fake data.
    
    Returns:
    - loss: PyTorch Tensor containing (scalar) the loss for the discriminator.
    """
    
    loss = None
    
    ####################################
    #          YOUR CODE HERE          #
    ####################################
    loss = (bce_loss(logits_fake, Variable(torch.zeros(128, 1).cuda())) + 
        bce_loss(logits_real, Variable(torch.ones(128, 1).cuda())))/2
    # loss = -bce_loss(logits_fake, logits_real)
    
    ##########       END      ##########
    
    return loss

def generator_loss(logits_fake):
    """
    Computes the generator loss.
    
    You should use the stable torch.nn.functional.binary_cross_entropy_with_logits 
    loss rather than using a separate softmax function followed by the binary cross
    entropy loss.

    Inputs:
    - logits_fake: PyTorch Tensor of shape (N,) giving scores for the fake data.
    
    Returns:
    - loss: PyTorch Tensor containing the (scalar) loss for the generator.
    """
    
    loss = None
    
    ####################################
    #          YOUR CODE HERE          #
    ####################################
    # loss = bce_loss(logits_fake, Variable(torch.ones(args.batch_size, 1).cuda()))
    # loss = bce_loss(logits_fake, Variable(torch.ones(args.batch_size, 1).to(device)))
    loss = bce_loss(logits_fake, Variable(torch.ones(128, 1).cuda()))
    
    ##########       END      ##########
    
    return loss


def ls_discriminator_loss(scores_real, scores_fake):
    """
    Compute the Least-Squares GAN loss for the discriminator.
    
    Inputs:
    - scores_real: PyTorch Tensor of shape (N,) giving scores for the real data.
    - scores_fake: PyTorch Tensor of shape (N,) giving scores for the fake data.
    
    Outputs:
    - loss: A PyTorch Tensor containing the loss.
    """
    
    loss = None
    
    ####################################
    #          YOUR CODE HERE          #
    ####################################
    loss = (mse_loss(scores_fake, Variable(torch.zeros(128, 1).cuda())) + 
        mse_loss(scores_real, Variable(torch.ones(128, 1).cuda())))/2
    
    ##########       END      ##########
    
    return loss

def ls_generator_loss(scores_fake):
    """
    Computes the Least-Squares GAN loss for the generator.
    
    Inputs:
    - scores_fake: PyTorch Tensor of shape (N,) giving scores for the fake data.
    
    Outputs:
    - loss: A PyTorch Tensor containing the loss.
    """
    
    loss = None
    
    ####################################
    #          YOUR CODE HERE          #
    ####################################
    loss = mse_loss(scores_fake, Variable(torch.ones(128, 1).cuda()))
    
    ##########       END      ##########
    
    return loss

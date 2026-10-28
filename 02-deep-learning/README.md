# Extrapolating Neural Calculator: LSTM vs Transformer

## Overview

This project explores whether neural networks can learn to evaluate arithmetic expressions without using explicit symbolic computation or predefined arithmetic rules.

The task is formulated as a character-level sequence regression problem: given an expression containing single-digit operands, addition, subtraction and parentheses, the model must predict its integer value.

The main objective is to investigate **generalisation and extrapolation**: how well neural networks perform on expressions that are longer or more structurally complex than those encountered during training.

Two architectures are implemented and compared: a unidirectional Long Short-Term Memory (LSTM) network and a Transformer encoder.

## Objectives

- Train neural networks to evaluate arithmetic expressions through supervised learning.
- Compare recurrent and attention-based architectures on a sequence regression task.
- Evaluate generalisation to out-of-distribution (OOD) and longer expressions.
- Investigate how sequence length, parenthesis depth, operator count and result magnitude affect prediction errors.
- Analyse the strengths and limitations of each architecture under distribution shift.

## Models

### 1. Unidirectional LSTM

The LSTM processes expressions from left to right and uses its final hidden state to predict the numerical result.

- Single recurrent layer.
- Embedding dimension: 16.
- Hidden dimension: 256.
- Dropout: 0.1.
- Final-state read-out.

### 2. Transformer Encoder

The Transformer uses self-attention and sinusoidal positional encodings to represent relationships between characters. Masked mean pooling produces a fixed-size representation for the final regression layer.

- 4 encoder layers.
- 4 attention heads.
- Model dimension: 128.
- Feed-forward dimension: 384.
- Dropout: 0.1.

Both architectures were implemented in PyTorch and trained as regression models, without a symbolic arithmetic evaluation procedure.

## Training and Evaluation

The original dataset includes:

- **12,000 training examples**
- **2,000 validation examples**

Training uses the AdamW optimizer, Smooth L1 loss, a learning rate of \(5 \times 10^{-4}\), batches of 32 and gradient clipping. Models are trained for 50 epochs, with validation performance monitored after each epoch.

To improve structural diversity, 6,000 synthetic expressions were generated for data augmentation. The augmented dataset was used to train the Transformer, while the LSTM was trained on the original dataset.

Evaluation considers three test settings:

- **In-distribution (ID):** expressions similar to those seen during training.
- **Out-of-distribution (OOD):** expressions with different structural properties.
- **Long-sequence:** substantially longer expressions designed to test extrapolation.

The evaluation metrics include exact-match accuracy, Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE). Exact-match accuracy is calculated by rounding predictions to the nearest integer.

## Results

The LSTM consistently outperformed the Transformer across the evaluated test sets, particularly under distribution shift.

Mean exact-match accuracy across multiple independent runs:

| Evaluation split | LSTM | Transformer |
|---|---:|---:|
| In-distribution test | 99.10% | 90.16% |
| OOD test | 95.50% | 80.91% |
| Long-sequence test | 79.60% | 53.80% |

These figures are averages across multiple runs, rather than the best individual results.

### Error Analysis

The two architectures exhibited different extrapolation limitations:

- **LSTM — numerical limitations:** prediction errors increased substantially for large-magnitude results, while remaining comparatively robust to sequence length and parenthesis depth.
- **Transformer — structural limitations:** prediction errors increased with sequence length, operator count and nesting depth, indicating difficulties with expressions more complex than those encountered during training.

The findings suggest that recurrent inductive biases can be advantageous when learning sequential symbolic computations, particularly when evaluation requires generalisation to longer and more deeply nested expressions.

## Technologies

- Python
- PyTorch
- LSTM and Transformer architectures
- Neural network training and optimization
- Synthetic data generation
- Regression evaluation and error analysis

## Skills Demonstrated

- Deep learning and neural network design
- Sequence modelling
- Natural language-style character tokenization and embeddings
- Regression modelling
- Data augmentation
- Model comparison and hyperparameter tuning
- Out-of-distribution evaluation
- Statistical analysis of experimental results
- Error analysis and model interpretability

## Team Contributions

This project was developed collaboratively.

My contributions included working on the dataset pipeline, contributing to the training infrastructure, and participating in the writing and organization of the report. All team members contributed to discussions, interpretation of results and final revisions.

## Key Takeaway

The project demonstrates that high performance on familiar examples does not necessarily imply robust generalisation. By comparing LSTM and Transformer architectures under increasingly difficult evaluation settings, we investigated how model design influences the ability to extrapolate beyond the training distribution.
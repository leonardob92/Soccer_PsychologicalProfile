#%% INSTALLING REQUIRED PACKAGES

#% to install new packages in my environment
#% create environment
#conda create -n spyder-env -y 
#% activate environment  
#conda activate spyder-env
# % install package
#conda install spyder-kernels tensorflow -y

#Make sure the path below is correctly chosen:
#Preferences —> Python interpreter —> Use the following Python interpreter 
#/Users/au550322/miniconda3/envs/spyder-env/bin/python


#%% importing libraries

import numpy as np #for array operations
import pandas as pd #for data manipulation
import tensorflow as tf #for the actual neural networks
from sklearn.preprocessing import MinMaxScaler, normalize #for normalising the data
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.metrics import precision_score, recall_score, f1_score, roc_auc_score, accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns
import os
import pickle

#%% NEURAL NETWORKS

#FOOTBALL PLAYERS VS CONTROLS

# Initialize empty lists to store results
test_loss_list = []
test_accuracy_list = []
precision_list = []
recall_list = []
f1_list = []
confusion_matrix_list = []
training_history_list = []  # Store training history for each permutation


#reading dataset
data = pd.read_excel('/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/Base_Futebol_2021.xlsx')
#extracting psychological features
personality_data = data[['Neuroticism', 'Extroversion','Openness','Agreeableness','Conscientiousness','5-points new','Simple Manual Reaction Time','Forward Digit Span','Backward Digit Span','Tower of Hanoi (moves)']]
float_array = personality_data.values.astype(float)
football_players = float_array[0:153,:]
controllers = float_array[153:,:]
dimensions = personality_data.shape

# Loop over 1000 permutations
for ii in range(1000):

    # Create the features matrix and labels vector
    features = np.vstack([football_players, controllers])
    labels = np.concatenate([np.ones(len(football_players)), np.zeros(len(controllers))])
    
    # Shuffle the data (this may be not necessary since the "train_test_split" function should already randomise the data..)
    shuffle_indices = np.random.permutation(len(labels))
    features = features[shuffle_indices]
    labels = labels[shuffle_indices]
    
    # Create an instance of the MinMaxScaler
    scaler = MinMaxScaler()
    # Normalize the feature matrix using Max-Min normalization
    normalized_features = scaler.fit_transform(features)
    features = normalized_features
    
    # Split the data into training and testing sets
    train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size=0.2, stratify=labels, random_state=42)
    
    # Define the model
    model = tf.keras.models.Sequential()
    model.add(tf.keras.layers.Dense(dimensions[1], activation='relu')) #input layer with the same number of neurons as input variables
    model.add(tf.keras.layers.Dense(16, activation='relu'))
    model.add(tf.keras.layers.Dense(1, activation='sigmoid'))
    
    # Compile the model
    model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
    
    # Train the model
    model.fit(train_features, train_labels, epochs=20, batch_size=32,verbose = 0) #batch_size refers to the number of samples (i.e. participants in this case) that are used together to compute gradient descend. After that the weights for different batches are summed together. This is good especially for memory issue (you don't have to load the whole dataset at once) and for preventing potential issues with local minima, overfitting, etc.
    #here, the evaluation is done in the same line of code (faster, but for now I like to keep them separately to have a better control on the different steps)
    #model.fit(train_features, train_labels, epochs=20, batch_size=32, validation_data=(test_features, test_labels))
    
    history = model.fit(train_features, train_labels, epochs=20, batch_size=32, verbose=1)
    
    # Evaluate the model on the test set
    loss, accuracy = model.evaluate(test_features, test_labels)
    test_loss_list.append(loss)
    test_accuracy_list.append(accuracy)
    
    # Make predictions
    predictions = model.predict(test_features)
    #print(predictions)
    pred_labels = np.array(predictions >= 0.5, dtype=int)
    #precision (proportion of true positive predictions out of all positive predictions made by the classifier)
    precision = precision_score(test_labels, pred_labels)
    #print("Precision:", precision)
    #recall (proportion of true positive predictions out of all actual positive instances)
    recall = recall_score(test_labels, pred_labels)
    #print("Recall:", recall)
    #f1 score (harmonic mean of precision and recall)
    f1 = f1_score(test_labels, pred_labels)
    #print("F1 Score:", f1)
    #confusion matrix
    y_pred = model.predict(test_features)
    y_pred = (y_pred > 0.5).astype(int)  # Convert probabilities to binary predictions
    cm = confusion_matrix(test_labels, y_pred, labels = [1,0])
    #print(cm)
    
    print(ii + 1) #print the counter
    
    # storing metrics
    precision_list.append(precision_score(test_labels, pred_labels))
    recall_list.append(recall_score(test_labels, pred_labels))
    f1_list.append(f1_score(test_labels, pred_labels))
    confusion_matrix_list.append(confusion_matrix(test_labels, (predictions > 0.5).astype(int), labels=[1, 0]))
    training_history_list.append(history.history)  # Store training history for each permutation
    

# Display or use the collected results as needed
print("Average Test Loss:", np.mean(test_loss_list), "+/-", np.std(test_loss_list))
print("Average Test Accuracy:", np.mean(test_accuracy_list), "+/-", np.std(test_accuracy_list))
print("Average Precision:", np.mean(precision_list), "+/-", np.std(precision_list))
print("Average Recall:", np.mean(recall_list), "+/-", np.std(recall_list))
print("Average F1 Score:", np.mean(f1_list), "+/-", np.std(f1_list))

average_confusion_matrix = np.mean(confusion_matrix_list, axis=0)
std_confusion_matrix = np.std(confusion_matrix_list, axis=0)
print("Average Confusion Matrix:")
print(average_confusion_matrix)
print("Standard Deviation Confusion Matrix:")
print(std_confusion_matrix)

#%% SAVING RESULTS

output_directory = '/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/'
# Save all variables to a file using pickle in the specified directory
result_data = {
    'test_loss_list': test_loss_list,
    'test_accuracy_list': test_accuracy_list,
    'precision_list': precision_list,
    'recall_list': recall_list,
    'f1_list': f1_list,
    'confusion_matrix_list': confusion_matrix_list,
    'training_history_list': training_history_list
}

output_filepath = os.path.join(output_directory, 'results.pkl')

with open(output_filepath, 'wb') as file:
    pickle.dump(result_data, file)

print(f"Results saved to: {output_filepath}")

#%% plotting average (over the 1000 permutations) training loss and accuracy

# loading previously saved results
with open('/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/results.pkl', 'rb') as f:
    data = pickle.load(f)

# extracting results from loaded file
training_history_list = data.get('training_history_list')

# mean over 1000 permutations
average_loss = np.mean([history['loss'] for history in training_history_list], axis=0)
average_accuracy = np.mean([history['accuracy'] for history in training_history_list], axis=0)

# Set the default font to "Helvetica Neue Light"
plt.rcParams['font.family'] = 'Helvetica Neue'

# Plot training loss and accuracy
plt.figure(figsize=(12, 6))
plt.subplot(1, 2, 1)
plt.plot(average_loss, label='Training Loss')
plt.title('Training Loss - average 1000 permutations')
plt.xlabel('Epochs')
plt.ylabel('Loss')
plt.legend()
plt.grid(True)  # Add grid

plt.subplot(1, 2, 2)
plt.plot(average_accuracy, label='Training Accuracy')
plt.title('Training Accuracy - average 1000 permutations')
plt.xlabel('Epochs')
plt.ylabel('Accuracy')
plt.legend()
plt.grid(True)  # Add grid

plt.tight_layout()
plt.savefig('/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/TrainingLoss_Accuracy_1000_permutations.png', dpi=300)
plt.show()

#%% plots for all the permutations

# loading previously saved results
with open('/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/results.pkl', 'rb') as f:
    data = pickle.load(f)

# extracting results from loaded file
test_accuracy_list = data.get('test_accuracy_list')
precision_list = data.get('precision_list')
recall_list = data.get('recall_list')
f1_list = data.get('f1_list')

# Combine metrics into a DataFrame
metrics_df = pd.DataFrame({
    'Test Accuracy': test_accuracy_list,
    'Precision': precision_list,
    'Recall': recall_list,
    'F1 Score': f1_list,
})

plt.rcParams['font.family'] = 'Helvetica Neue'
# Plot box plots
plt.figure(figsize=(12, 8))
ax = sns.boxplot(data=metrics_df)
ax.set_title('Metrics Distribution over 1000 Permutations')
# Add a grid
ax.yaxis.grid(True, linestyle='--', alpha=0.7)
#sns.boxplot(data=metrics_df)
plt.title('Metrics Distribution over 1000 Permutations')
# Save the figure with high resolution (e.g., 300 dpi)
plt.savefig('/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/metrics_distribution_boxplot.png', dpi=300)
plt.show()

#%% violin plots

plt.rcParams['font.family'] = 'Helvetica Neue'
# Plot violin plots
plt.figure(figsize=(12, 8))
ax = sns.violinplot(data=metrics_df)
ax.set_title('Metrics Distribution over 1000 Permutations')
# Add a grid
ax.yaxis.grid(True, linestyle='--', alpha=0.7)
# Save the figure with high resolution (e.g., 300 dpi)
plt.savefig('/Users/au550322/Documents/AarhusUniversitet/CarlsbergFoundation_Oxford_PaperWork/Research/PredragPetrovic/Alberto_Predrag_Torbjorn/metrics_distribution_violins.png', dpi=300)
plt.show()

#%% 

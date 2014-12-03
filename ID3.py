
import math

def readSample():
    dataSet=[
        ['Age','Marriage','Salary','Buy'],
		[0,0,2,'no'],
		[0,0,1,'no'],
		[1,0,0,'yes'],
		[1,1,1,'no'],
		[1,1,0,'no'],
		[1,1,0,'no'],
		[1,1,1,'no'],
		[0,0,2,'no'],
		[0,1,0,'no'],
		[1,1,1,'no'],
		[0,1,2,'yes'],
		[1,1,1,'no'],
		[1,0,1,'yes'],
		[1,1,0,'no'],
		[1,0,1,'yes'],
		[0,0,0,'no']]
    return dataSet

def calculate_Entropy(samples, categories, feature_idx):
    all_features = [sample[feature_idx] for sample in samples[1:]]
    features = set(all_features)
    matrix_I = [[0 for i in range(len(categories))] for j in range(len(features))]
    for sample in samples[1:]:
        x = sample[feature_idx]
        y = sample[-1]
        index_i = list(features).index(x)
        index_j = list(categories).index(y)
        matrix_I [list(features).index(x)][list(categories).index(y)] += 1

    entropy = 0
    for x_split in matrix_I:
        shannon = 0.0
        if 0 not in x_split :
            for y_split in x_split:
                prob = float(y_split)/float(sum(x_split))
                shannon -= prob * math.log(prob, 2)
        entropy += shannon * float(sum(x_split))/float(len(all_features))
    return entropy

def getCategories(samples):
    y = [sample[-1] for sample in samples[1:]]
    return set(y)

def makeSplitByValue(samples, feature_idx, feature_value):
    splited = []
    title = samples[0][:feature_idx]
    title.extend(samples[0][feature_idx+1:])
    splited.append(title)

    for sample in samples[1:]:
        if sample[feature_idx] == feature_value :
            splited_row = sample[:feature_idx]
            splited_row.extend(sample[feature_idx+1:])
            splited.append(splited_row)
    return splited

def majorityCnt(categories):
    classCount = {}
    for vote in categories:
        if vote not in classCount.keys():
            classCount[vote] = 0
        classCount[vote] += 1
    return max(classCount)

def growTree(samples):
    categories = [sample[-1] for sample in samples[1:]]
    if categories.count(categories[0]) ==len(categories):
        return categories[0]
    if len(samples[0]) == 1:
        return majorityCnt(categories)

    best_idx = getBestIndex(samples)
    best_name = samples[0][best_idx]

    Dtree = {best_name:{}}
    feature_values = [sample[best_idx] for sample in samples[1:]]
    feature_set = set(feature_values)

    for feature_value in feature_set:
        Dtree[best_name][feature_value] = growTree(makeSplitByValue(samples, best_idx, feature_value))

    return Dtree


def getBestIndex(samples):
    categories = getCategories(samples)
    num_features = len(samples[0]) -1

    bestEntropy = 100
    bestFeature = -1
    for i in range(num_features):
        entropy = calculate_Entropy(samples, categories, i)
        if entropy < bestEntropy:
            bestEntropy = entropy
            bestFeature = i
    return bestFeature


def main():
    samples = readSample()
    Dtree = growTree(samples)
    print Dtree

if __name__ == '__main__':
    main()

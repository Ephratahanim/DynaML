# Bayes Learn
Scala Library/REPL for working with Bayesian graphical models.

Introduction
============

Bayes learn is a scala library/repl for implementing and working with Probabilistic Graphical Models. The aim is to build a robust set of abstract classes and interfaces, so general graph based ML algorithms can be realized using the API.

Installation
============
Prerequisites: Maven to build the executables.

1. Clone this repository
2. Run the following.
```shell
  mvn clean compile
  mvn package
```

3. Make sure you give execution permission to `bayeslearn-repl` in the `target/bin` directory.
```shell
  chmod +x target/bin/bayesLearn-repl
  target/bin/bayesLearn-repl
```
  You should get the following prompt.
  
```
  Welcome to Bayes Learn 



         \,,,/
         (o o)
-----oOOo-(_)-oOOo-----
scala> 
  
```

Getting Started
===============

The `data/` directory contains a few sample data sets, we will be using them in the following example.

1. First we create a linear Gaussian Bayesian model using a csv data set. We will assume that the last column in each line of the file is the target variable, and we build a Bayesian regression model with additive Gaussian noise.

```scala
  val model = GaussianLinearModel(utils.getCSVReader("data/challenge.txt", '\t'))
```

2. We can now (optionally) add a Kernel on the model to create a generalized linear Bayesian model. Although the kernel feature extraction is experimental as of now.

```scala
  val rbf = new RBFKernel(0.005)
  model.applyKernel(rbf)
```

3. Now we can use Gradient Descent to learn the parameters w of the Bayesian model, with priors corresponding to zero mean and variance depending on the regularization parameter.

```scala
  model.setMaxIterations(200).setLearningRate(0.001).learn
```

4. We can now predict the value of the targer variable given a new point consisting of a Vector of features

```scala
  val pred = model.predict(DenseVector(0.68220219, 0.657091331))
```

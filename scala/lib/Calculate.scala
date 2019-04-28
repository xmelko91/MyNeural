package lib

trait Calculate extends PrepareData {

  val learningRate: Double
  val numberOfAllLayers : Int = numberHiddenLayers + 2

  //calculate outputs for out layer of neurons, and put it in "output" values sof Neurons
  def calculateOuts(in : Array[Double], arr: Array[Array[Double]], out: Array[Neuron]): Unit = {

    for (x <- out.indices){
      out(x).output += arr(arr.length - 1)(x)
      out(x).error = 0.0
    }

    for (x <- in.indices){
      for (y <- out.indices){
        out(y).output += in(x) * arr(x)(y)
      }
    }
    out.foreach(x => x.output = sigmoid(x.output))
  }

  def calculateOuts(in : Array[Neuron], arr: Array[Array[Double]], out: Array[Neuron]): Unit = {

    for (x <- out.indices){
      out(x).output += arr(arr.length - 1)(x)
      out(x).error = 0.0
    }

    for (x <- in.indices){
      for (y <- out.indices){
        out(y).output += in(x).output * arr(x)(y)
      }
    }
    out.foreach(x => x.output = sigmoid(x.output))
  }

  /*
  Calculate error of last layer
  @ out : last layer of Neurons
  @ pref : number of ideal out Neuron

  calculated error put in Neuron.error value
   */

  def outputError(out : Array[Neuron], pref : Int): Unit = {
    for (x <- out.indices){
      var need = 0.01
      if (x == pref) {need = 0.99}
      out(x).error = need - out(x).output
    }
  }

  /*
  Calculate error of hidden layers

  @ past : prev. layer of Neurons
  @ arr : Array ow weights
  @ now : this layer of Neurons

  calculated error put in Neuron.error value
   */
  def hiddenError(past : Array[Neuron], arr: Array[Array[Double]], now: Array[Neuron]): Unit = {
    for (x <- past.indices){
      for (y <- now.indices){
        past(x).error += now(y).error * arr(x)(y)
      }
    }
  }

  /*
  Setting new weights for Neurons chains

  @ past : prev. layer of Neurons/inputs
  @ arr : Array of weights
  @ now : this layer of Neurons

  calculated new weights and changed in array of weights
   */

  def calcNewWeight(past : Array[Double], arr : Array[Array[Double]], now : Array[Neuron]): Unit = {
    for (x <- past.indices){
      for (y <- now.indices){
        arr(x)(y) += learningRate * now(y).error * fSigmoid(now(y).output) * past(x)
      }
    }

    //biases weights
    for (x <- now.indices){
      arr(arr.length - 1)(x) += learningRate * now(x).error * fSigmoid(now(x).output)
      now(x).output = 0.0
    }
  }

  def calcNewWeight(past : Array[Neuron], arr : Array[Array[Double]], now : Array[Neuron]): Unit = {
    for (x <- past.indices){
      for (y <- now.indices){
        arr(x)(y) += learningRate * now(y).error * fSigmoid(now(y).output) * past(x).output
      }
    }

    //biases weights
    for (x <- now.indices){
      arr(arr.length - 1)(x) += learningRate * now(x).error * fSigmoid(now(x).output)
      now(x).output = 0.0
    }
  }


  //full iteration of calculating outputs
  def calculateOutValuesIteration(input : Array[Double]): Unit = {
    calculateOuts(input, weights.weights(0), neuronArray(0))
    for (x <- 0 until numberHiddenLayers){
      calculateOuts(neuronArray(x), weights.weights(x + 1),neuronArray(x + 1))
    }
  }

  //full iteration of calculating errors
  def calculateErrorsIteration(idealOut: Int): Unit = {
    outputError(neuronArray(neuronArray.length - 1), idealOut)
    for (x <- 0 until numberHiddenLayers){
      hiddenError(neuronArray(neuronArray.length - 2 - x),
        weights.weights(neuronArray.length - 1 - x),
        neuronArray(neuronArray.length - 1 - x))
    }
  }

  //full iteration of setting of new Weights
  def calculateNewWeightsIteration(input : Array[Double]):Unit = {
    for (x <- 0 until numberHiddenLayers){
      calcNewWeight(
        neuronArray(weights.weights.length - x - 2),
        weights.weights(weights.weights.length - x - 1),
        neuronArray(weights.weights.length - 1 - x))
    }
    calcNewWeight(input, weights.weights(0), neuronArray(0))
  }

  //fill iteration of learn
  def fullCalculatingIteration(data: Array[Double], ideal : Int) : Unit = {
    calculateOutValuesIteration(data)
    calculateErrorsIteration(ideal)
    calculateNewWeightsIteration(data)
  }

  //finding final output
  def outputOfNeural(arr : Array[Neuron]): Int = {
    var out = new Neuron
    arr.foreach(n => if (n.output > out.output) out = n)
    arr.indexOf(out)
  }

  //cleaning neurons from side eff., util func.
  def clearSide(neuronArray: Array[Array[Neuron]]): Unit = {
    neuronArray.foreach(x =>
    x.foreach(y =>
    y.output = 0.0))
  }

  //is function activation for output values
  def sigmoid(x : Double): Double = 1 / (1 + math.exp(-x))

  //F' of sigmoid
  def fSigmoid(x : Double): Double = x * (1 - x)

  //calculating sum of squares of output - to see total Error, util func.
  def calcTotalError(out : Array[Neuron]): Double = {
    out.map(x => x.error * x.error).sum
  }
}

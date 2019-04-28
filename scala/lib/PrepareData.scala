package lib

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

trait PrepareData {

  val numberHiddenLayers : Int
  val numberOfHiddenNeurons: Int
  val numberOfInputNeurons: Int
  val numberOfOutNeurons: Int


  //create Serializable Weights class - all matrices of all layers
  val weights: Weights

  //Prepare all Neuron layers
  val neuronArray: Array[Array[Neuron]]


  /*
 @ past - number of neurons from past layer
 @ now - number neurons of this layer

 @ returns matrix of new random weights + biases weights
  */
  def prepareWeights(past : Int, now : Int) : Array[Array[Double]] = {
    val out = new Array[Array[Double]](past + 1)
    val randFactor = 1 / math.sqrt(past)

    for (x <- 0 to past){
      val arr = new Array[Double](now)
      for (y <- 0 until now){
        arr(y) = math.random() * randFactor
      }
      out(x) = arr
    }
    out
  }

  def getNewWeights: Array[Array[Array[Double]]] = {
    val numberOfNeurons : Int = {
      if (numberHiddenLayers == 0) numberOfOutNeurons
      else numberOfHiddenNeurons
    }
    val out = new Array[Array[Array[Double]]](1 + numberHiddenLayers)
    out (0) = prepareWeights(numberOfInputNeurons,numberOfNeurons)
    for (x <- 1 until numberHiddenLayers){
      out(x) = prepareWeights(numberOfHiddenNeurons, numberOfHiddenNeurons)
    }
    out(numberHiddenLayers) = prepareWeights(
      if (numberHiddenLayers == 0) numberOfInputNeurons //if no hidden layers here - set input layer
      else numberOfHiddenNeurons,
      numberOfOutNeurons)

    out
  }
  def getNewNeuronsLayer: Array[Array[Neuron]] = {
    val out = new Array[Array[Neuron]](numberHiddenLayers + 1)
    for (x <- 0 to numberHiddenLayers){
      var capacity = if (x == numberHiddenLayers) numberOfOutNeurons else numberOfHiddenNeurons
      val outN = new Array[Neuron](capacity)
      for (y <- 0 until capacity) {
        outN(y) = new Neuron
      }
      out(x) = outN
    }
    out
  }

  def savingWeights(weightsToWrite: PropertiesTransit, name: String): Unit ={

    val fos = new ObjectOutputStream(new FileOutputStream(name))
    fos.writeObject(weightsToWrite)
    fos.close()
  }

  def readWeights(name : String) : PropertiesTransit = {
    val fis = new ObjectInputStream(new FileInputStream(name))
    fis.readObject().asInstanceOf[PropertiesTransit]
  }
}

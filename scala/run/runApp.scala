package run

import java.nio.file.{Files, Paths}

import lib.{Calculate, Neuron, PropertiesTransit, Weights}

object runApp extends App with Calculate{

  override val learningRate = 0.1
  override val numberHiddenLayers = 2
  override val numberOfHiddenNeurons = 800
  override val numberOfInputNeurons = 784
  override val numberOfOutNeurons = 10
  override val weights = new Weights {
    override val weights: Array[Array[Array[Double]]] = getNewWeights
  }
  override val neuronArray: Array[Array[Neuron]] = getNewNeuronsLayer


  //properties for Serialization
  val props: PropertiesTransit = new PropertiesTransit {
    override var weights1: Weights = weights
    override val learningRate1: Double = learningRate
    override val numberHiddenLayers1: Int = numberHiddenLayers
    override val numberOfHiddenNeurons1: Int = numberOfHiddenNeurons
    override val numberOfInputNeurons1: Int = numberOfInputNeurons
    override val numberOfOutNeurons1: Int = numberOfOutNeurons
  }


  val pathToWeights:String = "myNeuroWeb"
  val epoch = 1000


  val train = getClass.getResourceAsStream("train_data").readAllBytes()
  val names = getClass.getResourceAsStream("train_label").readAllBytes()

  import lib.PrepareMnistData
  def getData(number : Int) = PrepareMnistData.getDataFromMnist(number, (train, names))

  def iteration(number : Int): Unit = {
    val data = getData(number)
    fullCalculatingIteration(data._2, data._1)
  }

  var sum = 0.0
  for (e <- 0 to epoch) {
    for (x <- 1 to 60000) {
      iteration(x)
      sum += calcTotalError(neuronArray(neuronArray.length - 1))
      if ((x - 1) % 1000 == 0 && x != 1) {
        println(sum)
        sum = 0.0
      }
    }
    props.weights1 = weights
    savingWeights(props, pathToWeights)
    println("saved!")
  }

}

package run

import lib.{Calculate, Neuron, PrepareMnistData, Weights}

object testApp extends App with Calculate{

  val pathToWeights:String = "myNeuroWeb"
  var dataName = "train_data"
  var labelName = "train_label"

  val props = readWeights(pathToWeights)
  println(props.learningRate1)

  override val learningRate = props.learningRate1
  override val numberHiddenLayers = props.numberHiddenLayers1
  override val numberOfHiddenNeurons = props.numberOfHiddenNeurons1
  override val numberOfInputNeurons = props.numberOfInputNeurons1
  override val numberOfOutNeurons = props.numberOfOutNeurons1
  override val weights: Weights = props.weights1
  override val neuronArray: Array[Array[Neuron]] = getNewNeuronsLayer


  var train = getClass.getResourceAsStream(dataName).readAllBytes()
  var names = getClass.getResourceAsStream(labelName).readAllBytes()

  def getData(number : Int): (Int, Array[Double]) = PrepareMnistData.getDataFromMnist(number, (train, names))

  def iteration(number : Int, data : (Int, Array[Double]) = null): Int = {
    if (data == null) {
      val data1 = getData(number)
      calculateOutValuesIteration(data1._2)
      data1._1
    }
    else {
      calculateOutValuesIteration(data._2)
      data._1
    }
  }

  def show(number: Int) {
    val data = getData(number)
    iteration(number)
    PrepareMnistData.outValues(data._2)
    println(outputOfNeural(neuronArray(neuronArray.length - 1)))
  }

  def testDate(): Double = {
    train = getClass.getResourceAsStream("test_data").readAllBytes()
    names = getClass.getResourceAsStream("test_label").readAllBytes()

    var sum = 0
    for (x <- 1 to 10000){
      val prefered = iteration(x)
      if (prefered == outputOfNeural(neuronArray(neuronArray.length - 1)))
        sum += 1
      clearSide(neuronArray)
    }

    sum * 100.0 / 10000.0
  }

  //show(15555)
  println(testDate() + " %")
}

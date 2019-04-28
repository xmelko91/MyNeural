package lib

import scala.collection.mutable.ArrayBuffer

object PrepareMnistData {

  /*
  @number - number of image from dataset of Mnist
  @train - (ByteArray of data image, ByteArray of data label)
  @return (Number that shows on image, Array of double values)
   */
  def getDataFromMnist(number: Int, train: (Array[Byte], Array[Byte]), batch: Int = 784): (Int, Array[Double]) = {
    (math.BigInt.apply(train._2.slice(7 + number, 8 + number)).intValue()
      , getNewArray(train._1.slice(16 + batch * (number - 1), 16 + batch * number)).toArray)
  }


  //Returns array of Double values from unsignet Byte array, min value upped by 0.1, max value lowed by 0.1
  private def getNewArray(arr: Array[Byte], rows: Int = 28, columns: Int = 28): ArrayBuffer[Double] = {
    var newArr = new ArrayBuffer[Double]()
    for (x <- arr) {
      if (x.intValue() < 0)
        newArr += ((126 + x.intValue()) + 124.9).doubleValue() / 255.0
      else
        newArr += (x.intValue().doubleValue() + 0.1) / 255.0
    }
    newArr
  }

  //Util func - shows on screen image of input array
  def outValues(value: Array[Double], rows: Int = 28) = {
    var z = 1
    for (y <- value) {
      print(y.round + " ")
      if (z % rows == 0 && z != 0)
        print("\n")
      z += 1
    }
  }


}

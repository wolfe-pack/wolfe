package ml.wolfe.util

/**
 * Defines a hyper parameter with options
 * Created by Ingolf Becker on 05/11/2014.
 * @param name The name of the HyperParameter
 * @param min Minimum value of the hyper parameter
 * @param max Maximum value of the hyper parameter
 */
case class HyperParameter(name:String, min: Double = Double.NegativeInfinity, max: Double = Double.PositiveInfinity)


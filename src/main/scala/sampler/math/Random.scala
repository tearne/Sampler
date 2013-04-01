/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.math

//trait RandomFactory {
//  def newRandom: Random = new Random
//}
//
//object RandomFactory extends RandomFactory

class Random extends scala.util.Random with Serializable{

  def nextDouble(min: Double, max: Double): Double = 
    (max - min) * nextDouble() + min
  
  def nextBoolean(p: Probability): Boolean = math.random < p.value
}


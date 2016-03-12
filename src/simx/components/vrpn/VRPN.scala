/*
 * Copyright 2012 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.vrpn

import simx.core.ontology.{Symbols, types}
import simx.core.svaractor.semantictrait.base.BaseValueDescription

/* author: dwiebusch
* date: 23.09.2010
*/

object VRPN{
  val button      = types.Boolean as Symbols.button
  val position    = types.Transformation as Symbols.position
  val orientation = types.Transformation as Symbols.orientation
  val oriAndPos   = types.Transformation
  val text        = types.String
  val analog      = types.Vector2 as Symbols.analogInput
  val timestamp   = types.Time
  val correction  = types.Typ as Symbols.name

  val url = types.String as Symbols.uRL
  val id = types.Identifier as Symbols.trackingTargetId
  val updateRateInMillis = types.Long as Symbols.refreshRate

  def prettyPrintMat( mat : Any ) : String =
    "\n " + mat.toString.replaceAll(";", "\n").split("[\\(\\)]")(1)

}
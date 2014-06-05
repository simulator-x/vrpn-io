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

package simx.components.vrpn.devices

import simx.components.vrpn.VRPN
import simx.core.ontology.{EntityDescription, Symbols}

/**
 * @author dwiebusch
 * Date: 05.07.11
 * Time: 11:03
 */

case class TrackingTarget(url : String, id : Symbol) extends VRPNAspect(Symbols.trackingTarget){
  def getCreateParams = addCVars{ Set(VRPN.id.apply(id), VRPN.url.apply(url), VRPN.timestamp(-1)) }
  def getFeatures               = Set(VRPN.oriAndPos, VRPN.url, VRPN.id, VRPN.timestamp)
  def getProvidings             = getFeatures
}

case class SimpleTarget(url : String, id : Symbol ) {
//  def realize( handler : (Entity) => Unit ) {
//    desc.realize( handler )
//  }

  val desc = new EntityDescription(
    TrackingTarget(url, id)
  )
}
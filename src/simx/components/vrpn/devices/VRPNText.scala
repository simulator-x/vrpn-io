/*
 * Copyright 2015 The SIRIS Project
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
import simx.core.entity.description.SValSet
import simx.core.ontology.{EntityDescription, Symbols}

/**
 * Created by martin 
 * on 29/06/15.
 */
case class VRPNText(localName: String, targetUrl: String, sensorId: Long) extends VRPNTarget(Symbols.string) {
  def getCreateParams = addCVars{ SValSet(VRPN.text(""), VRPN.id.apply(Symbol(""+sensorId)) , VRPN.url(targetUrl) , VRPN.timestamp(-1))}
  def getFeatures = Set(VRPN.text, VRPN.url, VRPN.id, VRPN.timestamp)
  def getProvidings = getFeatures
  def desc: EntityDescription = new EntityDescription(
    localName + "[" + targetUrl + "]",
    this
  )
}
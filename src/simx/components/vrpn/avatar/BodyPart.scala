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

package simx.components.vrpn.avatar

import simx.components.vrpn.devices.TrackingTarget
import simx.core.entity.description.{TypedSValSet, SVal, SValSet}
import simx.core.helper.chirality.Chirality
import simx.core.ontology._
import simx.core.svaractor.StateParticle

object BodyPart{
  def describedBy(entityType: GroundedSymbol) = BodyPartDescription(entityType)
}
case class BodyPartDescription(entityType: GroundedSymbol, chiralityOption: Option[Chirality] = None) {
  def andChirality(c: Chirality) = BodyPartDescription(entityType, Some(c))
  def accessedViaId(vrpnId: Int) = {
    var properties = Set[SVal.SValType[_]](types.EntityType(entityType))
    if(chiralityOption.isDefined) properties += types.Chirality(chiralityOption.get)
    new BodyPart(vrpnId, Set(), properties)
  }
}
class BodyPart(val vrpnId: Int, val subParts: Set[BodyPart], val properties: Set[SVal.SValType[_]]) {
  def hasSubParts(subParts: BodyPart*) = new BodyPart(vrpnId, subParts.toSet, properties)
  def hasSubParts(subParts: Set[BodyPart]) = new BodyPart(vrpnId, subParts.toSet, properties)


  def toDesc(vrpnServerUrl: String): EntityDescription = new EntityDescription(
    TrackingTarget(vrpnServerUrl, Symbol(vrpnId.toString)) :: subParts.map(_.toDesc(vrpnServerUrl)).toList,
    Symbol(shortName),
    List[Symbol](),
    Set[simx.core.ontology.Annotation](),
    SValSet(properties.toSeq:_*)
  )

  def shortName = getName(short = true)

  def getEP = properties.foldLeft(new TypedSValSet[Any]())(_ and _)
  def getEntityProperties = getEP

  override def toString = getName(short = false)

  private def getName(short: Boolean): String =
    "BodyPart[" + properties.map(_.getValue).flatten.map(_.toString).mkString(", ") +
      (if(!short && subParts.nonEmpty)
        " containing " + subParts.size + " sub parts " + subParts.map(_.toString).mkString("{", ", ", "}")
      else "") +
      "]"
}

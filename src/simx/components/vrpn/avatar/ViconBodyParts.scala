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


import simx.core.helper.chirality.Chirality
import simx.core.ontology._
import simx.core.helper.chirality

/**
 *
 *
 * Created by dennis on 12/6/15.
 */
object ViconBodyParts extends BodyPartNames{
  val bodyParts = Set[BodyPart](
    BodyPart describedBy HEAD                                  accessedViaId 0,
    BodyPart describedBy SHOULDER andChirality chirality.None  accessedViaId 0  hasSubParts shoulderSubParts,
    BodyPart describedBy SPINE                                 accessedViaId 0,
    BodyPart describedBy HIP      andChirality chirality.None  accessedViaId 0  hasSubParts hipSubParts,
    BodyPart describedBy ELBOW    andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy ELBOW    andChirality chirality.Right accessedViaId 0,
    BodyPart describedBy WRIST    andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy WRIST    andChirality chirality.Right accessedViaId 0,
    BodyPart describedBy HAND     andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy HAND     andChirality chirality.Right accessedViaId 0,
    BodyPart describedBy KNEE     andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy KNEE     andChirality chirality.Right accessedViaId 0,
    BodyPart describedBy ANKLE    andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy ANKLE    andChirality chirality.Right accessedViaId 0,
    BodyPart describedBy FOOT     andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy FOOT     andChirality chirality.Right accessedViaId 0
  )

  def upperBodySymbols =
    Seq(HEAD, SHOULDER, SPINE, HIP, ELBOW, WRIST, HAND)

  private def shoulderSubParts = Set(
    BodyPart describedBy SHOULDER andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy SHOULDER andChirality chirality.Right accessedViaId 0
  )

  private def hipSubParts = Set(
    BodyPart describedBy HIP andChirality chirality.Left  accessedViaId 0,
    BodyPart describedBy HIP andChirality chirality.Right accessedViaId 0
  )

  def getBodyPart(description: GroundedSymbol) =
    bodyParts.find(_.properties contains types.EntityType(description))

  def getBodyPart(description: GroundedSymbol, c : Chirality) =
    bodyParts.filter(_.properties contains types.EntityType(description)).find(_.properties contains types.Chirality(c))


  private def getSubset(descriptions: GroundedSymbol*): Set[BodyPart] =
    descriptions.map(types.EntityType.apply).flatMap(d => bodyParts.filter(_.properties contains d)).toSet
}

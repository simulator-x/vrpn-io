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

import simplex3d.math.doublex.ConstQuat4d
import simplex3d.math.floatx.ConstVec2f
import simx.components.vrpn.converter.Json
import simx.core.svaractor.TimedRingBuffer.At
import simplex3d.math.floatx.{Mat3f, ConstVec2f}
import simx.core.svaractor._
import simx.core.entity.Entity
import simx.core.component.Component
import simx.core.entity.description._
import simx.core.ontology.{GroundedSymbol, Symbols, types}
import simx.core.svaractor.semantictrait.base.GroundedSymbolBase
import vrpn.{AnalogRemote, TextReceiver, ButtonRemote, TrackerRemote}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.component.{ComponentAspect, EntityConfigLayer}
import simplex3d.math.float._
import scala.reflect.ClassTag

/*
 * Created by Dennis Wiebusch and Martin Fischbach
 */

case class VRPNComponentAspect(name : Symbol) extends ComponentAspect[VRPNConnector](Symbols.vrpn, name){
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set()
  def getCreateParams: NamedSValSet = NamedSValSet(aspectType)
}

/*
 * To register new vrpn types, you have to take 2 steps here:
 * 1. create a handling function and add it to the createHandle function (as already done with the existing functions)
 * 2. enter the correct call to create for the new type in the VRPNActor.createOValue function
 *
 * --> don't forget to follow the steps which can be found in the SirisVRPN.scala file
 */
class VRPNConnector(name : Symbol = 'vrpnconnector ) extends Component(name, Symbols.vrpn) with EntityConfigLayer{
  //enable debugging output here
  val debug = true
  //the internal actor, which just updates its ovalues
  protected def performSimulationStep() {
    this.simulationCompleted()
  }

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) =
    SValSet()

  protected def finalizeConfiguration(e: Entity){}

  /**
   * standard full trackerinfo handling
   */
  protected def configure(params: SValSet) {}

  private type TSSVar = StateParticle[VRPN.timestamp.dataType]

  def removeFromLocalRep(e: Entity) {}

  override def shutdown(){
    VRPNFactory.cleanup()
    super.shutdown()
  }

  def createHandle[T]( e: Entity, typ : GroundedSymbolBase[_], semantics : Symbol,  oval : StateParticle[T],
                       timeStampSVar : Option[TSSVar] = None ) : Option[ (Any, Any) => Unit ] =
    typ match {
      case VRPN.text.semantics         => Some(handleText(semantics, oval, timeStampSVar) )
      case VRPN.button.semantics       => Some(handleButton(semantics, oval, timeStampSVar) )
      case VRPN.analog.semantics       => Some(handleAnalog( semantics, oval, timeStampSVar) )
      case VRPN.position.semantics     => Some(handlePosition(semantics, oval, timeStampSVar) )
      case VRPN.oriAndPos.semantics    => Some(handleOriAndPos(e, semantics, oval, e.getSVars(types.Position).head._2, e.getSVars(types.Orientation).head._2, timeStampSVar) )
      case VRPN.orientation.semantics  => Some(handleOrientation(semantics, oval, timeStampSVar) )
      case types.Token.semantics       => Some(handleToken(semantics, oval, timeStampSVar))
      case _ => None
    }

  /**
   * standard full trackerinfo handling
   */
  def handlePosition[T](semantics : Symbol, oval : StateParticle[T], timeStampSVar : Option[TSSVar])( msg : Any, src : Any){
    msg match{
      case msg : TrackerRemote#TrackerUpdate if Symbol(msg.sensor.toString) == semantics =>
        timeStampSVar.collect{ case t => t.set(msg.msg_time.getTime) }
        oval.set( createMat(msg.pos).asInstanceOf[T] )
      case msg : TrackerRemote#TrackerUpdate =>
      case _ => println("ERROR: handlePosition got something that was no trackerupdate: " + msg)
    }
  }

  /**
   * standard full trackerinfo handling
   */
  def handleOrientation[T](semantics : Symbol, oval : StateParticle[T], timeStampSVar : Option[TSSVar])( msg : Any, src : Any){
    msg match {
      case msg : TrackerRemote#TrackerUpdate if Symbol(msg.sensor.toString) == semantics =>
        timeStampSVar.collect{ case t => t.set(msg.msg_time.getTime) }
        oval.set( quat2Mat(msg.quat).asInstanceOf[T] )
      case msg : TrackerRemote#TrackerUpdate =>
      case _ => println("ERROR: handleOrientation got something that was no trackerupdate: " + msg)
    }
  }

  /**
   * standard full tracker info handling
   */
  def handleOriAndPos[T](
    e: Entity,
    semantics: Symbol,
    transformationSVar: StateParticle[T],
    positionSVar: StateParticle[ConstVec3],
    orientationSVar: StateParticle[ConstQuat4],
    timeStampSVar : Option[TSSVar])( msg : Any, src : Any)
  {
    msg match {
      case msg : TrackerRemote#TrackerUpdate if Symbol(msg.sensor.toString) == semantics =>
        timeStampSVar.collect{ case t => t.set(msg.msg_time.getTime) }
        
        val (position, orientation, transformation) = convertOrientationAndPosition(msg.quat, msg.pos)
        val timestamp = At(System.currentTimeMillis())
        
        transformationSVar.set(ConstMat4(transformation).asInstanceOf[T], timestamp)
        positionSVar.set(position, timestamp)
        orientationSVar.set(orientation, timestamp)

      case msg : TrackerRemote#TrackerUpdate =>
      case _ => println("ERROR: handleOriAndPos got something that was no TrackerUpdate: " + msg)
    }
  }

  /**
   *  standard button press update handling
   */
  def handleButton[T](semantics : Symbol, oval : StateParticle[T], timeStampSVar : Option[TSSVar])( msg : Any, src : Any){
    msg match{
      case msg : ButtonRemote#ButtonUpdate if Symbol(msg.button.toString) == semantics =>
        timeStampSVar.collect{ case t => t.set(msg.msg_time.getTime) }
        oval.set( (msg.state == 1).asInstanceOf[T] )
      case msg : ButtonRemote#ButtonUpdate =>
      case _ => println("ERROR: handleButton got something that was no button: " + msg)
    }
  }

  def handleToken[T](semantics : Symbol, oval : StateParticle[T], timeStampSVar : Option[TSSVar])( msg : Any, src : Any){
    msg match {
      case msg : TextReceiver#TextMessage =>
        val timestamp = msg.msg_time.getTime
        val data = Json.parseSpeechRecognitionResult(msg.msg)
        timeStampSVar.collect{ case t => t.set(timestamp) }
        oval.set( data.asInstanceOf[T], At(System.currentTimeMillis()) )
      case _ => println("ERROR: handleText got something that was no TextMessage")
    }
  }

  /**
   *  standard text update handling (untested)
   */
  def handleText[T](semantics : Symbol, oval : StateParticle[T], timeStampSVar : Option[TSSVar])( msg : Any, src : Any){
    msg match {
      case msg : TextReceiver#TextMessage =>
        timeStampSVar.collect{ case t => t.set(msg.msg_time.getTime) }
        oval.set( msg.msg.asInstanceOf[T] )
      case _ => println("ERROR: handleText got something that was no TextMessage")
    }
  }

  def handleAnalog[T](semantics : Symbol, oval : StateParticle[T], timeStampSVar : Option[TSSVar] )(msg : Any, src : Any ) {
    msg match {
      case msg : AnalogRemote#AnalogUpdate =>
        timeStampSVar.collect{ case t => t.set(msg.msg_time.getTime) }
        oval.set( ConstVec2f(msg.channel( 0 ).toFloat, msg.channel( 1 ).toFloat).asInstanceOf[T] )
      case _ =>  println("ERROR: handleAnalog got something that was not an array of analog data")
    }
  }

  /**
   *  debug output
   */
  def handleInput( msg : Any, src : Any) {
    if (debug) println(msg)
  }

  /**
   * creates an position array
   */
  def createMat( arr : Array[Double] ) : ConstMat4 =
    ConstMat4( Mat4x3.translate( Vec3(arr(0).toFloat, arr(1).toFloat, arr(2).toFloat) ) )
  /**
   *  creates a 4x4 matrix-representation of the given quaternion
   */
  private def quat2Mat[A]( quat : Array[Double] ) : ConstMat4 = {
    val (qx , qy , qz , qw)  = (quat(0).toFloat, quat(1).toFloat, quat(2).toFloat, quat(3).toFloat)
//    val (qx2, qy2, qz2, _) = (qx*qx, qy*qy, qz*qz, qw*qw)
//    ConstMat4(
//      1 - 2*qy2 - 2*qz2, 2*qx*qy - 2*qz*qw, 2*qx*qz + 2*qy*qw, 0,
//      2*qx*qy + 2*qz*qw, 1 - 2*qx2 - 2*qz2, 2*qy*qz - 2*qx*qw, 0,
//      2*qx*qz - 2*qy*qw, 2*qy*qz + 2*qx*qw, 1 - 2*qx2 - 2*qy2, 0,
//      0,                   0,                   0,                   1
//    )

    val q = ConstQuat4(qw, qx, qy, qz)
    val rotation = simplex3d.math.float.functions.rotationMat(q)
    ConstMat4(rotation)
  }
  /**
   * creates a combined matrix with quaternion and position information
   */
  private def convertOrientationAndPosition(quat : Array[Double], pos : Array[Double]): (ConstVec3, ConstQuat4, ConstMat4) = {
    val (qx , qy , qz , qw)  = (quat(0).toFloat, quat(1).toFloat, quat(2).toFloat, quat(3).toFloat)
//    val (qx2, qy2, qz2, _) = (qx*qx, qy*qy, qz*qz, qw*qw)
//    ConstMat4(
//      1 - 2*qy2 - 2*qz2, 2*qx*qy - 2*qz*qw, 2*qx*qz + 2*qy*qw, 0,
//      2*qx*qy + 2*qz*qw, 1 - 2*qx2 - 2*qz2, 2*qy*qz - 2*qx*qw, 0,
//      2*qx*qz - 2*qy*qw, 2*qy*qz + 2*qx*qw, 1 - 2*qx2 - 2*qy2, 0,
//      pos(0).toFloat,      pos(1).toFloat,      pos(2).toFloat,      1
//    )

    val orientation = ConstQuat4(qw, qx, qy, qz)
    val rotation = simplex3d.math.float.functions.rotationMat(orientation)
    val position = Vec4(pos(0).toFloat,      pos(1).toFloat,      pos(2).toFloat,      1)
    val transformation = ConstMat4(Vec4(rotation(0), 0), Vec4(rotation(1), 0), Vec4(rotation(2), 0), position)
    (position.xyz, orientation, transformation)
  }

  override protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {
    aspect.getCreateParams.semantics.Symbol match {
      case Symbols.trackingTarget.Symbol =>
        connectSVar(VRPN.oriAndPos, e, aspect)
      case Symbols.button.Symbol =>
        aspect.getCreateParams.toSValSeq.find(_.typedSemantics.getBase == types.Boolean.getBase).collect{
          case key => connectSVar(key.typedSemantics.asConvertibleTrait, e, aspect, Some(Symbols.button))
        }
      case Symbols.analogInput.Symbol =>
        connectSVar(VRPN.analog, e, aspect)
      case VRPN.text.semantics =>
        connectSVar(VRPN.text, e, aspect)
      case types.Token.semantics =>
        connectSVar(types.Token, e, aspect)
      case something =>
        println("VRPN: unsupported parameter " + something.toString)
    }
  }

  override protected def requestInitialValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect,
                                              e: Entity, given: SValSet) {
    val (retVal, remaining) = aspect.getCreateParams.combineWithValues(toProvide)
    provideInitialValues(e, aspect.getCreateParams.semantics.Symbol match {
      case Symbols.button.Symbol => retVal
      case Symbols.trackingTarget.Symbol => remaining.foldLeft(retVal){
        (set, elem) => elem match {
          case VRPN.oriAndPos => set += VRPN.oriAndPos(Mat4.Identity)
          case types.Position => set += types.Position(Vec3.Zero)
          case types.Orientation => set += types.Orientation(Quat4.Identity)
          case something      => set
        }
      }
      case Symbols.analogInput.Symbol =>
        retVal += VRPN.analog(ConstVec2f(0,0))
      case VRPN.text.semantics =>
        retVal
      case types.Token.semantics =>
        retVal
      case _ => throw new Exception
    })
  }

  private def connectSVar[T : ClassTag]( c : ConvertibleTrait[T], e : Entity, aspect : EntityAspect,
                                         typ : Option[GroundedSymbol] = None, id : Option[Symbol] = None) {
    val url = aspect.getCreateParams.getFirstValueFor(VRPN.url).getOrElse(throw new Exception("url missing"))
    val sem = id.getOrElse(aspect.getCreateParams.getFirstValueFor(VRPN.id).getOrElse(throw new Exception("sem missing")))
    val updateRate = aspect.getCreateParams.getFirstValueForOrElse(VRPN.updateRateInMillis)(16L)
    val sVarIdentifier = typ.collect{case x => x.Symbol}.getOrElse(c.semantics)
    val timeStampSVar = e.getSVars(VRPN.timestamp).headOption.map(_._2)
    VRPNFactory.createClient(sVarIdentifier, url, updateRate).collect {
      //create handle and check if this step was successful
      case client : VRPNClient => createHandle(e, sVarIdentifier, sem, e.getSVars(c.asConvertibleTrait).head._2, timeStampSVar).collect{
        //create listener and check if this step was successful
        case handle => VRPNFactory.createListener(sVarIdentifier, handle).collect {
          // subscribe
          case listener => client subscribe listener
        }.getOrElse(println("VRPN createFor: could not create handle for " + url))
      }.getOrElse(println("VRPN createFor: could not regiser listener for " + url))
    }.getOrElse(println("VRPN createFor: could not create client for " + url))
  }

  /*
  * configures the connector. this is only necessary if you want debugging output, as the create for method will
  * create new clients and listeners id necessary (when you didn't create them via configure)
  *
  * @param confParamType a List of Tuples containing the clients data type and the server url
  */
  def configure(param: List[(GroundedSymbolBase[_], String)]) {
    var listener : Option[VRPNListener] = None
    for ( (listenerSymbol, url) <- param ){
      val client = VRPNFactory.createClient(listenerSymbol, url)
      if (client.isDefined){
        //handle input (just outputs the received message)
        listener = VRPNFactory.createListener(listenerSymbol, handleInput)
        if (listener.isDefined){
          client.get.subscribe(listener.get)
        } else println("VRPN configure: could not create listener for " + url)
      } else println("VRPN configure: could not create client for " + url)
    }
  }
}
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

import simx.core.svaractor.semantictrait.base.{Thing, GroundedSymbolBase}
import vrpn.TrackerRemote
import vrpn.AnalogRemote
import vrpn.ButtonRemote
import vrpn.TextReceiver

import scala.collection.immutable.HashSet
import scala.collection.mutable
import simx.core.ontology.{types, GroundedSymbol}


/* author: dwiebusch
 * date: 23.09.2010
 */


/*
 * To implement the support for a new vrpn component, the following steps have to be taken:
 *
 * 1. create a listener class that takes the handling function as constructor argument and
 *    set it as handling function. Furthermore the class has to extend the specific (java-)
 *    vrpnListener and mix in the VRPNListener Trait (look at existing listeners for
 *    clarification)
 * 2. create a class representing the client extending extends VRPNSource[LTYPE], where LTYPE
 *    is the listener class created in step 1. Here the device (the java vrpn connection) and
 *    its subMethod (normally called addXYListener) have to be set. (as above, look for the
 *    existing classes if you don't know what to do)
 * 3. create the corresponding companion object, extending InstanciableVRPNObject and
 *    implementing the createInstance function
 * 4. register both classes at the factory by calling the addType method with 3 parameters:
 *    vrpn_tpye : Symbol, clien_class_name.getInstance(_), new ListenerName(_)
 *    see the constructor of VRPNFactory for more information
 *
 * --> don't forget to follow the steps which can be found in the VRPNConnector.scala file
 */

//supported listeners
class TrackerListener( func : (TrackerRemote#TrackerUpdate, TrackerRemote) => Unit ) extends TrackerRemote.PositionChangeListener with VRPNListener{
  //redirection
  def trackerPositionUpdate( msg : TrackerRemote#TrackerUpdate, tr : TrackerRemote ) { func(msg, tr) }
}

class ButtonListener( func : (ButtonRemote#ButtonUpdate, ButtonRemote) => Unit ) extends ButtonRemote.ButtonChangeListener with VRPNListener{
  //redirection
  def buttonUpdate( bu : ButtonRemote#ButtonUpdate, br : ButtonRemote) { func(bu, br) }
}

class TextListener ( func : (TextReceiver#TextMessage, TextReceiver) => Unit) extends TextReceiver.TextMessageListener with VRPNListener{
  //redirection
  override def receiveTextMessage( tm : TextReceiver#TextMessage, tr : TextReceiver) { func(tm, tr) }
}

class AnalogListener ( func : (AnalogRemote#AnalogUpdate, AnalogRemote ) => Unit ) extends AnalogRemote.AnalogChangeListener with VRPNListener {

  def analogUpdate ( au : AnalogRemote#AnalogUpdate, ar : AnalogRemote ) { func( au, ar ) }
}


//the specific clients. most of their functionality is specified in the parent classes
class VRPNTrackerClient(val url : String, rateInMillis : Long) extends VRPNSource[TrackerListener] {
  val device      = new TrackerRemote(url, null, null, null, null){ mainloopPeriod = rateInMillis }
  def subMethod   = device.asInstanceOf[TrackerRemote].addPositionChangeListener
  def unsubMethod = device.asInstanceOf[TrackerRemote].removePositionChangeListener _
  waitForConnection(2)
}


class VRPNButtonClient(val url : String, rateInMillis : Long) extends VRPNSource[ButtonListener]{
  val device      = new ButtonRemote (url, null, null, null, null){ mainloopPeriod = rateInMillis }
  def subMethod   = device.asInstanceOf[ButtonRemote].addButtonChangeListener
  def unsubMethod = device.asInstanceOf[ButtonRemote].removeButtonChangeListener _
  waitForConnection(2)
}

class VRPNTextReceiver(val url : String, rateInMillis : Long) extends VRPNSource[TextListener]{
  val device      = new TextReceiver( url , null, null, null, null ){ mainloopPeriod = rateInMillis }
  def subMethod   = device.asInstanceOf[TextReceiver].addTextListener
  def unsubMethod = device.asInstanceOf[TextReceiver].removeTextListener _
  waitForConnection(2)
}

class VRPNAnalogReceiver( val url : String, rateInMillis : Long ) extends VRPNSource[AnalogListener] {
  val device      = new AnalogRemote(url, null, null, null, null){ mainloopPeriod = rateInMillis }
  def subMethod   = device.asInstanceOf[AnalogRemote].addAnalogChangeListener
  def unsubMethod = device.asInstanceOf[AnalogRemote].removeAnalogChangeListener _
  waitForConnection(2)
}

//some singleton helpers, until I know how to do this shorter
object VRPNButtonClient extends InstanciableVRPNObject {
  def createInstance( url : String, rateInMillis : Long ) = new VRPNButtonClient(url, rateInMillis)
}

object VRPNTrackerClient extends InstanciableVRPNObject {
  def createInstance( url : String, rateInMillis : Long ) = new VRPNTrackerClient(url, rateInMillis)
}

object VRPNTextReceiver extends InstanciableVRPNObject {
  def createInstance( url : String, rateInMillis : Long ) = new VRPNTextReceiver(url, rateInMillis)
}

object VRPNAnalogReceiver extends InstanciableVRPNObject {
  def createInstance( url : String, rateInMillis : Long ) = new VRPNAnalogReceiver(url, rateInMillis)
}

/*!
 * factory object to create vrpn clients and listeners
 */
object VRPNFactory{
  /**
   *  map for functions to create vrpnlisteners using a given handling function
   */
  private val listenerMappings = new mutable.HashMap[GroundedSymbolBase[_], ((Any,Any) => Unit) => VRPNListener]
  /**
   *  map for functions to create clients using the given url
   */
  private val clientMappings = new mutable.HashMap[GroundedSymbolBase[_], InstanciableVRPNObject]

  /*
   * add supported types
   *
   * for each type (param1) there has to be a function (param2) that calls the appropriate constructor and
   * a function (param3) that takes another function (the actual event handling function which is passed to the
   * VRPN Listeners) and returns a VRPNListener (normally this should be a call to the Listeners constructor, passing
   * the handling function)
   */
  addType( VRPN.orientation.semantics, VRPNTrackerClient, new TrackerListener(_) ) //';
  addType( VRPN.oriAndPos.semantics, VRPNTrackerClient, new TrackerListener(_) )   //';
  addType( VRPN.position.semantics, VRPNTrackerClient, new TrackerListener(_) )    //';
  addType( VRPN.button.semantics, VRPNButtonClient, new ButtonListener(_) )        //';
  addType( VRPN.text.semantics, VRPNTextReceiver, new TextListener(_) )            //';
  addType( VRPN.analog.semantics, VRPNAnalogReceiver, new AnalogListener( _ ) )
  addType( types.Token.semantics, VRPNTextReceiver, new TextListener( _ ) )

  /**
   *  short form for adding a new client/listener pair to the maps
   */
  def addType( typ : GroundedSymbolBase[_ <: Thing],
               clientCreateFunc : InstanciableVRPNObject,
               listenerCreateFunc : ((Any,Any) => Unit) => VRPNListener) {
    clientMappings    += typ -> clientCreateFunc
    listenerMappings  += typ -> listenerCreateFunc
  }

  /*!
  * creates a new vrpn client, connected to the given url. The clients type is specified by the given symbol
  *
  * @param sym the symbol defining the created client's type
  * @param url the url the client will be connected to
  *
  * @return Some(client) if successful, None otherwise
  */
  def createClient(sym : GroundedSymbolBase[_] , url : String, rateInMillis : Long = 16L) : Option[VRPNClient] = {
    //get the factory function
    clientMappings.get(sym) match {
      //if there is none, we have to print an error
      case None => println("VRPN ERROR: Trying to create unknown typed client")
      case Some(vrpnobject) =>
        //if it exists, we try to call it and return the result
        try {
          return Some( vrpnobject.getInstance(url, rateInMillis) )
        } catch {
          //if the call fails, we have to tell so
          case _ : Throwable  => println("Unknown error while creating client of type " + sym.toString + " with connection to " + url )
        }
    }
    //if we got here, there was a problem and therefore we have to return None
    None
  }

  /*!
  * creates a new listener, using the given handling function. The listeners type is specified by the given symbol
  *
  * @param sym the symbol defining the created listener's type
  * @param func the function handling the incoming events
  *
  * @return Some(listener) if successful, None otherwise
  */
  def createListener(sym : GroundedSymbolBase[_], func : (Any, Any) => Unit) : Option[VRPNListener] =
    listenerMappings.get(sym) collect { case function => function(func) }

  /*!
  * removes all instantiated objects and removes connections, listeners and so on.
  * This should be called at the end of the program, to ensure all connections to the
  * vrpn server(s) are closed
  */
  def cleanup() { for ( client <- clientMappings ) client._2.cleanup() }
}

/*!
 * basic functionality of a vrpn client
 */
abstract class VRPNClient{
  /**
   * does all the necessary stuff to register a VRPNListener
   */
  def subscribe( a : VRPNListener)
  /**
   *  for now does nothing
   */
  def unsubscribe( a : VRPNListener)
  /**
   *  disconnects all listeners
   */
  def cleanup()
}

/**
 *  there will be listeners...
 */
trait VRPNListener

//class to create instances of different clients
trait InstanciableVRPNObject{
  /**
   *  this should be the constructor of the overriding vrpn client
   */
  protected def createInstance(url : String, rateInMillis : Long) : VRPNClient
  /**
   *  holding instance for different urls
   */
  private val instances = new mutable.HashMap[String, VRPNClient]
  /**
   *  create an new instance, if there is none yet
   */
  def getInstance(url : String, rateInMillis : Long) : VRPNClient =
  //check for a client's existence and return it or ...
    instances.getOrElse(url, {
      // ... create a new one and register it. return it then by tail recursion (may be not the nicest way, but looks nicer than val retVal = ... ; return retVal)
      instances.update(url, createInstance(url, rateInMillis))
      getInstance(url, rateInMillis)
    })

  /**
   *  removes all instances
   */
  def cleanup() {
    for ( instance <- instances ) instance._2.cleanup()
    instances.clear()
  }
}

//generic methods for vrpn clients
trait VRPNSource[A <: VRPNListener] extends VRPNClient{
  /**
   *  registered listeners
   */
  private var listeners : Set[VRPNListener] = new HashSet
  /**
   *  internal variable to see if device was already stopped (as the vrpn device's isConnected shows some strange behavior)
   */
  private var deviceStopped = false
  /**
   *  the underlying device
   */
  val device : vrpn.VRPNDevice
  /**
   *  the subscribe method which must be called to receive updates
   */
  def subscribe( a : VRPNListener ) {
    listeners += a.asInstanceOf[A]
    subMethod(a.asInstanceOf[A])
  }

  /**
   *  wait timeout seconds for a successful connection, after that cancel the connection
   */
  def waitForConnection( timeout : Long ) {
    println("trying to connect to: " + device.getConnectionName)
    var counter = 0
    while (counter < timeout*4 && !device.isConnected){
      Thread.sleep(250)
      counter +=1
    }
    if (!device.isConnected){
      println("\tresult: could not connect to " +device.getConnectionName)
      device.stopRunning()
      deviceStopped = true
    } else
      println("\tresult: connected to " + device.getConnectionName)
  }

  /**
   *  unsubscribing. removing listeners
   */
  def unsubscribe( a : VRPNListener ) {
    listeners -= a.asInstanceOf[A]
    unsubMethod(a.asInstanceOf[A])
  }

  /**
   *  unsubscribes all listeners
   */
  def cleanup() {
    while (listeners.nonEmpty) unsubscribe(listeners.head)
    if (!deviceStopped)
      device.stopRunning()
    deviceStopped = true
  }

  //! the function which registers the specific type of listener
  protected def subMethod : A => Unit
  protected def unsubMethod : A => Unit
}

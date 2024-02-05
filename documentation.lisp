(in-package #:org.shirakumo.fraf.gog-galaxy)

;; conditions.lisp
(docs:define-docs
  (type gog-error
    "Error signalled when an API call fails for some reason.

See NAME
See MESSAGE
See KIND
See GOG-ERROR (function)
See CHECK-ERROR")
  
  (function gog-error
    "Signal a GOG-ERROR.

See GOG-ERROR (type)")
  
  (function name
    "Returns the name of the error type (if any).

See GOG-ERROR (type)")
  
  (function message
    "Returns the error message (if any).

See GOG-ERROR (type)")
  
  (function kind
    "Returns the error type (if any).

See GOG-ERROR (type)")
  
  (function check-error
    "Checks whether the underlying GOG Galaxy API encountered an
    error.

If an error was encountered, signals a GOG-ERROR, otherwise returns
VALUE. This will \"catch\" any error since the last time CHECK-ERROR
was called.

See GOG-ERROR (type)"))

;; c-object.lisp
(docs:define-docs
  (type c-object
    "Encapsulates a foreign-allocated object.

See HANDLE
See FREE")
  
  (function handle
    "Returns the handle (pointer) of the object.

If the object is no longer valid returns NIL.

See C-OBJECT (type)")
  
  (function free
    "Frees the underlying handle of the object.

See HANDLE
See C-OBJECT (type)")
  
  (type interface
    "Encapsulates a GOG Galaxy API interface object.

Interface instances are all singletons.

See C-OBJECT (type)
See INTERFACE")
  
  (function interface
    "Returns the singleton interface object of the given name.

See INTERFACE (type)"))

;; id.lisp
(docs:define-docs
  (type id-registered-object
    "Encapsulates an API object represented by an ID.

See ID")
  
  (function id
    "Returns the ID of the object.

See ID-REGISTERED-OBJECT (type)")
  
  (type user
    "Representation of a user on the GOG Galaxy API.

See ENSURE-USER
See ID-REGISTERED-OBJECT (type)")
  
  (function ensure-user
    "Coerces the ID to a USER instance.

ID may be one of:

  INTEGER --- A singleton instance corresponding to this ID is
              returned. If the ID is not a user-id, an error is
              signalled.
  USER    --- Returns the user again.
  T       --- Returns the user corresponding to the one currently
              logged in to the API.
  STRING  --- Tries to find a user with the given name. See
              FIND-USER.

See USER (type)
See FIND-USER")
  
  (type lobby
    "Representation of a lobby on the GOG Galaxy API.

See ENSURE-LOBBY
See ID-REGISTERED-OBJECT (type)")
  
  (function ensure-lobby
    "Coerces the ID to a LOBBY instance.

ID may be one of:

  INTEGER --- A singleton instance corresponding to this ID is
              returned. If the ID is not a lobby-id, an error is
              signalled.
  USER    --- Returns the lobby again.

See LOBBY (type)"))

;; listener.lisp
(docs:define-docs
  (variable *listener-timeout*
    "Specifies the timeout for synchronous API calls in seconds.

Defaults to 10.

See WITH-LISTENER*")
  
  (type listener
    "Representation of an event listener instance.

Handlers can either be local to an operation if they can be passed as
an argument to that operation, or global (the default). Each handler
must specify which events to listen for. You can do so either by
explicitly calling REGISTER-FOR, or passing :REGISTER-FOR as an
initarg to an instance.

You can also dynamically unregister from events by calling
UNREGISTER-FROM.

Each event corresponds to a generic function that is called on the
listener, which you should create methods for to handle the event. If
no method to handle the event exists, a SIMPLE-WARNING is signalled
and a message is printed to *ERROR-OUTPUT*.

When a listener is FREEd it is automatically unregistered first.

See DEFINE-GLOBAL-LISTENER
See WITH-LISTENER
See REGISTER-FOR
See UNREGISTER-FROM
See C-OBJECT (type)")
  
  (function register-for
    "Register a listener for a specified event type.

EVENT can be an event symbol or a list of event symbols. If the
listener is already registered for the event, nothing happens.

See LISTENER (type)")
  
  (function unregister-from
    "Deregisters a listener from a specified event type.

EVENT can be an event symbol, a list of event symbols, or T for all
registered events. If the listener is not currently listening for the
event, nothing happens.

See LISTENER (type)")
  
  (type global-listener
    "A globally registered listener.

You should use DEFINE-GLOBAL-LISTENER to define subclasses. A
global-listener is instantiated and registered automatically on INIT
and freed again on SHUTDOWN.

See LISTENER (type)
See DEFINE-GLOBAL-LISTENER")
  
  (function define-global-listener
    "Defines a global listener.

TYPES should be an event type, list of event types, or T for all
events, that the listener should register itself for.

HANDLERS should be body forms of the following structure:

  (HANDLER-NAME ARGS . FORMS)

The HANDLER-NAME should be the name of an event handling function.
ARGS should be the argument list for the event handling function
excluding the first argument. FORMS should be forms evaluated when the
event is handled. During evaluation of FORMS the symbol LISTENER is
bound to the listener instance.

See GLOBAL-LISTENER (type)")
  
  (type dynamic-listener
    "A dynamically established listener.

You should use WITH-LISTENER or WITH-LISTENER* to establish such a
listener in a dynamic extent.

See LISTENER (type)
See WITH-LISTENER")
  
  (function with-listener
    "Creates a dynamic listener instance.

THUNK is a form to be evaluated for which LISTENER is bound to a
DYNAMIC-LISTENER instance with established HANDLERS.

HANDLERS should be body forms of the following structure:

  (HANDLER-NAME ARGS . FORMS)

The HANDLER-NAME should be the name of an event handling function.
ARGS should be the argument list for the event handling function
excluding the first argument. FORMS should be forms evaluated when the
event is handled. During evaluation of FORMS the block with the name
of LISTENER is established to allow easy returning.

See DYNAMIC-LISTENER (type)
See WITH-LISTENER*")
  
  (function with-listener*
    "Creates a dynamic listener instance and blocks.

After THUNK has been executed, this form blocks by repeatedly calling
PROCESS-DATA until TIMEOUT seconds have passed. You should cause an
exit from the listener block when the handler is done, or signal an
error in case of failure.

This effectively turns an asynchronous API call into a synchronous
one.

See *LISTENER-TIMEOUT*
See DYNAMIC-LISTENER (type)
See WITH-LISTENER"))

;; wrapper.lisp
(docs:define-docs
  (function init
    "Initializes the API.

This sets up the GOG API and establishes global handlers.
If successful, returns true, otherwise NIL. Unless this function
returns success, you may not use any actual API functions.

See SHUTDOWN
See INITIALIZED-P")
  
  (function shutdown
    "Shuts the API down and cleans things up.

This is safe to call multiple times.
After calling shutdown, all API objects are invalidated.

See INIT
See INITIALIZED-P")
  
  (function initialized-p
    "Returns true if the API was initialised successfully and is ready for use.

See INIT
See SHUTDOWN")
  
  (function process-data
    "Give the GOG Galaxy API some time to process events.

You should regularly call this function if initialisation was
successful. Calling this function will also cause events to be
delivered and event handlers to be called synchronously.

See INIT
See LISTENER (type)"))

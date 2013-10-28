---
layout : default
title : API - GLFW 3+ Binding
packages :
  - GLFW (3+)
weight: 13
---

# The GLFW 3+ Binding

This is a binding for the [GLFW][3] library, version 3 and above.
It is feature-complete (as of GLFW 3.0.3), meaning that it wraps
all functionality available in the GLFW API.

Before using any GLFW functionality, you have to call `GLFW.Init`.
When you're done with GLFW, you should call `GLFW.Shutdown` (this
is named *terminate* in the C API, but as that is a keyword in Ada,
it has been renamed).

## `GLFW.Windows`

The major difference between versions 2.x and 3.x of GLFW is that
the latter supports multiple windows. The majority of the GLFW
API functions need a window handle as parameter now.

The binding exposes this handle as tagged type `Window`. Its
whole interface is object oriented. In order to create a window
on your own, you should derive from the `Window` type, override
any callback functions you want to handle, and enable those
callbacks. As an example, here is some code that defines a new
window type which closes itself when the user presses `Escape`:

<?prettify lang=ada?>

    type My_Window is new Glfw.Windows.Window with null record;
    
    overriding
    procedure Init (Object : not null access My_Window;
                    Width, Height : Glfw.Size;
                    Title   : String;
                    Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                    Share   : access Glfw.Windows.Window'Class := null);
    
    overriding
    procedure Key_Changed (Object   : not null access My_Window;
                           Key      : Glfw.Input.Keys.Key;
                           Scancode : Glfw.Input.Keys.Scancode;
                           Action   : Glfw.Input.Keys.Action;
                           Mods     : Glfw.Input.Keys.Modifiers);
    
    procedure Init (Object : not null access My_Window;
                    Width, Height : Glfw.Size;
                    Title   : String;
                    Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                    Share   : access Glfw.Windows.Window'Class := null) is
       Upcast : Glfw.Windows.Window_Reference
         := Glfw.Windows.Window (Object.all)'Access;
    begin
       Upcast.Init (Width, Height, Title, Monitor, Share);
       Object.Enable_Callback (Glfw.Windows.Callbacks.Key);
    end Init;
    
    procedure Key_Changed (Object   : not null access My_Window;
                           Key      : Glfw.Input.Keys.Key;
                           Scancode : Glfw.Input.Keys.Scancode;
                           Action   : Glfw.Input.Keys.Action;
                           Mods     : Glfw.Input.Keys.Modifiers) is
       use type Glfw.Input.Keys.Key;
    begin
       If Key = Glfw.Input.Keys.Escape then
          Object.Set_Should_Close (True);
       end if;
    end Key_Changed;

The package `GLFW.Windows` has three child packages:

 * `Context` provides functionality related to the OpenGL
   context on the window, like switching front and back buffers.
 * `Hints` allows the user to set parameters for windows
   that will be created afterwards. Windows get created by
   calling the `Init` function, which has to be done before
   using the window in any way.
 * `Clipboard` for accessing the operating system's clipboard.

## `GLFW.Monitors`

Query information about the available monitors. You can create
a fullscreen window by passing a monitor reference to its `Init`
procedure.

## `GLFW.Input`

Provides the polling procedures `Poll_Events` and `Wait_For_Events`.
The child packages `Mouse` and `Keys` provide the types needed for
mouse and keyboard interaction - as the interaction itself is bound
to an GLFW window, the event handlers and query functions for the
states of these devices have moved to `GLFW.Windows`.

`GLFW.Joysticks` is still independent of the window, because
joystick input is global.

## `GLFW.Errors`

GLFW 3 introduced an error callback mechanism to be able to better
track down errors. You can register an error callback before
the obligatory call to `GLFW.Init`.

 [3]: http://www.glfw.org/
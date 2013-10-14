## Zephyros - Racket API

#### Setup

* (require "path/to/zephyros.rkt")

The routines are almost consistent with the Zephyros names - underscores are replaced with hyphens so the routines look scheme-like.

#### Top-Level Routines

```racket
   (clipboard-contents)
   (focused-window)
   (visible-windows)
   (all-windows)
   (main-screen)
   (all-screens)
   (running-apps)
   (choose-from lst title lines-tall chars-wide)
   (alert msg duration)
   (log msg)
   (show-box msg)
   (hide-box)
   (update-settings k-v)
   (undo)
   (redo)
```

#### Window routines

```racket
   (title window-id)
   (frame window-id)
   (top-left window-id)
   (size window-id)
   (app window-id)
   (screen window-id)
   (focus-window window-id)
   (windows-to-north window-id)
   (windows-to-south window-id)
   (windows-to-east window-id)
   (windows-to-west window-id)
   (normal-window? window-id)
   (minimized? window-id)
   (other-windows-on-same-screen window-id)
   (other-windows-on-all-screens window-id)
   (set-frame window-id dim-hash)
   (set-top-left window-id x-y-hash)
   (set-size window-id w-h-hash)
   (maximize window-id)
   (minimize window-id)
   (un-minimize window-id)
   (focus-window-left window-id)
   (focus-window-right window-id)
   (focus-window-up window-id)
   (focus-window-down window-id)
```

#### App routines

```racket
   (hidden? app-id)
   (show app-id)
   (hide app-id)
   (kill app-id)
   (kill9 app-id)
   (app/visible-windows app-id)
   (app/all-windows app-id)
   (app/title app-id)
   (app/hidden? app-id)
```

#### Screen routines

```racket
   (frame-including-dock-and-menu screen-id)
   (frame-without-dock-or-menu screen-id)
   (previous-screen screen-id)
   (next-screen screen-id)
   (rotate-to screen-id degree) - must be a right angle
```

Listeners are defined using <code>on-<event></code>. They expect a function to be passed as an argument. This function is invoked with the value as an argument.

```racket
   (on-app-died f)
   (on-app-hidden f)
   (on-app-launched f)
   (on-app-shown f)
   (on-focus-changed f)
   (on-modifiers-changed f)
   (on-mouse-moved f)
   (on-screens-changed f)
   (on-window-created f)
   (on-window-minimized f)
   (on-window-moved f)
   (on-window-resized f)
   (on-window-unminiized f)
```

There's an unlistener corresponding to every listener:

```racket
   (unlisten-app-died)
   (unlisten-app-hidden)
   (unlisten-app-launched)
   (unlisten-app-shown)
   (unlisten-focus-changed)
   (unlisten-modifiers-changed)
   (unlisten-mouse-moved)
   (unlisten-screens-changed)
   (unlisten-window-created)
   (unlisten-window-minimized)
   (unlisten-window-moved)
   (unlisten-window-resized)
   (unlisten-window-unminiized)

```


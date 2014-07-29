register-channel
================

Register Channel is an emacs package that let you move around buffer like switching TV channels.

To use, put this in your init file:

    (require 'register-channel)
    (register-channel-default-keybindings)

Then you can use `M-g M-1` to put point position into register `1`. This works for register `1` to `5`; `6` to `8` by default hold window
configurations. To utilize these registers, use `M-1` etc. The old
position / window configuration are automatically stored into
register `~`, so you can easily go back with `M-~`.

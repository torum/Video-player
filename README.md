# Video-player

Currently, **under development**.

Yet another [mpv](https://mpv.io/) frontend based on [LibMPVDelphi](https://github.com/nbuyer/libmpvdelphi).  
Even though Video-player utilize the libmpv library to play videos, it does not use any other functionalities of mpv such as scripts or on-screen controls. It just simply plays videos quickly and easily without being overwhelemed by vast number of options. If you want full control and customization, this player isn't for you.  

## Current progress

[x] Open single or multiple video files via command line parameters to make a playlist. (this includes launching from sendto:)  
[x] Open single or multiple video files via drag and drop of file or folder onto the app window to make a playlist.  
[x] Fullscreen viewing.  
[x] Basic keyboard command including,  
```
Space/Pause/P => Play or Pause 
Right => Skip 10 sec  
Left => Go back 10 sec  
Shift+Right => Next video  
Shift+Left => Previous video 
Ctrl+Right => Skip 100 sec
Ctrl+Left => Go back 100 sec 
Up => Volume up by 5 
Down => Volume down by 5 
Ctrl+Up => Volume up by 10 
Ctrl+Down => Volume down by 10 
F => Full screen on/off 
Escape/F11 => Full screen off 
Q => App quit 
```
[x] Basic mouse control including,  
```
Drag on Volume bar
Drag on Seek bar
Wheel up/down on Volume bar    
Wheel up/down on Seek bar   
```

 ## TODO:
[ ] More visual control buttons.  
[ ] Right click popup menu.  
[ ] Commandline options.


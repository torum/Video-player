# Video-player

**work in progress**.

Yet another [mpv](https://mpv.io/) frontend based on [LibMPVDelphi](https://github.com/nbuyer/libmpvdelphi) for Windows.  
Even though Video-player utilize the libmpv library to play videos, it does not use any other functionalities of mpv such as scripts or on-screen controls. It just simply plays videos quickly and easily without being overwhelemed by vast number of options. If you want full control and customization, this player isn't for you.  

## Current progress

[x] Open single or multiple video files via command line parameters. (this includes launching from Explorer and shell:sendto)  
[x] Open single or multiple video files via drag and drop of file or folder onto the app window.  
[x] Fullscreen viewing.  
[x] Basic keyboard command including,  
```
Space/Pause/P => Play or Pause 
Right => Skip 10 sec  
Left => Go back 10 sec  
Ctrl+Right => Skip 100 sec
Ctrl+Left => Go back 100 sec
Shift+Right => Next video  
Shift+Left => Previous video 
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
Wheel up/down on Volume bar or video screen   
Wheel up/down on Seek bar   
```
[x] On-screen button controls including play/pause, next vieo, previous video.

 ## TODO:
[x] Right click popup menu "Stay on top", "Repeat", "Single" menu.  
[ ] More options.   
[ ] Commandline options.  
[ ] Linux version. 

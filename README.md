# Video-player

Simple and minimum, yet configurable [mpv](https://mpv.io/) frontend based on [LibMPVDelphi](https://github.com/nbuyer/libmpvdelphi).

Under development.

Even though Video-player ueses libmpv library component from mpv project to play videos, it does not follow almost any shortcut keys or resuse config nor scripts or anything. It simply just plays video quickly, simply and easily without any distraction.

##Current progress

[x] Open single or multiple video files via command line parameters to make a playlist. (this includes launching from sendto: shortcut menu in explorer)  
[x] Open single or multiple video files via folder dorag and drop onto the app window to make a playlist.  
[x] Fullscreen viewing.  
[x] Basic keyboard command including,  
```
Space/Pause/P => Play or Pause 
Right => Skip 10 sec 
Left => Go back 10 sec 
Shift+Right => Skip 100 sec 
Shift+Left => Go back 100 sec 
Ctrl+Right => Next video 
Ctrl+Left => Previous video 
Up => Volume up by 5 
Down => Volume down by 5 
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
Wheele down => Volume down  
Wheele up => Volume up  
Right click => show on screen/visual controls.  
```


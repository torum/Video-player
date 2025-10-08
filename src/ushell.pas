unit UShell;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ComCtrls, BCImageButton, BCFluentSlider, BGRAFlashProgressBar,
  BCRadialProgressBar, BCButton, BCSVGButton, BGRAImageList, BCRoundedImage,
  LCLType, ExtCtrls,
  {$ifdef windows}Windows, DWMApi, windirs, win32titlestyler,{$endif}
  MPVBasePlayer, Types;

type

  { TfrmShell }

  TfrmShell = class(TForm)
    IdleTimerOverlayControlsHide: TIdleTimer;
    Panel1: TPanel;
    LabelTimeFormatted: TLabel;
    RoundedImage: TBCRoundedImage;
    RoundedImageNext: TBCRoundedImage;
    RoundedImagePrev: TBCRoundedImage;
    SliderSeek: TBCFluentSlider;
    TrackBarVolume: TBCFluentSlider;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerOverlayControlsHideStopTimer(Sender: TObject);
    procedure IdleTimerOverlayControlsHideTimer(Sender: TObject);
    procedure RoundedImageClick(Sender: TObject);
    procedure RoundedImageMouseEnter(Sender: TObject);
    procedure RoundedImageMouseLeave(Sender: TObject);
    procedure RoundedImageNextClick(Sender: TObject);
    procedure RoundedImagePrevClick(Sender: TObject);
    procedure SliderSeekChangeValue(Sender: TObject);
    procedure SliderSeekMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SliderSeekMouseEnter(Sender: TObject);
    procedure SliderSeekMouseLeave(Sender: TObject);
    procedure SliderSeekMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SliderSeekMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TrackBarVolumeChangeValue(Sender: TObject);
    procedure TrackBarVolumeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBarVolumeMouseEnter(Sender: TObject);
    procedure TrackBarVolumeMouseLeave(Sender: TObject);
    procedure TrackBarVolumeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBarVolumeMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FPos: TPoint;
    FisMouseDown: boolean;
    FisMoving: boolean;
    FblnTrackBarVolumeChanging:boolean; // Change volume using TrackBar manually.
    FblnTrackBarVolumeUpdating:boolean; // Update volume value from player.
    FblnSliderSeekChanging:boolean; // Change seek using TrackBar manually.
    FblnSliderSeekUpdating:boolean; // Update seek position value from player.
    FblnInhibitIdleHideControls: boolean;
  public
    procedure ChangeVolumeWithMouseWheel(WheelDelta: Integer);
    Property IsTrackBarVolumeChanging:boolean read FblnTrackBarVolumeChanging;
    Property IsTrackBarVolumeUpdating:boolean read FblnTrackBarVolumeUpdating write FblnTrackBarVolumeUpdating;
    Property IsSliderSeekChanging:boolean read FblnSliderSeekChanging;
    Property IsSliderSeekUpdating:boolean read FblnSliderSeekUpdating write FblnSliderSeekUpdating;
    Property IsInhibitIdleHideControls:boolean read FblnInhibitIdleHideControls;
  end;

var
  frmShell: TfrmShell;

implementation

uses
  UMain;

{$R *.lfm}

{ TfrmShell }

procedure TfrmShell.FormCreate(Sender: TObject);
begin
  Panel1.Color := clBlack;
  Panel1.Repaint;

  RoundedImage.Picture.LoadFromResourceName(Hinstance,'IC_FLUENT_PLAY_CIRCLE_24_FILLED'); //IC_FLUENT_PAUSE_CIRCLE_24_FILLED
  RoundedImageNext.Picture.LoadFromResourceName(Hinstance,'IC_FLUENT_ARROW_CIRCLE_RIGHT_24_FILLED');
  RoundedImagePrev.Picture.LoadFromResourceName(Hinstance,'IC_FLUENT_ARROW_CIRCLE_LEFT_24_FILLED');
end;

procedure TfrmShell.FormShow(Sender: TObject);
begin

  // This code block uses winapi to set lyaered window with background transparent.
  // However, the edges of the controls are really really ugry.
  {
  self.Color:=clBlack;
  SetWindowLongPtr(Self.Handle, GWL_EXSTYLE, GetWindowLongPtr(Self.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Self.Handle, clBlack, 0, LWA_COLORKEY); // Transparent with black
  }

  // This one sets the whole window transparent.
  // However, this is easly archived with alpha blend with more controlability.
  //SetLayeredWindowAttributes(Self.Handle, 0, 90, LWA_ALPHA); // Semi-transparent

  //SetWindowPos(Self.Handle, HWND_TOPMOST, self.Left, self.Top, self.Width, self.Height, SWP_NOSIZE);

  Panel1.Repaint;

  FblnInhibitIdleHideControls:=false;
  IdleTimerOverlayControlsHide.Enabled:=true;
end;

procedure TfrmShell.IdleTimerOverlayControlsHideStopTimer(Sender: TObject);
begin
  //FblnInhibitIdleHideControls:=false;
end;

procedure TfrmShell.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  frmMain.FormDropFiles(self,FileNames);
end;

procedure TfrmShell.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Button1 stole all the key down events..
  frmMain.DebugOutput('TfrmShell.FormKeyDown');
  frmMain.FormKeyDown(self,Key,Shift);
end;

procedure TfrmShell.FormDblClick(Sender: TObject);
begin
  // this is needed for mouse drag window to work correctly.
  FisMoving:=false;
  FisMouseDown:=false;

  if (frmMain.IsFullscreen) then
  begin
    frmMain.ShowFullScreen(false);
  end else
  begin
    frmMain.ShowFullScreen(true);
  end;
end;

procedure TfrmShell.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: begin
      FPos.X:=X;
      FPos.Y:=Y;
      FisMouseDown:=true;
    end;
    mbRight: begin
      //  Right click show controls.
      frmMain.ShowOverlayControls();
    end;
  end;
end;

procedure TfrmShell.FormMouseEnter(Sender: TObject);
begin
  //frmMain.DebugOutput('FormMouseEnter');
  //FblnInhibitIdleHideControls:=true;
end;

procedure TfrmShell.FormMouseLeave(Sender: TObject);
begin
  //frmMain.DebugOutput('FormMouseLeave');
  FblnInhibitIdleHideControls:=false;
end;

procedure TfrmShell.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ((not FisMouseDown) and (not FisMoving)) then
  begin
    FblnInhibitIdleHideControls:=false;
    frmMain.ShowOverlayControls();
  end;

  if (frmMain.IsFullscreen) then exit;

  if (FisMouseDown) then
  begin
    FisMoving:=true;
    frmMain.Left:=frmMain.Left+X-FPos.X;
    frmMain.Top:=frmMain.Top+Y-FPos.Y;
  end;
end;

procedure TfrmShell.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FisMouseDown:=false;
  FisMoving:=False;
end;

procedure TfrmShell.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := true;
  ChangeVolumeWithMouseWheel(WheelDelta);
end;

procedure TfrmShell.ChangeVolumeWithMouseWheel(WheelDelta: Integer);
var
  curVol:double;
  newVol:integer;
begin
  // Gets current volume.
  curVol := frmMain.Player.GetVolume;

  //newVol := WheelDelta div 10;
  // testing...
  //newVol := newVol div 2;
  newVol := 0;

  if (WheelDelta < 0) then
  begin
    newVol := -6;
    if (curVol <= abs(newVol)) then
    begin
      frmMain.SetVolume(0);
    end else begin
      frmMain.SetVolume(Trunc(curVol)+newVol);
    end;
  end else
  begin
    newVol := 6;
    newVol:=Trunc(curVol)+newVol;
    if (1000 <= newVol) then
    begin
      frmMain.SetVolume(1000);
    end else begin
      frmMain.SetVolume(newVol);
    end;
  end;

  if (self.AlphaBlendValue <> 0) then
  begin
    IdleTimerOverlayControlsHide.Enabled:=false;
    IdleTimerOverlayControlsHide.Enabled:=true;
  end;
end;

procedure TfrmShell.IdleTimerOverlayControlsHideTimer(Sender: TObject);
begin
  //frmMain.DebugOutput('Timer');
  if (frmMain.IsPopupVisible) then
  begin
    IdleTimerOverlayControlsHide.Enabled:=false;
    IdleTimerOverlayControlsHide.Enabled:=true;
  end else
  begin
    frmMain.HideOverlayControls;
  end;
end;

procedure TfrmShell.Button1Click(Sender: TObject);
begin
  //Outputdebugstring(pchar('TfrmShell.Button1Click'));
end;

procedure TfrmShell.Button1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Somehow this button steals all the key down events...
  frmMain.DebugOutput('TfrmShell.Button1KeyDown');
  frmMain.FormKeyDown(self,Key,Shift);
end;

procedure TfrmShell.RoundedImageClick(Sender: TObject);
var
  plState:TMPVPlayerState;
begin
  // Check player status.
  plState := frmMain.Player.GetState;

  if (plState = TMPVPlayerState.mpsPlay) then
  begin
    frmMain.Player.Pause;
  end
  else if (plState = TMPVPlayerState.mpsPause) then
  begin
    frmMain.Player.Resume;
  end
  else if (plState = TMPVPlayerState.mpsStop) then // TODO: need to check.
  begin
     // do nothing. probably closing.
    frmMain.DebugOutput('TMPVPlayerState.mpsStop @RoundedImageClick');
  end
  else if (plState = TMPVPlayerState.mpsEnd) then
  begin
     // TODO: mpsEnd is also fired when closing? need to check.
    frmMain.DebugOutput('TMPVPlayerState.mpsEnd @RoundedImageClick');

    // restart
    frmMain.ReStartVideo;
  end;

  {
  else if (plState = TMPVPlayerState.mpsUnk) then
  begin
    outputdebugstring(pchar('TMPVPlayerState.mpsUnk'));
  end
  else if (plState = TMPVPlayerState.mpsErr) then
  begin
    outputdebugstring(pchar('TMPVPlayerState.mpsErr'));
  end
  else if (plState = TMPVPlayerState.mpsStep) then
  begin
    outputdebugstring(pchar('TMPVPlayerState.mpsStep'));
  end else if (plState = TMPVPlayerState.mpsLoading) then
  begin
    outputdebugstring(pchar('TMPVPlayerState.mpsLoading'));
  end else
  begin
    outputdebugstring(pchar('TMPVPlayerState else'));
  end;
  }
end;

procedure TfrmShell.RoundedImageMouseEnter(Sender: TObject);
begin
  FblnInhibitIdleHideControls:=true;
end;

procedure TfrmShell.RoundedImageMouseLeave(Sender: TObject);
begin
  FblnInhibitIdleHideControls:=false;
end;

procedure TfrmShell.RoundedImageNextClick(Sender: TObject);
begin
  frmMain.LoadNextVideo;
end;

procedure TfrmShell.RoundedImagePrevClick(Sender: TObject);
begin
  frmMain.LoadPrevVideo
end;

procedure TfrmShell.TrackBarVolumeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FblnTrackBarVolumeChanging:=true;
end;

procedure TfrmShell.TrackBarVolumeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FblnTrackBarVolumeChanging:=false;
end;

procedure TfrmShell.TrackBarVolumeMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := true;
  ChangeVolumeWithMouseWheel(WheelDelta);
end;

procedure TfrmShell.TrackBarVolumeMouseEnter(Sender: TObject);
begin
  FblnInhibitIdleHideControls:=true;
end;

procedure TfrmShell.TrackBarVolumeMouseLeave(Sender: TObject);
begin
  FblnInhibitIdleHideControls:=false;
end;

procedure TfrmShell.TrackBarVolumeChangeValue(Sender: TObject);
begin
  if (not FblnTrackBarVolumeUpdating) then begin
    frmMain.SetVolume(TrackBarVolume.Value);
  end;
end;

procedure TfrmShell.SliderSeekMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FblnSliderSeekChanging:=true;
end;

procedure TfrmShell.SliderSeekMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FblnSliderSeekChanging:=false;
end;

procedure TfrmShell.SliderSeekMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  newValue:integer;
begin
  Handled := true;
  FblnSliderSeekChanging:=true;

  if (WheelDelta < 0) then
  begin
    newValue := -2;
    if (SliderSeek.Value <= abs(newValue)) then
    begin
      SliderSeek.Value := 0;
    end else begin
      SliderSeek.Value := SliderSeek.Value + newValue;
    end;
  end else
  begin
    newValue := SliderSeek.Value + 1;
    if (SliderSeek.MaxValue >= (newValue)) then
    begin
      //SliderSeek.Value := newPos; // not good when playing.
      frmMain.Player.Seek(1,true);
    end;
  end;
  FblnSliderSeekChanging:=false;

  if (self.AlphaBlendValue <> 0) then
  begin
    IdleTimerOverlayControlsHide.Enabled:=false;
    IdleTimerOverlayControlsHide.Enabled:=true;
  end;
end;

procedure TfrmShell.SliderSeekMouseEnter(Sender: TObject);
begin
  FblnInhibitIdleHideControls:=true;
end;

procedure TfrmShell.SliderSeekMouseLeave(Sender: TObject);
begin
  FblnInhibitIdleHideControls:=false;
end;

procedure TfrmShell.SliderSeekChangeValue(Sender: TObject);
var
  newPos:double;
begin
  if (IsSliderSeekUpdating) then exit;
  if (SliderSeek.MaxValue = 0) then exit;
  if (SliderSeek.MaxValue < SliderSeek.Value) then exit;

  if (frmMain.Player <> nil) then
  begin
    try
      newPos:=SliderSeek.Value div 10;
    except
      exit;
    end;
    frmMain.Player.Seek(newPos, false);
  end;
end;

procedure TfrmShell.FormDestroy(Sender: TObject);
begin

end;



end.


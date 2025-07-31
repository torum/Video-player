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
    RoundedImage: TBCRoundedImage;
    LabelTimeFormatted: TLabel;
    SliderSeek: TBCFluentSlider;
    TrackBarVolume: TBCFluentSlider;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerOverlayControlsHideStartTimer(Sender: TObject);
    procedure IdleTimerOverlayControlsHideStopTimer(Sender: TObject);
    procedure IdleTimerOverlayControlsHideTimer(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure RoundedImageClick(Sender: TObject);
    procedure TrackBarVolumeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FPos: TPoint;
    FisMouseDown: boolean;
    FisMoving: boolean;
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

  //self.Color:=clBlack;

  RoundedImage.Picture.LoadFromResourceName(Hinstance,'FLUENT_PLAY_CIRCLE_48_FILLED');
  //Image1.Picture.LoadFromResourceName(Hinstance,'48_TRANSPARENT');
end;

procedure TfrmShell.Button1Click(Sender: TObject);
begin
  //Outputdebugstring(pchar('TfrmShell.Button1Click'));
end;

procedure TfrmShell.Button1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Somehow this button steals all the key down events...
  //Outputdebugstring(pchar('TfrmShell.Button1KeyDown'));
  frmMain.FormKeyDown(self,Key,Shift);
end;

procedure TfrmShell.FormShow(Sender: TObject);
begin
  //self.Color:=clBlack;
  //SetWindowLongPtr(Self.Handle, GWL_EXSTYLE, GetWindowLongPtr(Self.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
  //SetLayeredWindowAttributes(Self.Handle, clBlack, 0, LWA_COLORKEY); // Transparent with black
  //SetLayeredWindowAttributes(Self.Handle, 0, 90, LWA_ALPHA); // Semi-transparent

  //SetWindowPos(Self.Handle, HWND_TOPMOST, self.Left, self.Top, self.Width, self.Height, SWP_NOSIZE);
  IdleTimerOverlayControlsHide.Enabled:=true;
end;

procedure TfrmShell.IdleTimerOverlayControlsHideStartTimer(Sender: TObject);
begin
  //frmMain.DebugOutput('StartTimer');
end;

procedure TfrmShell.IdleTimerOverlayControlsHideStopTimer(Sender: TObject);
begin
  {
  IdleTimerOverlayControlsHide.Enabled:=false;

  Screen.Cursor:= crDefault;
  Self.Cursor:=crDefault;
  frmMain.Cursor:=crDefault;

  if (self.AlphaBlendValue = 0) then
  begin
    frmMain.ShowOverlayControls();
  end;
  }
  {
  if (self.Visible = false) then begin
    frmMain.ShowOverlayControls();
  end;
  }
  //IdleTimerOverlayControlsHide.Enabled:=false;
end;

procedure TfrmShell.IdleTimerOverlayControlsHideTimer(Sender: TObject);
begin
  //frmMain.DebugOutput('Timer');

  IdleTimerOverlayControlsHide.Enabled:=false;

  frmMain.HideOverlayControls;

  IdleTimerOverlayControlsHide.Enabled:=false;
end;

procedure TfrmShell.Image1Click(Sender: TObject);
begin

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

procedure TfrmShell.TrackBarVolumeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Somehow this TrackBar steals all the key down events...
  //Outputdebugstring(pchar('TfrmShell.TrackBarVolumeKeyDown'));
  frmMain.FormKeyDown(self,Key,Shift);
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
  //Outputdebugstring(pchar('TfrmShell.FormKeyDown'));
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

procedure TfrmShell.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
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

procedure TfrmShell.FormDestroy(Sender: TObject);
begin

end;



end.


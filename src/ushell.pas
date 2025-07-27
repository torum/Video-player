unit UShell;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ComCtrls,
  {$ifdef windows}Windows, DWMApi, windirs, win32titlestyler,{$endif}
  MPVBasePlayer, Types;

type

  { TfrmShell }

  TfrmShell = class(TForm)
    Button1: TButton;
    TrackBarVolume: TTrackBar;
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
  self.Color:=clBlack;
end;

procedure TfrmShell.Button1Click(Sender: TObject);
begin
  Outputdebugstring(pchar('TfrmShell.Button1Click'));
end;

procedure TfrmShell.Button1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Somehow this button steals all the key down events...
  Outputdebugstring(pchar('TfrmShell.Button1KeyDown'));
  frmMain.FormKeyDown(self,Key,Shift);
end;

procedure TfrmShell.FormShow(Sender: TObject);
begin

end;

procedure TfrmShell.TrackBarVolumeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Somehow this TrackBar steals all the key down events...
  Outputdebugstring(pchar('TfrmShell.TrackBarVolumeKeyDown'));
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
  Outputdebugstring(pchar('TfrmShell.FormKeyDown'));
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


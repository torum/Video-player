unit UShell;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  {$ifdef windows}Windows, DWMApi, windirs, win32titlestyler,{$endif}
  MPVBasePlayer;

type

  { TfrmShell }

  TfrmShell = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure PlayerStatusChanged(Sender: TObject; eState: TMPVPlayerState);

  public
    Player: TMPVBasePlayer;
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

  // player.
  Player := TMPVBasePlayer.Create;

end;

procedure TfrmShell.FormDblClick(Sender: TObject);
begin
    if (frmMain.IsFullscreen) then
  begin
    frmMain.ShowFullScreen(false);
  end else
  begin
    frmMain.ShowFullScreen(true);
  end;
end;

procedure TfrmShell.FormDestroy(Sender: TObject);
begin
  if Player <> nil then
    Player.Stop;

  FreeAndNil(Player);
end;

procedure TfrmShell.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  frmMain.FormDropFiles(self,FileNames);
end;

procedure TfrmShell.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  frmMain.FormKeyDown(self,Key,Shift);
end;

procedure TfrmShell.FormShow(Sender: TObject);
var
  errcode:integer;
begin

  BorderStyle := bsNone;
  //windowstate:=wsFullScreen;

  Player.InitPlayer(IntToStr(self.Handle), '', '', '');
  errcode := Player.SetHardwareDecoding('yes');
  //outputdebugstring(pchar(errcode.ToString));

  Player.OnStateChged := @PlayerStatusChanged;
end;

procedure TfrmShell.PlayerStatusChanged(Sender: TObject; eState: TMPVPlayerState);
//var
  //plWidth,plHeight:int64;
begin
  if (eState = TMPVPlayerState.mpsPlay) then
  begin
    // uhh not working?
    //outputdebugstring(pchar('mpsPlay height:'+Player.VideoHeight.ToString + ' width:'+Player.VideoWidth.ToString));

    // Fit window
    {
    plWidth:=0;plHeight:=0;
    Player.GetPropertyInt64('width', plWidth, False);
    Player.GetPropertyInt64('height', plHeight, False);
    //outputdebugstring(pchar('mpsPlay height:'+plHeight.ToString + ' width:'+plWidth.ToString));

    self.Align:=alClient;
    frmMain.height := plHeight;
    frmMain.width := plWidth;
    }

    // Manual
    // TODO:

  end else if (eState = TMPVPlayerState.mpsLoading) then
  begin
    //outputdebugstring(pchar('mpsLoading height:'+Player.VideoHeight.ToString + ' width:'+Player.VideoWidth.ToString));


  end else if (eState = TMPVPlayerState.mpsErr) then
  begin
    //outputdebugstring(pchar('mpsErr'));
  end
  else if (eState = TMPVPlayerState.mpsEnd) then
  begin
    outputdebugstring(pchar('mpsEnd'));

    // We don't know if this "End" is just finished the video or closing down the app. Need to check that or else we get AV.
    //frmMain.LoadNextVideo;
  end;


end;


end.


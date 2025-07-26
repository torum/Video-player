unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,Windows,
  MPVBasePlayer;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);   
    procedure FormDestroy(Sender: TObject);
    procedure Panel2DblClick(Sender: TObject);
  private 
    cPlayer: TMPVBasePlayer;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cPlayer := TMPVBasePlayer.Create;
  cPlayer.InitPlayer(IntToStr(Panel2.Handle), '', '', '');
  //cPlayer.OpenFile('test.mp4');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if cPlayer <> nil then
    cPlayer.Stop;

  FreeAndNil(cPlayer);
end;

procedure TfrmMain.Panel2DblClick(Sender: TObject);
begin
  //outputdebugstring(pchar('asdaf'));
end;

end.


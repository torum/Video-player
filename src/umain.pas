unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LclType, LclProc, LclIntf, Menus, StdCtrls, ExtDlgs, FileUtil,
  strutils, Types, FileCtrl, XMLConf,
  {$ifdef windows}Windows, DWMApi, windirs, win32titlestyler,{$endif}
  MPVBasePlayer, MPVClient;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    PopupMenu1: TPopupMenu;
    XMLConfig: TXMLConfig;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);

  private
    // Main file lists
    FstFileList:TStringList;
    FstDirectoryList:TStringList; 
    FstPlaylistList:TStringList; // TODO:

    FstFileExtList:TStringList;
    FstPlaylistExtList:TStringList;  // TODO:

    FstrInitialDir:string;
    FintCurrentFileIndex:integer;
    FisSingleFileSelected: boolean;
    FstrInitialSelectedVideoFile:string;

    // User Opts.
    FOptFileExts:string;// user defined file ext.
    FOptPlaylistExts:string;
    FOptIncludeSubFolders:boolean;
    FOptBackgroundBlack:boolean;
    FOptMinimulFileSizeKiloByte:integer; //TODO: need to check if this is even needed.

    // App status flags.
    FisFullScreen: boolean;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;
    //FCurrentMonitor:TMonitor;

    // Fullscreen related
    FintOldWndStyle:integer;
    FintOldExWndStyle:integer;

    // Form drag and move.
    FPos: TPoint;
    FisMouseDown: boolean;
    FisMoving: boolean;

    // no longer in use.
    FintAlphaBlendValue:integer;

    FintVolume:integer;
    FdblCurSec: Double;
    FdblTotalSec: Double;

    procedure OnPlayerStatusChanged(Sender: TObject; eState: TMPVPlayerState);
    procedure OnPlayerPropertyChanged(Sender: TObject; ID: MPVUInt64; Fmt: mpv_format; pData: Pointer);
    procedure OnPlayerProgress(Sender: TObject; CurSec, TotalSec: Double);
    procedure OnPlayerErrorMessage(Sender: TObject; const Prefix: string; Level: Int32; const Msg: string);

    procedure LoadDirectories(const Dirs: TStringList; FList: TStringList);
    procedure LoadSiblings(const FName: string; FList: TStringList);
    procedure LoadVideo;
    function PlayVideo(filename:string):boolean;

    procedure SetFullScreen_Universal(blnOn: boolean);
    procedure SetFullScreen_Win32(blnOn: boolean);

    procedure RestoreFormState;
    procedure StoreFormState;
    procedure ParsePramsAndBuildFileList; 
    procedure LoadSettings;
    procedure UpdateSeekValue();
    {$ifdef windows}
    //procedure EnableBlur(blon:boolean);
    {$endif}
    function GetCurrentMonitor():TMonitor;
    function GetCurrentMonitorIndex():integer;
    function FormatTime(sec: integer): string;
  public
    Player: TMPVBasePlayer;
    procedure ShowFullScreen(blnOn: boolean);
    procedure LoadNextVideo;
    procedure LoadPrevVideo;
    procedure SetVolume(newVol:double);
    procedure ShowOverlayControls;
    procedure HideOverlayControls();
    procedure DebugOutput(msg:string);
    Property IntCurrentFileIndex: integer read FintCurrentFileIndex;
    property FileList: TStringList read FstFileList;
    property CurrentMonitor:TMonitor read GetCurrentMonitor;
    Property IsFullScreen:boolean read FisFullscreen;
    Property IntAlphaBlendValue:integer read FintAlphaBlendValue;
  end;

  {$ifdef windows}
  { For EnableBlur
  AccentPolicy = packed record
    AccentState: Integer;
    AccentFlags: Integer;
    GradientColor: Integer;
    AnimationId: Integer;
  end;

  TWinCompAttrData = packed record
    attribute: THandle;
    pData: Pointer;
    dataSize: ULONG;
  end;
  }
  {$endif}

var
  frmMain: TfrmMain;

{$ifdef windows}
{ For EnableBlur
var
  SetWindowCompositionAttribute: function (Wnd: HWND; const AttrData: TWinCompAttrData): BOOL; stdcall = Nil;
}
{$endif}

implementation

uses
  UShell;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Set ini values.
  self.Caption := 'Movie Player';

  FstFileList:=TStringList.create;
  FstDirectoryList:=TStringList.create;
  FstPlaylistList:=TstringList.Create;

  FOptBackgroundBlack:=true;
  FOptFileExts:='.mp4;.mkv;.avi;.mpeg;.wmv';
  FOptPlaylistExts:='.m3u;.xspf';

  FstFileExtList := TStringList.Create;
  FstFileExtList.Delimiter:=';';
  FstFileExtList.DelimitedText:=FOptFileExts;

  FstPlaylistExtList:= TStringList.Create;
  FstPlaylistExtList.Delimiter:=';';
  FstPlaylistExtList.DelimitedText:=FOptPlaylistExts;

  FintCurrentFileIndex:=0;
  FstrInitialSelectedVideoFile :='';
  // Be carefull when settings this. If the size is too large, the list will be empty.
  FOptMinimulFileSizeKiloByte:=1;

  {$ifdef windows}
  FstrInitialDir := GetWindowsSpecialDir(CSIDL_MYVIDEO);
  {$else}
  // TODO:
  FstrInitialDir := '';
  {$endif}


  FintAlphaBlendValue := 100;

  // Load settings
  LoadSettings();

  // temp:
  FOptIncludeSubFolders := true;

  // Parse prams and build FileList
  ParsePramsAndBuildFileList();

  // Instansiate some objects.

  // Create player, but not init yet.
  Player := TMPVBasePlayer.Create;

  // Create so called shell to overlay some media controls using alpha values.
  frmShell := TfrmShell.create(self);
  frmShell.Parent := self;
end;

procedure TfrmMain.LoadSettings();
var
  configFile:string;
begin

  configFile := ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'.config');
  if (not FileExists(ExtractFilePath(Application.ExeName)+configFile)) and ForceDirectories(GetAppConfigDir(false)) then
  begin
    {$ifdef windows}
    XMLConfig.FileName:=GetAppConfigDir(false)+configFile;
    {$else}
    XMLConfig.FileName:=GetAppConfigDir(false)+'.'+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'') +'.config';
    {$endif}
  end else begin
    {$ifdef windows}
    XMLConfig.FileName:=ExtractFilePath(Application.ExeName)+configFile;
    {$else}
    XMLConfig.FileName:='.'+ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'') +'.config';
    {$endif}
  end;

  FstrInitialDir := string(XMLConfig.GetValue('/InitDir/Path',widestring(FstrInitialDir)));
  {$ifdef windows}
  if not (DirectoryExists(FstrInitialDir)) then
  begin
     FstrInitialDir := GetWindowsSpecialDir(CSIDL_MYVIDEO);
  end;
  {$endif}

  FoptBackgroundBlack := XMLConfig.GetValue('/Opts/BackgroundBlack',FoptBackgroundBlack);
  if (FOptBackgroundBlack) then
  begin
    self.Color:=clBlack;
  end else
  begin
    self.Color:=clWhite;
  end;

  FOptIncludeSubFolders := XMLConfig.GetValue('/Opts/IncludeSubFolders',false);
  FintVolume := XMLConfig.GetValue('/State/Volume',50);

  // This must be in "FormCreate", "FormShow" causes weird bug.
  if fileexists(XMLConfig.FileName) then
  begin
     RestoreFormState;
  end;
end;

procedure TfrmMain.ParsePramsAndBuildFileList();
var
  i:integer;
  f:int64;
begin

  // TODO: skip command line parameters for now.

  // Parse other prameters.
  for i := 1 to ParamCount do
  begin
    if (AnsiStartsStr('-',ParamStr(I))) then
    begin
      // Options. has been taken cared of already above.
    end else if (FileExists(ParamStr(I))) then
    begin
      // Found a file
      {$ifdef windows}
      {$else}
      // On UNIX, a directory is also a file.
      if (DirectoryExists(ParamStr(I))) then begin
        // Found a folder
        if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
        begin
          FstDirectoryList.Add(ParamStr(I));
        end;
        Continue;
      end;
      {$endif}
      if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(ParamStr(I)))) >= 0) then
      begin
        // Is a picture file.
        // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
        if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
        begin
          f:= FileSize(ParamStr(I));
          // Check file size.
          if f >= (FOptMinimulFileSizeKiloByte) then
          begin
            FstFileList.Add(ParamStr(I));

            FstrInitialSelectedVideoFile := ParamStr(I);
          end;
        end;
      end else if (FstPlaylistExtList.IndexOf(LowerCase(ExtractFileExt(ParamStr(I)))) >= 0) then
      begin
        // Found a playlist
        FstPlaylistList.Add(ParamStr(I));
      end;
    {$ifdef windows}
    end else if (DirectoryExists(ParamStr(I))) then
    begin
      // Found a folder.
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(ParamStr(I)))) then
      begin
        FstDirectoryList.Add(ParamStr(I));
      end;
    end else if (ParamStr(I) = '*') then
    begin
      // * option. Pretty much the same as "./"
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(GetCurrentDir))) then
      begin
        FstDirectoryList.Add(GetCurrentDir);
      end;
    {$endif}
    end;
  end;

  // sort
  //FstFileList.Sort;

  // Search inside folder(s)
  LoadDirectories(FstDirectoryList, FstFileList);

  {
  if ((FstFileList.Count < 1) and (FstrInitialSelectedVideoFile = '')) then
  begin
    // No files are provided in the parameter string, so open "file open" dialog.

    // Sets default dir
    OpenPictureDialog1.InitialDir:=FstrInitialDir;
    // Sets title
    OpenPictureDialog1.Title:=ReplaceStr(ExtractFileName(ParamStr(0)),ExtractFileExt(ParamStr(0)),'')+' - ' + resstrOpenPictures;
    // Open dialog
    if OpenPictureDialog1.Execute then
    begin
      for i:=0 to OpenPictureDialog1.Files.Count -1 do
      begin
        if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(OpenPictureDialog1.Files[i]))) >= 0) then
        begin
          // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
          if not (AnsiStartsStr('.',ExtractFilename(OpenPictureDialog1.Files[i]))) then
          begin
            f:= FileSize(OpenPictureDialog1.Files[i]);
            // Check file size.
            if f >= (FOptMinimulFileSizeKiloByte) then
            begin
              //FstFileList.Add(OpenPictureDialog1.Files[i]);
              if (i=0) then
              begin
                FstrInitialSelectedVideFile:=OpenPictureDialog1.Files[i];
                // sets init dir for next time.
                FstrInitialDir := ExtractFilePath(OpenPictureDialog1.Files[i]);
              end;
              FstFileList.Add(OpenPictureDialog1.Files[i]);
            end;
          end;
        end else if (FstPlaylistExtList.IndexOf(LowerCase(ExtractFileExt(OpenPictureDialog1.Files[i]))) >= 0) then
        begin
          // playlist.
          FstPlaylistList.Add(OpenPictureDialog1.Files[i]);
        end;
      end;

      // sort
      FstFileList.Sort;
    end;
  end;
  }

  // If only one image was selected, add all siblings automatically.
  // "send to" command-line parameters don't accept more than 255.
  if ((FstFileList.Count = 1) and (FstrInitialSelectedVideoFile <> '')) then
  begin
    LoadSiblings(FstFileList[0], FstFileList);

    //if (FstFileList.count = 0) then
    //   FstFileList.Add(FstrInitialSelectedImageFile);

    // Since automatically added, do not start slideshow at fullscreen later.
    FisSingleFileSelected:=true;
  end;

  if (FstFileList.indexOf(FstrInitialSelectedVideoFile) > -1) then
  begin
    FintCurrentFileIndex:=FstFileList.indexof(FstrInitialSelectedVideoFile);
  end else
  begin
    // Start with 0
    FintCurrentFileIndex:=0;
  end;

end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin

end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  errcode:integer;
begin

  {$ifdef windows}
  // Apply dark themed title bar.
  // https://forum.lazarus.freepascal.org/index.php/topic,59172.msg441116.html
  ApplyFormDarkTitle(self, true, false); // needed to force=false bacause of the glich when changing to fullscreen.
  {$endif}

  frmShell.Color:=clBlack;
  frmShell.AlphaBlend:=true;
  frmShell.AlphaBlendValue:=FintAlphaBlendValue;// set to max 255 or lower to show media controls when needed.
  frmShell.Visible:=true;
  frmShell.Show;

  // Int Player here.
  errcode := Player.InitPlayer(IntToStr(self.Handle), '', GetAppConfigDir(false), '');

  // TODO: Check errors.
  //outputdebugstring(pchar(errcode.ToString));
  //DebugOutput('err at FormShow: ' + errcode.ToString);

  Player.SetHardwareDecoding('yes');

  Player.SetPropertyString('media-controls', 'yes');
  //Player.SetPropertyString('idle', 'yes');
  //Player.SetPropertyString("screenshot-directory", "~~desktop/");
  Player.SetPropertyString('osd-playing-msg', '${media-title}');
  //Player.SetPropertyString('osc', 'yes');
  //Player.SetPropertyString('osd-bar', 'yes');
  Player.SetPropertyString('config-dir', GetAppConfigDir(false));
  Player.SetPropertyString('config', 'yes');
  //Player.SetPropertyString('input-conf', GetAppConfigDir(false)+'input.conf');
  // mpv logo on startup.
  Player.SetPropertyString('osc-idlescreen', 'no');
  //
  Player.SetPropertyString('osd-font-size', '16');
  //this isn't it. Player.SetPropertyString('osd-outline-size', '0'); //Default: 1.65

  //
  //Player.SetPropertyString('vo', 'libmpv');

  // Subscribing to status change event
  Player.OnStateChged := @OnPlayerStatusChanged; // @PlayerStatusChanged if objectpascal mode, PlayerStatusChanged if delphi.
  Player.OnPropertyChanged := @OnPlayerPropertyChanged; //TMPVPropertyChangedEvent
  Player.OnProgress:=@OnPlayerProgress; //TMPVProgressEvent
  Player.OnErrorMessage:=@OnPlayerErrorMessage;

  frmShell.TrackBarVolume.Value:=FintVolume;
  //SetVolume(FintVolume); // no need to set here, since TrackBarVolume.Value without flag updates it.

  // This need to be here in FormShow. not in the OnCreate.
  if (FstFileList.Count> 0) then
  begin
    LoadVideo;
  end;
end;

procedure TfrmMain.LoadDirectories(const Dirs: TStringList; FList: TStringList);
var
  f:int64;
  i,j:integer;
  fileSearchMask:string;
  folderfiles:TStringlist;
begin
  if Dirs.Count > 0 then
  begin
    // Create search mask
    fileSearchMask:='';
    for i:=0 to FstFileExtList.Count-1 do
    begin
      if trim(FstFileExtList[i]) <> '' then
      begin
         fileSearchMask:= fileSearchMask+'*'+trim(FstFileExtList[i])+';';
      end;
    end;
    // Loop directories and FindAllFiles.
    for i:=0 to Dirs.Count -1 do
    begin
      try
        // Recursively search files
        folderfiles := FindAllFiles(Dirs[i], fileSearchMask, FOptIncludeSubFolders);

        // sort
        folderfiles.Sort;

        for j:=0 to folderfiles.Count - 1 do
        begin
          // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
          if not (AnsiStartsStr('.',ExtractFilename(folderfiles[j]))) then
          begin
            f:= FileSize(folderfiles[j]);
            // Check file size.
            if f >= (FOptMinimulFileSizeKiloByte) then
            begin
              FList.Add(folderfiles[j]);
            end;
          end;
        end;
      finally
        folderfiles.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.LoadSiblings(const FName: string; FList: TStringList);
var
  f:int64;
  i,j:integer;
  fileSearchMask, fileFolder:string;
  folderfiles:TStringlist;
begin
  fileFolder:=ReplaceStr(FName,ExtractFileName(FName),'');
  //fileFolder:=ReplaceStr(FstrInitialSelectedImageFile,ExtractFileName(FstrInitialSelectedImageFile),'');

  // sets init dir for next time.
  FstrInitialDir := ExtractFilePath(FName);

  // Create search mask
  fileSearchMask:='';
  for i:=0 to FstFileExtList.Count-1 do
  begin
    if trim(FstFileExtList[i]) <> '' then
    begin
       fileSearchMask:= fileSearchMask+'*'+trim(FstFileExtList[i])+';';
    end;
  end;

  try
    // Find siblings.
    folderfiles := FindAllFiles(fileFolder, fileSearchMask, false);

    // sort
    folderfiles.Sort;

    for j:=0 to folderfiles.Count - 1 do
    begin
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(folderfiles[j]))) then
      begin
        // Ignore first selected image.
        if (folderfiles[j] <> FName) then
        begin
          f:= FileSize(folderfiles[j]);
          // Check file size.
          if f >= (FOptMinimulFileSizeKiloByte) then
          begin
            FList.Add(folderfiles[j]);
          end;
        end else
        begin
          // remove firest selected and add to list so that file order is right.
          FList.Delete(0);
          FList.Add(folderfiles[j]);
        end;
      end;
    end;
  finally
    folderfiles.Free;
  end;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  f:int64;
  i,newCurrentIndex:integer;
  FName, strInitialSelectedImageFile:string;
  TmpFileList, TmpDirList, TmpPlayList:TStringList;
  isSingleFSelected:boolean;
begin
  TmpFileList := TStringList.Create;
  TmpDirList := TStringList.Create;
  TmpPlayList := TStringList.Create;
  strInitialSelectedImageFile := '';
  isSingleFSelected:=false;

  for I := Low(FileNames) to High(FileNames) do
  begin
    FName := FileNames[I];
    if (FileExists(FName)) then
    begin
      // Found a file
      {$ifdef windows}
      {$else}
      // On UNIX, a directory is also a file.
      if (DirectoryExists(FName)) then
        Continue;
      {$endif}
      if (FstFileExtList.IndexOf(LowerCase(ExtractFileExt(FName))) >= 0) then
      begin
        // Is a picture file.
        // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
        if not (AnsiStartsStr('.',ExtractFilename(FName))) then
        begin
          f:= FileSize(FName);
          // Check file size.
          if f >= (FOptMinimulFileSizeKiloByte) then
          begin
            TmpFileList.Add(FName);
            strInitialSelectedImageFile := FName;
          end;
        end;
      end else if (FstPlaylistExtList.IndexOf(LowerCase(ExtractFileExt(FName))) >= 0) then
      begin
        // Found a playlist
        TmpPlayList.Add(FName);
      end else
      begin
        // Ignore.
      end;
    {$ifdef windows}
    end else if (DirectoryExists(FName)) then
    begin
      // Found a folder.
      // MacOS has a bad habit of leaving garbages like this. So, skipping files start with ".".
      if not (AnsiStartsStr('.',ExtractFilename(FName))) then
      begin
        TmpDirList.Add(FName);
      end;
    {$endif}
    end;
  end;

  LoadDirectories(TmpDirList, TmpFileList);

  if (TmpFileList.Count = 1) and (strInitialSelectedImageFile <> '') then
  begin
    LoadSiblings(TmpFileList[0], TmpFileList);
    isSingleFSelected:=true;
  end;

  if TmpFileList.Count <> 0 then
  begin
    FintCurrentFileIndex:=0;

    fisSingleFileSelected := isSingleFSelected;
    fstrInitialSelectedVideoFile := strInitialSelectedImageFile;
    FstFileList.Assign(TmpFileList);
    FstDirectoryList.Assign(TmpDirList);
    FstPlaylistList.Assign(TmpPlayList);

    if (FstrInitialSelectedVideoFile <> '') then
    begin
      newCurrentIndex:=FstFileList.indexOf(FstrInitialSelectedVideoFile);
      if (newCurrentIndex > -1) then
      begin
        FintCurrentFileIndex:=newCurrentIndex;
      end else
      begin
        FintCurrentFileIndex:=0;
      end;
    end;
    LoadVideo;
    SetFocus;
    BringToFront;
  end;

  TmpFileList.Free;
  TmpDirList.Free;
  TmpPlayList.Free;
end;

procedure TfrmMain.LoadNextVideo;
begin
  if FstFileList.Count > 0 then
  begin
    if (FstFileList.Count > FintCurrentFileIndex+1) then
    begin
      FintCurrentFileIndex := FintCurrentFileIndex+1;
    end else
    begin
      FintCurrentFileIndex := 0;
    end;

    LoadVideo;
  end;
end;

procedure TfrmMain.LoadPrevVideo;
begin
  if FstFileList.Count > 0 then
  begin
    if (FstFileList.Count >= FintCurrentFileIndex+1) then
    begin
      if (FintCurrentFileIndex <> 0) then
        FintCurrentFileIndex := FintCurrentFileIndex-1;
    end else
    begin
      FintCurrentFileIndex := 0;
      //exit;
    end;

    LoadVideo;
  end;
end;

procedure TfrmMain.LoadVideo;
var
  strPath:string;
begin
  // Let's not call this directly. Use LoadNextVideo or LoadPrevVideo.

  if FileList.Count > 0 then
  begin
    // Includes file path
    //strPath := MinimizeName(FileList[FintCurrentFileIndex],Self.Canvas, self.width - 300);
    // File name only.
    strPath := MinimizeName(ReplaceStr(ExtractFileName(FileList[FintCurrentFileIndex]),ExtractFileExt(FileList[FintCurrentFileIndex]),''),Self.Canvas, self.width - 300);

    if (FileList.Count = 1) then
    begin
      Self.Caption:=strPath;
    end else
    begin
      Self.Caption:='['+intToStr(FintCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
    end;

    PlayVideo(FileList[FintCurrentFileIndex]);
  end;
end;

function TfrmMain.PlayVideo(filename:string):boolean;
var
  errcode:integer;
begin
  try
    errcode := Player.OpenFile(filename);
    if (errcode <> 0) then
    begin
      Self.Caption := filename + ' is not playing.' + ' errcode: ' + errcode.ToString;
      result:=false;
    end else
    begin
      //
      result:=true;
    end;
  except
    on E: Exception do
    begin
      Self.Caption := 'Exception: ' + E.ClassName +' - '+ E.Message + ' ' + filename + ' is not playing.';
      {
      with Canvas do
        begin
          Brush.Style := bsClear;
          Font.Color := clWhite;
          TextOut(24,24, 'File load error: ' + E.ClassName +' - '+ E.Message );
          TextOut(24,52, 'File: ' + filename);
      end;
      }
      result:=false;
    end;
  end;
end;

procedure TfrmMain.OnPlayerStatusChanged(Sender: TObject; eState: TMPVPlayerState);
//var
  //plWidth,plHeight:int64;
begin
  // Need TThread.Synchronize();

  if (eState = TMPVPlayerState.mpsPlay) then
  begin
    // uhh not working? < looks like it gives me 0 for the first video.
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

  end
  else if (eState = TMPVPlayerState.mpsPause) then
  begin
    //
  end
  else if (eState = TMPVPlayerState.mpsStep) then
  begin
    // TODO: don't know what this is
  end
  else if (eState = TMPVPlayerState.mpsLoading) then
  begin
    // TODO: don't know what this is
    // how about "loaded"?
  end
  else if (eState = TMPVPlayerState.mpsUnk) then
  begin
    // TODO: don't know what this is
  end
  else if (eState = TMPVPlayerState.mpsErr) then
  begin
    //outputdebugstring(pchar('mpsErr'));
  end
  else if (eState = TMPVPlayerState.mpsEnd) then
  begin
    //outputdebugstring(pchar('mpsEnd'));
    // We don't know if this "End" is just finished the video or closing down the app. Need to check that or else we get AV.
    //frmMain.LoadNextVideo;
  end;

end;

procedure TfrmMain.OnPlayerPropertyChanged(Sender: TObject; ID: MPVUInt64; Fmt: mpv_format; pData: Pointer);
begin
  // Need TThread.Synchronize();
  //DebugOutput('OnPlayerPropertyChanged');
end;

procedure TfrmMain.OnPlayerProgress(Sender: TObject; CurSec, TotalSec: Double);
begin
  FdblCurSec := CurSec;
  FdblTotalSec := TotalSec;

  // Need TThread.Synchronize();
  TThread.Queue(nil,@UpdateSeekValue);
  //TThread.Synchronize(nil,@UpdateSeekValue);

  {
  TThread.Synchronize(
    nil,
    procedure
      begin
        ..
      end
  );
  }

end;

procedure TfrmMain.UpdateSeekValue();
var
  curTimeElapsed,totalTime:longint;
begin
  if (frmShell = nil) then exit;

  try
    totalTime := Round(FdblTotalSec * 10);
  except
    totalTime := 0;
  end;

  if (totalTime < 2) then exit;

  if (frmShell.SliderSeek.MaxValue <> totalTime) then
  begin
    frmShell.SliderSeek.MaxValue := totalTime;
  end;

  try
    curTimeElapsed := Round(FdblCurSec * 10);
  except
    curTimeElapsed :=0;
    exit;
  end;

  if (curTimeElapsed >= totalTime) then exit;

  frmShell.IsSliderSeekUpdating := true;
  if (not frmShell.IsSliderSeekChanging) then
  begin
    if (frmShell.SliderSeek.Value <> curTimeElapsed) then
    begin
      frmShell.SliderSeek.Value := curTimeElapsed;
    end;
  end;
  frmShell.LabelTimeFormatted.Caption := FormatTime(curTimeElapsed) + ' / ' + FormatTime(totalTime);
  frmShell.IsSliderSeekUpdating := false;

end;

function TfrmMain.FormatTime(sec: longint): string;
var
  TimeValue: TDateTime;
begin
  if (sec < 11) then result := '0';

  sec := sec div 10;
  TimeValue := sec / SecsPerDay;
  if (TimeValue > 3600) then
  begin
    result := FormatDateTime('hh:nn:ss', TimeValue);
  end else
  begin
    result := FormatDateTime('nn:ss', TimeValue);
  end;
end;

procedure TfrmMain.OnPlayerErrorMessage(Sender: TObject; const Prefix: string; Level: Int32; const Msg: string);
begin
  // Need TThread.Synchronize();
  DebugOutput('OnPlayerErrorMessage:'+ Msg);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  plState:TMPVPlayerState;
  curVol:double;
begin

  if ((Key = VK_F11) or (Key = VK_ESCAPE)) then
  begin
    if (FisFullscreen) then
    begin
      ShowFullScreen(false);
    end;
    HideOverlayControls();
    exit;
  end;

  if ((Chr(Key) = 'F') or (Chr(Key) = 'S')) then
  begin
    if (FisFullscreen) then
    begin
      ShowFullScreen(false);
    end else
    begin
      ShowFullScreen(true);
    end;
    exit;
  end;

  if (Chr(Key) = 'Q') then
  begin
    if (ssCtrl in Shift) then
    begin
      close;
    end;
  end;

  //Player

  // Check player status.
  plState := Player.GetState;

  // Pause, resume or Start
  if (Key = VK_PAUSE) or (Key = VK_SPACE) or (Chr(Key) = 'P') then
  begin
    if (plState = TMPVPlayerState.mpsPlay) then
    begin             
      //outputdebugstring(pchar('TMPVPlayerState.mpsPlay'));
      Player.Pause;
      ShowOverlayControls();
    end
    else if (plState = TMPVPlayerState.mpsPause) then
    begin                   
      //outputdebugstring(pchar('TMPVPlayerState.mpsPause'));
      Player.Resume;
    end
    else if (plState = TMPVPlayerState.mpsStop) then // TODO: need to check.
    begin
      //outputdebugstring(pchar('TMPVPlayerState.mpsStop'));
      // do nothing. probably closing.
    end
    else if (plState = TMPVPlayerState.mpsEnd) then
    begin
      // TODO: mpsEnd is also fired when closing? need to check.
      if (FileList.Count > 0) then
      begin
        // TODO: check auto next mode.
        LoadNextVideo;
        //outputdebugstring(pchar(FileList[0]));
      end else
      begin
        //outputdebugstring(pchar('FileList.Count = 0'));
      end;
    end;
    {
    else if (plState = TMPVPlayerState.mpsUnk) then
    begin
      //outputdebugstring(pchar('TMPVPlayerState.mpsUnk'));
    end
    else if (plState = TMPVPlayerState.mpsErr) then
    begin
      //outputdebugstring(pchar('TMPVPlayerState.mpsErr'));
    end
    else if (plState = TMPVPlayerState.mpsStep) then
    begin
      //outputdebugstring(pchar('TMPVPlayerState.mpsStep'));
    end else if (plState = TMPVPlayerState.mpsLoading) then
    begin
      //outputdebugstring(pchar('TMPVPlayerState.mpsLoading'));
    end else
    begin
      //outputdebugstring(pchar('TMPVPlayerState else'));
    end;
    }
  end;

  // Volume (can set if player is not playing)
  // UP
  if (Key = VK_UP) then
  begin
    curVol := Player.GetVolume;
    if (ssCtrl in Shift) then
    begin
      SetVolume(curVol+10);
    end else
    begin
      SetVolume(curVol+5);
    end;
    // Show controls
    ShowOverlayControls();
  end;
  // Down
  if (Key = VK_DOWN) then
  begin
    curVol := Player.GetVolume;
    if (ssCtrl in Shift) then
    begin
      SetVolume(curVol-10);
    end else
    begin
      SetVolume(curVol-5);
    end;
    // Show controls
    ShowOverlayControls();
  end;

  // Only if player is playing something.
  if ((plState = TMPVPlayerState.mpsPlay) or (plState = TMPVPlayerState.mpsPause)) then
  begin

    // Skip
    if (Key = VK_RIGHT) then
    begin
      if (ssShift in Shift) then
      begin
        // play next video
        LoadNextVideo;
      end
      else if (ssCtrl in Shift) then
      begin
        Player.Seek(100,true);
      end else
      begin
        Player.Seek(10,true);
      end;
      ShowOverlayControls();
    end;

    // Back
    if (Key = VK_LEFT) or (Key = VK_BACK) then
    begin
      if (ssShift in Shift) then
      begin
        // Play previous video.
        LoadPrevVideo;
      end
      else if (ssCtrl in Shift) then
      begin
        Player.Seek(-100,true);
      end else
      begin
        Player.Seek(-10,true);
      end;
      ShowOverlayControls();
    end;


  end;
        {

        if (Chr(Key) = 'I') then
        begin
          // Stretch In
          frmFullscreen.MenuItemFitClick(sender);
        end;
        if (Chr(Key) = 'O') then
        begin
          // Stretch Out
          frmFullscreen.MenuItemExpandClick(sender);
        end;
        if (Chr(Key) = 'E') then
        begin
          // Effect
          frmFullscreen.MenuItemEffectClick(sender);
        end;
        if (Chr(Key) = 'N') then
        begin
          // Random
          frmFullscreen.MenuItemRandomClick(sender);
        end;
        if (Chr(Key) = 'R') then
        begin
          // Repeat
          frmFullscreen.MenuItemRepeatClick(sender);
        end;

        if ((Key = VK_RMENU) or (Key = VK_LMENU) or (Key = VK_MENU)) then
        begin
          frmFullscreen.PopupMenu.PopUp(0,0);
        end;
        }


end;

procedure TfrmMain.FormDblClick(Sender: TObject);
begin
  // this is needed for mouse drag window to work correctly.
  FisMoving:=false;
  FisMouseDown:=false;

  if (FisFullscreen) then
  begin
    ShowFullScreen(false);
  end else
  begin
    ShowFullScreen(true);
  end;
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: begin
      FPos.X:=X;
      FPos.Y:=Y;
      FisMouseDown:=true;
    end;
    mbRight: begin
      // Right click show popupmenu.
      // when mouse up
    end;
  end;
end;

procedure TfrmMain.FormMouseEnter(Sender: TObject);
begin
  //DebugOutput('FormMouseEnter');
  //ShowOverlayControls();   //not really good.
end;

procedure TfrmMain.ShowOverlayControls();
var
  curVol:double;
begin
  Screen.Cursor:= crDefault;
  Self.Cursor:=crDefault;

  // Make visible
  frmShell.AlphaBlendValue:=FintAlphaBlendValue;
  //frmShell.Visible:=true;
  frmShell.Cursor:=crDefault;

  // needed this. when using non-alppha way
  //frmShell.Repaint;

  // re-enable
  frmShell.IdleTimerOverlayControlsHide.Enabled:=false;
  frmShell.IdleTimerOverlayControlsHide.Enabled:=true;

  if (Player = nil) then exit; // we are closing.

  // Update volume value
  curVol := Player.GetVolume;
  frmShell.TrackBarVolume.Value:=Trunc(curVol);
end;

procedure TfrmMain.HideOverlayControls();
begin
  frmShell.IdleTimerOverlayControlsHide.Enabled:=false;
  if (frmShell.IsInhibitIdleHideControls) then
  begin
    frmShell.IdleTimerOverlayControlsHide.Enabled:=true;
    exit;
  end;

  //if not FisPopupMenuShowing then
  Screen.Cursor:= crNone;
  Self.Cursor:=crNone;
  frmShell.Cursor:=crNone;

  if (frmShell.AlphaBlendValue <> 0) then
  begin
    frmShell.AlphaBlendValue:=0;
  end;

  if (frmShell.Visible) then begin
    //frmShell.Visible:=false;
  end;
                     
  frmShell.IdleTimerOverlayControlsHide.Enabled:=false;
end;

procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ((not FisMouseDown) and (not FisMoving)) then
  begin
    ShowOverlayControls();
  end;

  if (frmMain.IsFullscreen) then exit;

  if (FisMouseDown) then
  begin
    FisMoving:=true;
    frmMain.Left:=frmMain.Left+X-FPos.X;
    frmMain.Top:=frmMain.Top+Y-FPos.Y;
  end;
end;

procedure TfrmMain.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ((not FisMouseDown) and (not FisMoving)) then
  begin
    ShowOverlayControls();
  end;

  FisMouseDown:=false;
  FisMoving:=False;
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

  // Show controls
  //ShowOverlayControls();
end;

procedure TfrmMain.SetVolume(newVol:double);
begin
    frmShell.IsTrackBarVolumeUpdating := true;
    Player.SetVolume(newVol);
    if (not frmShell.IsTrackBarVolumeChanging) then
    begin
      frmShell.TrackBarVolume.Value:=Trunc(newVol);
    end;
    frmShell.IsTrackBarVolumeUpdating := false;

    // for settings save.
    FintVolume:=Trunc(newVol);
end;

procedure TfrmMain.ShowFullScreen(blnOn: boolean);
begin
  {$ifdef windows}
  SetFullScreen_Win32(blnOn);
  {$else}
  SetFullScreen_Universal(blnOn);
  {$endif}
  FisFullscreen:=blnOn;
end;

procedure TfrmMain.SetFullScreen_Universal(blnOn: boolean);
{$ifdef darwin}var
  Form: TForm;{$endif}
begin
  if blnOn then
  begin
    if not FisFullScreen then
    begin
      // Save original windowstate
      FOrigWndState:= WindowState;
      FOrigBounds := BoundsRect;
    end;
    // Don't do this if the form is modal on linux. And it won't work with multi moniters.
    // WindowState:=wsFullScreen;
    {$ifdef windows}
    // Don't do this at runtime on linux!
    // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
    BorderStyle:= bsNone;

    {$endif}

    {$ifdef darwin}
    // Hide title bar
    // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
    self.BorderStyle:=bsNone;
    Form := TForm.Create(nil);
    try
      Parent := Form;
      Parent := nil;
    finally
      Form.Free;
    end;
    {$endif}

    //if (CurrentMonitor <> Screen.Monitors[FOptIntMoniter]) then
    //begin
    //  BoundsRect:= Screen.Monitors[FOptIntMoniter].BoundsRect;
    //end else
    //begin
      BoundsRect:= CurrentMonitor.BoundsRect;
    //end;

    {$ifdef darwin}
    // on Mac, don't call SW_SHOWFULLSCREEN on main. And make sure to call SW_SHOWNORMAL on close window on Mac.
    {$else}
    ShowWindow(Handle, SW_SHOWFULLSCREEN);
    {$endif}

  end else
  begin
    WindowState:= FOrigWndState;
    {$ifdef windows}
    // Don't do this at runtime on linux!
    BorderStyle:= bsSizeable;
    {$endif}

    {$ifdef darwin}
    // Un-hide title bar
    // https://forum.lazarus.freepascal.org/index.php?topic=38675.0
    self.BorderStyle:=bsSizeable;
    Form := TForm.Create(nil);
    try
      Parent := Form;
      Parent := nil;
    finally
      Form.Free;
    end;
    {$endif}

    {$ifdef darwin}
    // on Mac, don't call SW_SHOWFULLSCREEN on main. And make sure to call SW_SHOWNORMAL on close window on Mac.
    {$else}
    ShowWindow(Handle, SW_SHOWNORMAL);
    {$endif}
    //BoundsRect:= FOrigBounds;
    WindowState:= FOrigWndState;
    if (FOrigWndState = wsNormal) then
    begin
      //BoundsRect:= FOrigBounds; // Not needed. Resized to a wrong size.
    end;
  end;
end;

procedure TfrmMain.SetFullScreen_Win32(blnOn: boolean);
begin
  // Do not use "BorderStyle:= bsSizeable;" or self.Parent:=nil;
  // Both change window handle which causes app to crash because libmpv dll does not like it.
  {$ifdef windows}
  if blnOn then
  begin

    if not FisFullScreen then
    begin
      // Must be this order from here.
      FOrigWndState:= WindowState;// 1
      FOrigBounds:= BoundsRect;   // 2

      FintOldWndStyle:= GetWindowLong(Self.handle,GWL_STYLE);      // 3
      FintOldExWndStyle:= GetWindowLong(Self.handle,GWL_EXSTYLE);  // 4


      //frmShell.Visible:=false;
      //frmShell.Parent:=nil;
      //Self.BeginFormUpdate;
      //SetWindowLong(Handle, GWL_STYLE, WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_SYSMENU); < deprecated
      SetWindowLongPtr(Self.Handle, GWL_STYLE, LONG_PTR(WS_VISIBLE and (not WS_DLGFRAME) or WS_EX_LAYERED)); // 5
      SetWindowLongPtr(Self.Handle, GWL_EXSTYLE, WS_EX_CONTROLPARENT or WS_EX_APPWINDOW or WS_EX_ACCEPTFILES);          // 6
      //SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOREDRAW or SWP_FRAMECHANGED);
      SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, CurrentMonitor.BoundsRect.Right, CurrentMonitor.BoundsRect.Bottom, SWP_NOMOVE or SWP_NOSIZE or SWP_NOREDRAW or SWP_FRAMECHANGED);


      //ShowWindow(Handle, SW_SHOWFULLSCREEN);
      //Application.ProcessMessages;
      BoundsRect:= CurrentMonitor.BoundsRect;  
      WindowState:= wsFullScreen; // 7

      //Self.EndFormUpdate;
      Self.Refresh;


      // Must be this order til here.
      //frmShell.Parent:=self;
      frmShell.ReAlign;
      frmShell.Align:=alBottom;
      //frmShell.Visible:=true;
      frmShell.Refresh;
      frmShell.Repaint;
    end;

    SetFocus;
    BringToFront;
    SetForegroundWindow(handle);

  end else
  begin
    if FisFullScreen then
    begin
      Self.BeginFormUpdate;

      if (FOrigWndState = wsNormal) then
      begin
        BoundsRect:= FOrigBounds;
      end;

      SetWindowLongPtr(handle, GWL_STYLE, FintOldWndStyle);
      //SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOREDRAW or SWP_FRAMECHANGED);
      SetWindowLongPtr(handle, GWL_EXSTYLE, FintOldExWndStyle);

      Self.EndFormUpdate;
      Application.ProcessMessages;

      WindowState:= FOrigWndState;

      SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);

    end;


    {
    // Background blur when background color is set to clBlack.
    if (Win32MajorVersion>=10) then
    begin
      self.color := clBlack; // Temp set to black
      DoubleBuffered := True;
      EnableBlur(false); // TODO: make this an option.
    end;
    }
    frmShell.ReAlign;
    frmShell.Align:=alBottom;
    frmShell.Refresh;
    frmShell.Repaint;

    //ShowWindow(Handle, SW_SHOWNORMAL);
    SetFocus;
    BringToFront;
    SetForegroundWindow(handle);
  end;
  {$endif}
end;

function TfrmMain.GetCurrentMonitor():TMonitor;
begin
  result:=Screen.MonitorFromWindow(Handle);  ;
end;

function TfrmMain.GetCurrentMonitorIndex():integer;
var
  i:integer;
begin
  result:=0;
  for i:=0 to Screen.MonitorCount-1 do
  begin
    if CurrentMonitor = Screen.Monitors[i] then
    begin
      result:=i;
      break;
    end;
  end;
end;

procedure TfrmMain.RestoreFormState;
var
  LastWindowState: TWindowState;
begin
  with XMLConfig do
  begin
    LastWindowState := TWindowState(GetValue('WindowState', Integer(WindowState)));

    if LastWindowState = wsMaximized then
    begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('RestoredLeft', RestoredLeft),
        GetValue('RestoredTop', RestoredTop),
        GetValue('RestoredWidth', RestoredWidth),
        GetValue('RestoredHeight', RestoredHeight));
      WindowState := wsMaximized;
    end else
    begin
      // Somehow, this causes strange behaviour.. fullscreen won't work < when restore in FormShow event. OnCreate is ok.
      //WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('NormalLeft', Left),
        GetValue('NormalTop', Top),
        GetValue('NormalWidth', Width),
        GetValue('NormalHeight', Height));
    end;
  end;
end;

procedure TfrmMain.StoreFormState;
begin
  // Save form state.
  with XMLConfig do
  begin
    SetValue('WindowState', Integer(WindowState));
    SetValue('NormalLeft', Left);
    SetValue('NormalTop', Top);
    SetValue('NormalWidth', Width);
    SetValue('NormalHeight', Height);
    SetValue('RestoredLeft', RestoredLeft);
    SetValue('RestoredTop', RestoredTop);
    SetValue('RestoredWidth', RestoredWidth);
    SetValue('RestoredHeight', RestoredHeight);
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  self.hide;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  StoreFormState;

  // Save options.
  //XMLConfig.SetValue('/Opts/Random',FOptRandom);
  //XMLConfig.SetValue('/Opts/Repeat',FOptRepeat);
  //XMLConfig.SetValue('/Opts/Moniter',FOptIntMoniter);
  XMLConfig.SetValue('/Opts/MinimulFileSizeKiloByte',FOptMinimulFileSizeKiloByte);
  //XMLConfig.SetValue('/Opts/StayOnTop',FoptStayOnTop);
  XMLConfig.SetValue('/Opts/BackgroundBlack',FoptBackgroundBlack);
  XMLConfig.SetValue('/Opts/IncludeSubFolders',FOptIncludeSubFolders);
  XMLConfig.SetValue('/Opts/FileExts',widestring(FOptFileExts));
  XMLConfig.SetValue('/Opts/PlaylistExts',widestring(FOptPlaylistExts));
  XMLConfig.SetValue('/InitDir/Path',widestring(FstrInitialDir));

  XMLConfig.SetValue('/State/Volume',FintVolume);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Clean up
  if Player <> nil then
    Player.Stop;

  FreeAndNil(Player);

  FstFileExtList.Free;
  FstPlaylistExtList.Free;
  FstFileList.Free;
  FstDirectoryList.Free;
  FstPlaylistList.Free;
end;

procedure TfrmMain.DebugOutput(msg:string);
begin
  {$ifdef windows}
  OutputDebugString(pchar(msg));
  {$endif}
end;

{$ifdef windows}
{
procedure TfrmMain.EnableBlur(blon:boolean);
// requires delphi mode.
// https://wiki.lazarus.freepascal.org/Aero_Glass
const
  WCA_ACCENT_POLICY = 19;
  ACCENT_ENABLE_BLURBEHIND = 3;
  //ACCENT_ENABLE_ACRYLICBLURBEHIND = 4; // Windows 10 April 2018 Update
  ACCENT_DISABLED = 0;
  DrawLeftBorder = $20;
  DrawTopBorder = $40;
  DrawRightBorder = $80;
  DrawBottomBorder = $100;
var
  dwm10: THandle;
  data : TWinCompAttrData;
  accent: AccentPolicy;
begin
  //require Windows 10
  if Win32MajorVersion<10 then exit;

  dwm10 := LoadLibrary('user32.dll');
  try
    @SetWindowCompositionAttribute := GetProcAddress(dwm10, 'SetWindowCompositionAttribute');
    if @SetWindowCompositionAttribute <> nil then
    begin
      if (blon) then
      begin
        accent.AccentState := ACCENT_ENABLE_BLURBEHIND;
      end else
      begin
        accent.AccentState := ACCENT_DISABLED;
      end;
      //accent.AccentState := ACCENT_ENABLE_BLURBEHIND;
      ////accent.AccentState := ACCENT_ENABLE_ACRYLICBLURBEHIND;
      //accent.GradientColor := (100 SHL 24) or (clBlue);
      accent.AccentFlags := DrawLeftBorder or DrawTopBorder or DrawRightBorder or DrawBottomBorder;

      data.Attribute := WCA_ACCENT_POLICY;
      data.dataSize := SizeOf(accent);
      data.pData := @accent;
      SetWindowCompositionAttribute(self.Handle, data);

    end
    else
    begin
      //ShowMessage('Not found Windows 10 blur API');
    end;
  finally
    FreeLibrary(dwm10);
  end;

end;
}
{$endif}

{$ifdef windows}
{
// for EnableBlur
initialization
  SetWindowCompositionAttribute := GetProcAddress(GetModuleHandle(user32), 'SetWindowCompositionAttribute');
}
{$endif}

end.


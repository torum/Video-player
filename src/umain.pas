unit UMain;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LclType, LclProc, LclIntf, Menus, StdCtrls, ExtDlgs, FileUtil,
  strutils, Types, FileCtrl, XMLConf,
  {$ifdef windows}Windows, DWMApi, windirs, win32titlestyler,{$endif}
  MPVBasePlayer;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    IdleTimerMouseHide: TIdleTimer;
    XMLConfig: TXMLConfig;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerMouseHideStopTimer(Sender: TObject);
    procedure IdleTimerMouseHideTimer(Sender: TObject);

  private 
    FintOldWndStyle:integer;
    FintOldExWndStyle:integer;

    // Main file lists
    FstFileList:TStringList;
    FstDirectoryList:TStringList; 
    FstPlaylistList:TStringList; // TODO:

    FstFileExtList:TStringList;
    FstPlaylistExtList:TStringList;  // TODO:

    // User Opts.
    FOptFileExts:string;// user defined file ext.
    FOptPlaylistExts:string;
    FOptIncludeSubFolders:boolean;
    FOptBackgroundBlack:boolean;
    FOptMinimulFileSizeKiloByte:integer; //TODO: need to check if this is even needed.
    FstrInitialDir:string;
    FiCurrentFileIndex:integer;
    FisSingleFileSelected: boolean;
    FstrInitialSelectedVideoFile:string;
    // App status flags.
    FisFullScreen: boolean;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;
    FCurrentMonitor:TMonitor;
    procedure LoadDirectories(const Dirs: TStringList; FList: TStringList);
    procedure LoadSiblings(const FName: string; FList: TStringList);
    procedure LoadVideo;
    function DispayVideo(filename:string):boolean;
    procedure ShowFullScreen(blnOn: boolean);
    procedure SetFullScreen_Universal(blnOn: boolean);
    procedure SetFullScreen_Win32(blnOn: boolean);
    function GetCurrentMonitor():TMonitor;
    function GetCurrentMonitorIndex():integer;
    procedure RestoreFormState;
    procedure StoreFormState;
  public
    cPlayer: TMPVBasePlayer;
    property FileList: TStringList read FstFileList;
    property CurrentMonitor:TMonitor read GetCurrentMonitor;
  end;

  {$ifdef windows}
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
  {$endif}

var
  frmMain: TfrmMain;

{$ifdef windows}
var
  SetWindowCompositionAttribute: function (Wnd: HWND; const AttrData: TWinCompAttrData): BOOL; stdcall = Nil;
{$endif}

implementation


{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  configFile:string;
begin

  self.Caption := 'Movie Player';

  FstFileList:=TStringList.create;
  FstDirectoryList:=TStringList.create;
  FstPlaylistList:=TstringList.Create;

  FOptBackgroundBlack:=true;
  FOptFileExts:='.mp4;.mkv;.avi;.mpeg;.wmv;';
  FOptPlaylistExts:='.m3u;.xspf';

  FstFileExtList := TStringList.Create;
  FstFileExtList.Delimiter:=';';
  FstFileExtList.DelimitedText:=FOptFileExts;

  FstPlaylistExtList:= TStringList.Create;
  FstPlaylistExtList.Delimiter:=';';
  FstPlaylistExtList.DelimitedText:=FOptPlaylistExts;

  FiCurrentFileIndex:=0;
  FstrInitialSelectedVideoFile :='';
  // Be carefull when settings this. If the size is too large, the list will be empty.
  FOptMinimulFileSizeKiloByte:=1;

  {$ifdef windows}
  FstrInitialDir := GetWindowsSpecialDir(CSIDL_MYVIDEO);
  {$else}
  // TODO:
  FstrInitialDir := '';
  {$endif}

  // Load settings
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

  if (FOptBackgroundBlack) then
  begin
    self.Color:=clBlack;
  end else
  begin
    self.Color:=clWhite;
  end;

  // This must be in "FormCreate", "FormShow" causes weird bug.
  if fileexists(XMLConfig.FileName) then
  begin
     RestoreFormState;
  end;

  // Init player.
  cPlayer := TMPVBasePlayer.Create;
  cPlayer.InitPlayer(IntToStr(self.Handle), '', '', '');

end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin


{$ifdef windows}
  // Apply dark themed title bar.
  // https://forum.lazarus.freepascal.org/index.php/topic,59172.msg441116.html
  ApplyFormDarkTitle(self, FoptBackgroundBlack, true);
{$endif}

end;

procedure TfrmMain.LoadDirectories(const Dirs: TStringList; FList: TStringList);
var
  i,j,f:integer;
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
  i,j,f:integer;
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
  i,f,newCurrentIndex:integer;
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
    FiCurrentFileIndex:=0;

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
        FiCurrentFileIndex:=newCurrentIndex;
      end else
      begin
        FiCurrentFileIndex:=0;
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

procedure TfrmMain.LoadVideo;
var
  strPath:string;
begin
  if FileList.Count > 0 then
  begin

    strPath := MinimizeName(FileList[FiCurrentFileIndex],Self.Canvas, self.width - 300);

    if (FileList.Count = 1) then
    begin
      Self.Caption:=strPath;
    end else
    begin
      Self.Caption:='['+intToStr(FiCurrentFileIndex+1)+'/'+ intToStr(FileList.Count) +'] ' + strPath;
    end;

    DispayVideo(FileList[FiCurrentFileIndex]);
  end;
end;

function TfrmMain.DispayVideo(filename:string):boolean;
begin
  try
    cPlayer.OpenFile(filename);
    result:=true;
  except
    on E: Exception do
    begin
      with Canvas do
        begin
          Brush.Style := bsClear;
          Font.Color := clWhite;
          TextOut(24,24, 'File load error: ' + E.ClassName +' - '+ E.Message );
          TextOut(24,52, 'File: ' + filename);
      end;
      result:=false;
    end;
  end;
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
    end else
    begin
      //ShowFullScreen(true);
    end;
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

  plState := cPlayer.GetState;

  // Pause/Start
  if (Key = VK_PAUSE) or (Key = VK_SPACE) then
  begin
    if (plState = TMPVPlayerState.mpsPlay) then
    begin
      cPlayer.Pause;
    end
    else if (plState = TMPVPlayerState.mpsPause) then
    begin
      cPlayer.Resume;
    end
    else if (plState = TMPVPlayerState.mpsStop) then // TODO: need to check.
    begin
      //outputdebugstring(pchar('mpsStop'));
      if (FileList.Count > 0) then
      begin
        DispayVideo(FileList[0]);
        //outputdebugstring(pchar(FileList[0]));
      end;
    end;
  end;

  if (plState = TMPVPlayerState.mpsPlay) then
  begin

    // Skip
    if (Key = VK_RIGHT) then
    begin
      if (ssCtrl in Shift) then
      begin
        cPlayer.Seek(100,true);
      end else
      begin
        cPlayer.Seek(10,true);
      end;
    end;

    // Back
    if (Key = VK_LEFT) or (Key = VK_BACK) then
    begin
      if (ssCtrl in Shift) then
      begin
        cPlayer.Seek(-100,true);
      end else
      begin
        cPlayer.Seek(-10,true);
      end;
    end;

    // Volume
    if (Key = VK_UP) then
    begin
      curVol := cPlayer.GetVolume;

      if (ssCtrl in Shift) then
      begin
        cPlayer.SetVolume(curVol+10);
      end else
      begin
        cPlayer.SetVolume(curVol+5);
      end;
    end;

    if (Key = VK_DOWN) then
    begin
      curVol := cPlayer.GetVolume;

      if (ssCtrl in Shift) then
      begin
        cPlayer.SetVolume(curVol-10);
      end else
      begin
        cPlayer.SetVolume(curVol-5);
      end;
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
  if (FisFullscreen) then
  begin
    ShowFullScreen(false);
  end else
  begin
    ShowFullScreen(true);
  end;
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

  if blnOn then
  begin

    if not FisFullScreen then
    begin
      // Must be this order from here.
      FOrigWndState:= WindowState;// 1
      FOrigBounds:= BoundsRect;   // 2

      FintOldWndStyle:= GetWindowLong(handle,GWL_STYLE);      // 3
      FintOldExWndStyle:= GetWindowLong(handle,GWL_EXSTYLE);  // 4

      self.Visible:=false;

      //SetWindowLong(Handle, GWL_STYLE, WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_SYSMENU); < deprecated
      SetWindowLongPtr(Handle, GWL_STYLE, LONG_PTR(WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_SYSMENU)); // 5
      SetWindowLongPtr(Handle, GWL_EXSTYLE, WS_EX_CONTROLPARENT or WS_EX_APPWINDOW);                     // 6


      WindowState:= wsFullScreen; // 7
      //BoundsRect:= CurrentMonitor.BoundsRect;
      self.Visible:=true;
      // Must be this order til here.

    end;

    //ShowWindow(Handle, SW_SHOW);
    SetFocus;
    BringToFront;
    SetForegroundWindow(handle);

  end else
  begin
    if FisFullScreen then
    begin
      SetWindowLongPtr(handle, GWL_STYLE, FintOldWndStyle);
      SetWindowLongPtr(handle, GWL_EXSTYLE, FintOldExWndStyle);

      WindowState:= FOrigWndState;

      if (FOrigWndState = wsNormal) then
      begin
        BoundsRect:= FOrigBounds;
      end;
    end;

    //ShowWindow(Handle, SW_SHOWNORMAL);
    SetFocus;
    BringToFront;
    //SetForegroundWindow(handle);
  end;
end;

function TfrmMain.GetCurrentMonitor():TMonitor;
begin
  if not Assigned(FCurrentMonitor) then
  begin
    FCurrentMonitor := Screen.MonitorFromWindow(Handle);
    result:=FCurrentMonitor;
  end else
  begin
    result:=FCurrentMonitor;
  end;
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

procedure TfrmMain.IdleTimerMouseHideStopTimer(Sender: TObject);
begin
  Screen.Cursor:= crDefault;
  Self.Cursor:=crDefault;
end;

procedure TfrmMain.IdleTimerMouseHideTimer(Sender: TObject);
begin
  //if not FisPopupMenuShowing then
  //begin
    Screen.Cursor:= crNone;
    Self.Cursor:=crNone;
  //end;
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

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Clean up

  if cPlayer <> nil then
    cPlayer.Stop;

  FreeAndNil(cPlayer);


  FstFileExtList.Free;
  FstPlaylistExtList.Free;
  FstFileList.Free;
  FstDirectoryList.Free;
  FstPlaylistList.Free;
end;


end.


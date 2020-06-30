unit main;
(*
Open Sitemap Builder

Copyright (C) 2010 Matthew Hipkin <http://www.matthewhipkin.co.uk>
All rights reserved.

Copyright (C) 2020 Алексей Черемных <https://КонтинентСвободы.рф>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
   may be used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.

IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, SynHighlighterXML,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  XiPanel, XiButton, miscfunc, resolve, StrUtils, IniFiles, LCLIntF, scanthread;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAboutS: TButton;
    btnCopyS: TButton;
    btnClearS: TButton;
    btnSaveS: TButton;
    btnGoS: TButton;
    btnCancelS: TButton;
    labelUpdate: TLabel;
    labelCount: TLabel;
    PageControl1: TPageControl;
    bgPanelS: TPanel;
    btnOptions: TSpeedButton;
    StatusBar1: TStatusBar;
    textList: TSynEdit;
    tabList: TTabSheet;
    updatesTimer: TTimer;
    workPanelS: TPanel;
    SaveDialog1: TSaveDialog;
    textHTML: TSynEdit;
    textXML: TSynEdit;
    textCSV: TSynEdit;
    HTMLHighlighter: TSynHTMLSyn;
    tabHTML: TTabSheet;
    tabXML: TTabSheet;
    tabCSV: TTabSheet;
    textURL: TComboBox;
    Label1: TLabel;
    procedure btnAboutSClick(Sender: TObject);
    procedure btnCancelSClick(Sender: TObject);
    procedure btnClearSClick(Sender: TObject);
    procedure btnCopySClick(Sender: TObject);
    procedure btnGoSClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnSaveSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure labelUpdateClick(Sender: TObject);
    procedure textURLKeyPress(Sender: TObject; var Key: char);
    procedure updatesTimerTimer(Sender: TObject);
  private
    { private declarations }
    appdir: String;
    bgPanel: TXiPanel;
    btnAbout: TXiButton;
    btnSave: TXiButton;
    btnClear: TXiButton;
    btnGo: TXiButton;
    btnCopy: TXiButton;
    updatePanel: TXiPanel;
    workPanel: TXiPanel;
    btnCancel: TXiButton;
    scan: TScanThread;
    startTime: TDateTime;
    endTime: TDateTime;
    Links: TList;
    procedure setTheme;
    procedure positionPanel;
    procedure addURL(url: String);
    procedure loadConfig;
    procedure saveConfig;
    procedure applyConfig;
    procedure ScanThreadItemScanned(l: TLinkItem);
    procedure ScanCountUpdate(c: Integer);
    procedure ScanTerminated(Sender: TObject);
  public
    { public declarations }
    options: TConfigOptions;
  end;

var
  frmMain: TfrmMain;

implementation

uses options, about;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Установка начальных переменных
  frmMain.Caption := 'Open Sitemap Builder '+APPVER + ' от КонтинентСвободы.рф';
  Application.Title := frmMain.Caption;
  StatusBar1.SimpleText := '';
  appdir := GetUserDir + '.ssm' + PathDelim;
  labelCount.Caption := '';
  // Провяем каталог настроек программы, если он не существует, создаем его
  try
    if not DirectoryExists(appdir) then mkdir(appdir);
  except
    // По умолчанию exe каталог, если не удается создать appdir
    appdir := ExtractFilePath(Application.ExeName);
  end;
  // Загрузить историю URL в поле URL
  if FileExists(appdir + 'history.txt') then textURL.Items.LoadFromFile(appdir + 'history.txt');
  textURL.Text := 'http://';
  // Создать bgpanel
  bgPanel := TXiPanel.Create(self);
  bgPanel.Parent := frmMain;
  bgPanel.Align := alTop;
  bgPanel.Height := bgPanelS.Height;
  bgPanel.ColorScheme := XiPanel.csSky;
  bgPanel.BevelOuter := bvNone;
  // Создать кнопки
  btnAbout := TXiButton.Create(Self);
  btnAbout.Parent := bgPanel;
  btnAbout.Left := btnAboutS.Left;
  btnAbout.Width := 75;
  btnAbout.Height := 25;
  btnAbout.Top := btnAboutS.Top;
  btnAbout.Caption := 'О программе';
  btnAbout.ColorScheme := csNeoSky;
  btnAbout.Visible := true;
  btnAbout.BringToFront;
  btnAbout.OnClick := btnAboutSClick;
  btnCopy := TXiButton.Create(Self);
  btnCopy.Parent := bgPanel;
  btnCopy.Left := btnCopyS.Left;
  btnCopy.Width := 75;
  btnCopy.Height := 25;
  btnCopy.Top := btnCopyS.Top;
  btnCopy.Caption := 'Копировать';
  btnCopy.ColorScheme := csNeoSky;
  btnCopy.Visible := true;
  btnCopy.BringToFront;
  btnCopy.OnClick := btnCopySClick;
  btnClear := TXiButton.Create(Self);
  btnClear.Parent := bgPanel;
  btnClear.Left := btnClearS.Left;
  btnClear.Width := 75;
  btnClear.Height := 25;
  btnClear.Top := btnClearS.Top;
  btnClear.Caption := 'Очистить';
  btnClear.ColorScheme := csNeoSky;
  btnClear.Visible := true;
  btnClear.BringToFront;
  btnClear.OnClick := btnClearSClick;
  btnSave := TXiButton.Create(Self);
  btnSave.Parent := bgPanel;
  btnSave.Left := btnSaveS.Left;
  btnSave.Width := 75;
  btnSave.Height := 25;
  btnSave.Top := btnSaveS.Top;
  btnSave.Caption := 'Сохранить';
  btnSave.ColorScheme := csNeoSky;
  btnSave.Visible := true;
  btnSave.BringToFront;
  btnSave.OnClick := btnSaveSClick;
  btnGo := TXiButton.Create(Self);
  btnGo.Parent := bgPanel;
  btnGo.Left := btnGoS.Left;
  btnGo.Width := 75;
  btnGo.Height := 25;
  btnGo.Top := btnGoS.Top;
  btnGo.Caption := 'Начать';
  btnGo.ColorScheme := csNeoGrass;
  btnGo.Visible := true;
  btnGo.BringToFront;
  btnGo.OnClick := btnGoSClick;
  workPanel := TXiPanel.Create(Self);
  workPanel.Parent := frmMain;
  workPanel.Width := 170;
  workPanel.Height := 96;
  workPanel.Top := 0;
  workPanel.Left := 0;
  workPanel.ColorScheme := XiPanel.csGrass;
  workPanel.Visible := false;
  labelCount.Parent := workPanel;
  workPanelS.Width := 170;
  workPanelS.Height := 96;
  workPanelS.Visible := false;
  workPanelS.Parent := frmMain;
  btnCancel := TXiButton.Create(Self);
  btnCancel.Parent := workPanel;
  btnCancel.Width := 75;
  btnCancel.Height := 25;
  btnCancel.Left := (workPanel.Width div 2) - (btnCancel.Width div 2);
  btnCancel.Top := workPanel.Height - btnCancel.Height - 2;
  btnCancel.Caption := 'Отмена';
  btnCancel.ColorScheme := csNeoRose;
  btnCancel.Visible := true;
  btnCancel.BringToFront;
  btnCancel.OnClick := btnCancelSClick;
  btnCancelS.Width := 75;
  btnCancelS.Height := 25;
  btnCancelS.Left := (workPanelS.Width div 2) - (btnCancelS.Width div 2);
  btnCancelS.Top := workPanelS.Height - btnCancelS.Height - 2;
  updatePanel := TXiPanel.Create(self);
  updatePanel.Parent := frmMain;
  updatePanel.Top := 1;
  updatePanel.Height := 48;
  updatePanel.Width := 170;
  updatePanel.Left := 318;
  updatePanel.ColorScheme := XiPanel.csRose;
  updatePanel.BorderStyle := bsNone;
  labelUpdate.Parent := upDatePanel;
  labelUpdate.Align := alClient;
  labelUpdate.Alignment := taCenter;
  labelUpdate.Layout := tlCenter;
  labelUpdate.Caption := 'Доступна новая версия'#13#10'Нажмите здесь, чтобы получить ее';
  updatePanel.Visible := false;
  loadConfig;
  applyConfig;
  setTheme;
  updatesTimer.Enabled := true;
  textCSV.Font.Color := clBlack;
  // Очистить текст
  textHTML.Clear;
  textXML.Clear;
  textCSV.Clear;
  textList.Clear;
  // Установить вкладку по умолчанию
  PageControl1.ActivePage := tabXML;
  SetDefaultTheme(HTMLHighlighter);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  positionPanel;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  // Сбросить положение окна при использовании нескольких экранов
  if Screen.MonitorCount > 1 then
  begin
    frmMain.Left := (Screen.Monitors[0].Width div 2) - (frmMain.Width div 2);
  end;
  {$ENDIF}
end;

procedure TfrmMain.labelUpdateClick(Sender: TObject);
begin
  OpenURL('https://xn--90abhbolvbbfgb9aje4m.xn--p1ai/');
  updatePanel.Visible := false;
end;

procedure TfrmMain.textURLKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then btnGoSClick(Sender);
end;

procedure TfrmMain.updatesTimerTimer(Sender: TObject);
var
  response: String;
  header: String;
  newVer: Boolean;
begin
  updatesTimer.Enabled := false;
  // Проверить наличие новой версии, сравнивая переменную CURRVER с возвращенным значением
  newVer := false;
  header := '';
  response := '';
  try
    getURL('https://xn--90abhbolvbbfgb9aje4m.xn--p1ai/images/OpenSitemapBuilder.txt',defaultUserAgent,response,header);
    response := trim(response);
    if CURRVER < StrToInt(response) then newVer := true;
  except
    newVer := false;
  end;
  if newVer then
  begin
    updatePanel.Visible := true;
    updatePanel.Align := alBottom;
    updatePanel.Left := frmMain.ClientWidth - updatePanel.Width;
  end;
end;

procedure TfrmMain.btnAboutSClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.btnCancelSClick(Sender: TObject);
begin
  scan.stop := true;
  workPanel.Caption := 'Завершение';
  Application.ProcessMessages;
end;

procedure TfrmMain.btnClearSClick(Sender: TObject);
begin
  textHTML.Lines.Clear;
  textXML.Lines.Clear;
  textCSV.Lines.Clear;
  textList.Lines.Clear;
  StatusBar1.SimpleText := '';
end;

procedure TfrmMain.btnCopySClick(Sender: TObject);
begin
  if PageControl1.ActivePage = tabHTML then
  begin
    textHTML.SelectAll;
    textHTML.CopyToClipboard;
  end;
  if PageControl1.ActivePage = tabXML then
  begin
    textXML.SelectAll;
    textXML.CopyToClipboard;
  end;
  if PageControl1.ActivePage = tabCSV then
  begin
    textCSV.SelectAll;
    textCSV.CopyToClipboard;
  end;
  if PageControl1.ActivePage = tabList then
  begin
    textList.SelectAll;
    textList.CopyToClipboard;
  end;
  StatusBar1.SimpleText := 'Скопировано в буфер обмена.';
end;

procedure TfrmMain.btnGoSClick(Sender: TObject);
var
  U: TURIParser;
  r: Integer;
  h: TStrings;
  s: String;
  newurl: String;
begin
  Links := TList.Create;
  // Запустить часы!
  startTime := Now;
  // Сброс переменных
  scan := TScanThread.Create(true);
  scan.FreeOnTerminate := true;
  scan.OnCountUpdate := ScanCountUpdate;
  scan.OnTerminate := ScanTerminated;
  scan.OnScanItemComplete := ScanThreadItemScanned;
  StatusBar1.SimpleText := '';
  // Отключить все кнопки, кроме Отмены
  textURL.Enabled := false;
  textHTML.Enabled := false;
  if not options.disableCustomTheme then
  begin
    workPanel.Caption := 'Пожалуйста, подождите';
    workPanel.Visible := true;
    workPanel.BringToFront;
    btnGo.Enabled := false;
    btnSave.Enabled := false;
    btnClear.Enabled := false;
    btnCopy.Enabled := false;
    labelCount.Parent := workPanel;
    labelCount.Align := alTop;
    labelCount.BringToFront;
    btnCancel.Visible := true;
  end
  else
  begin
    workPanelS.Caption := 'Пожалуйста, подождите';
    workPanelS.Visible := true;
    workPanelS.BringToFront;
    btnGoS.Enabled := false;
    btnSaveS.Enabled := false;
    btnClearS.Enabled := false;
    btnCopyS.Enabled := false;
    labelCount.Parent := workPanelS;
    labelCount.Align := alTop;
    labelCount.BringToFront;
    btnCancelS.Visible := true;
  end;
  positionPanel;
  PageControl1.Enabled := false;
  btnOptions.Enabled := false;
  // Если введенный URL не имеет конечного / добавочного
  if (AnsiRightStr(textURL.Text,1) <> '/') then
  begin
    U:=TURIParser.Create(Nil);
    U.ParseURI(textURL.Text);
    if U.Document = '' then textURL.Text := textURL.Text + '/';
    U.Free;
  end;
  // Проверяем не перенаправлены ли мы на HTTPS
  h := TStringList.Create;
  r := 0;
  GetHead(textURL.Text,options.currentUserAgent,r,s);
  h.Text := s;
  if (r = 301) or (r = 302) then
  begin
    newurl := GetLocation(h);
    if CompareURLs(textURL.Text, newurl) = 0 then
    textURL.Text := newurl;
  end;
  h.Free;
  // Добавить URL в выпадающий список истории
  addURL(textURL.Text);
  // обновление UI
  Application.ProcessMessages;
  // Обработать сайт
  scan.UserAgent := options.currentUserAgent;
  scan.baseURL := textURL.Text;
  scan.ignoreFiles.AddStrings(options.ignoreFiles);
  scan.Start;
end;

procedure TfrmMain.ScanTerminated(Sender: TObject);
var
  l: ^TLinkItem;
  commentText: TStrings;
  ss: String;
  tmp: String;
  x: Integer;
begin
  commentText := TStringList.Create;
  // Остановить часы!
  endTime := Now;
  // Сортировать ссылки
  Links.Sort(@sortLinks);
  // Создание комментарие текстовых заголовков
  DateTimeToString(ss,'yyyy-mm-dd hh:nn',startTime);
  commentText.Add('<!-- Карта сайта для '+textURL.Text+' создана в ' + ss + '-->');
  tmp := '';
  DateTimeToString(ss,'hh',(endTime-startTime));
  tmp := tmp + ss + 'h ';
  DateTimeToString(ss,'nn',(endTime-startTime));
  tmp := tmp + ss + 'm ';
  DateTimeToString(ss,'ss',(endTime-startTime));
  tmp := tmp + ss + 's';
  commentText.Add('<!-- Количество обнаруженных ссылок: ' + IntToStr(Links.Count) + ', время выполнения: '+tmp+' -->');
  // Очистить редактор HTML
  textHTML.Lines.Clear;
  textHTML.Lines.AddStrings(commentText);
  // Создать неупорядоченный список
  textHTML.Lines.Add('<ul>');
  // Очистить редактор XML
  textXML.Lines.Clear;
  // Создать заголовок XML
  textXML.Lines.Add('<?xml version="1.0" encoding="UTF-8"?>');
  textXML.Lines.AddStrings(commentText);
  textXML.Lines.Add('<urlset xmlns="http://www.google.com/schemas/sitemap/0.84"');
  textXML.Lines.Add('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
  textXML.Lines.Add('xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84');
  textXML.Lines.Add('http://www.google.com/schemas/sitemap/0.84/sitemap.xsd">');
  // Очистить редактор CSV
  textCSV.Lines.Clear;
  // Очистить редактор списка
  textList.Lines.Clear;
  // Переходит по ссылкам
  for x := 0 to Links.Count -1 do
  begin
    // Это заставляет программу отвечать
    Application.ProcessMessages;
    l := Links[x];
    if l^.rescode = 200 then
    begin
      // Добавить элемент маркера с информацией о текущей ссылке
      textHTML.Lines.Add('  <li><a href="'+l^.link+'">'+l^.title+'</a></li>');
      // Добавить элемент XML
      textXML.Lines.Add('  <url>');
      textXML.Lines.Add('    <loc>'+l^.link+'</loc>');
      textXML.Lines.Add('    <lastmod>'+l^.modtime+'</lastmod>');
      textXML.Lines.Add('  </url>');
      // Добавить элемент CSV
      textCSV.Lines.Add('"'+l^.link+'","'+l^.title+'"');
      // Добавить элемент списка
      textList.Lines.Add(l^.link);
    end;
  end;
  // Указать информационный заголовок
  StatusBar1.SimpleText := 'Количество обнаруженных ссылок: ' + IntToStr(Links.Count);
  if scan.stop then StatusBar1.SimpleText := 'Отмена действий.';
  // Закрыть список тегов
  textHTML.Lines.Add('</ul>');
  // Конец XML
  textXML.Lines.Add('</urlset>');
  // Повторно включить стандартные элементы управления
  textURL.Enabled := true;
  textHTML.Enabled := true;
  if not options.disableCustomTheme then
  begin
    workPanel.Visible := false;
    btnGo.Enabled := true;
    btnSave.Enabled := true;
    btnClear.Enabled := true;
    btnCopy.Enabled := true;
  end
  else
  begin
    workPanelS.Visible := false;
    btnGoS.Enabled := true;
    btnSaveS.Enabled := true;
    btnClearS.Enabled := true;
    btnCopyS.Enabled := true;
  end;
  Application.ProcessMessages;
  PageControl1.Enabled := true;
  btnOptions.Enabled := true;
  commentText.Free;
  Links.Free;
end;

procedure TfrmMain.ScanCountUpdate(c: Integer);
begin
  labelCount.Caption := 'Ссылок: ' + IntToStr(c);
  Application.ProcessMessages;
end;

procedure TfrmMain.ScanThreadItemScanned(l: TLinkItem);
var
  p: ^TLinkItem;
begin
  new(p);
  //p := @l;
  p^.rescode := l.rescode;
  p^.link := l.link;
  p^.modtime := l.modtime;
  p^.title := l.title;
  Links.Add(p);
  Application.ProcessMessages;
end;

procedure TfrmMain.btnOptionsClick(Sender: TObject);
begin
  frmOptions.checkDisableTheme.Checked := options.disableCustomTheme;
  if options.userAgents.Count > 0 then
  begin
    frmOptions.textUserAgent.Items.Clear;
    frmOptions.textUserAgent.Items.AddStrings(options.userAgents);
    frmOptions.textUserAgent.Text := options.currentUserAgent;
  end;
  if options.editorFont <> '' then
    frmOptions.textEditorFont.Text := options.editorFont
  else
    frmOptions.textEditorFont.Text := textHTML.Font.Name;
  if options.editorFontSize <> 0 then
    frmOptions.textEditorFontSize.Value := options.editorFontSize
  else
    frmOptions.textEditorFontSize.Value := textHTML.Font.Size;
  frmOptions.listIgnoreFiles.Clear;
  frmOptions.listIgnoreFiles.Items.AddStrings(options.ignoreFiles);
  frmOptions.checkClearHistory.Checked := false;
  if frmOptions.ShowModal = mrOK then
  begin
    options.disableCustomTheme := frmOptions.checkDisableTheme.Checked;
    options.editorFont := frmOptions.textEditorFont.Text;
    options.editorFontSize := frmOptions.textEditorFontSize.Value;
    options.ignoreFiles.Clear;
    options.ignoreFiles.AddStrings(frmOptions.listIgnoreFiles.Items);
    options.currentUserAgent := frmOptions.textUserAgent.Text;
    options.userAgents.Clear;
    options.userAgents.AddStrings(frmOptions.textUserAgent.Items);
    if not InTStrings(DefaultUserAgent, options.userAgents) then
      options.userAgents.Add(DefaultUserAgent);
    saveConfig;
    applyConfig;
    setTheme;
    if frmoptions.checkClearHistory.Checked then
    begin
      textURL.Items.Clear;
      textURL.Items.SaveToFile(appdir + 'history.txt');
    end;
  end;
end;

procedure TfrmMain.btnSaveSClick(Sender: TObject);
begin
  if PageControl1.ActivePage = tabHTML then
  begin
    SaveDialog1.Filter := 'HTML Файлы (*.html)|*.html|Все файлы (*.*)|*.*';
    if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
    begin
      textHTML.Lines.SaveToFile(SaveDialog1.FileName);
      StatusBar1.SimpleText := 'Сохранить в ' + SaveDialog1.FileName + '.';
    end;
  end;
  if PageControl1.ActivePage = tabXML then
  begin
    SaveDialog1.Filter := 'XML Файлы (*.xml)|*.xml|Все файлы (*.*)|*.*';
    if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
    begin
      textXML.Lines.SaveToFile(SaveDialog1.FileName);
      StatusBar1.SimpleText := 'Сохранить в ' + SaveDialog1.FileName + '.';
    end;
  end;
  if PageControl1.ActivePage = tabCSV then
  begin
    SaveDialog1.Filter := 'CSV Файлы (*.csv)|*.csv|Все файлы (*.*)|*.*';
    if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
    begin
      textCSV.Lines.SaveToFile(SaveDialog1.FileName);
      StatusBar1.SimpleText := 'Сохранить в ' + SaveDialog1.FileName + '.';
    end;
  end;

  if PageControl1.ActivePage = tabList then
  begin
    SaveDialog1.Filter := 'Текстовые файлы (*.txt)|*.txt|Все файлы (*.*)|*.*';
    if (SaveDialog1.Execute) and (SaveDialog1.FileName <> '') then
    begin
      textList.Lines.SaveToFile(SaveDialog1.FileName);
      StatusBar1.SimpleText := 'Сохранить в ' + SaveDialog1.FileName + '.';
    end;
  end;
end;

procedure TfrmMain.setTheme;
begin
  if options.disableCustomTheme then
  begin
    bgPanel.Visible := false;
    bgPanelS.Visible := true;
    Label1.Parent := bgPanelS;
    textURL.Parent := bgPanelS;
    labelCount.Parent := workPanelS;
    btnOptions.Parent := bgPanelS;
  end
  else
  begin
    bgPanel.Visible := true;
    bgPanelS.Visible := false;
    Label1.Parent := bgPanel;
    textURL.Parent := bgPanel;
    labelCount.Parent := workPanel;
    btnOptions.Parent := bgPanel;
  end;
end;

procedure TfrmMain.loadConfig;
var
  conf: TIniFile;
  i: integer;
  arr: TArray;
  s: String;
begin
  options.disableCustomTheme := false;
  options.ignoreFiles := TStringList.Create;
  options.userAgents := TStringList.Create;
  s := '.png,.jpg,.gif,.mp3,.wav,.exe,.bin,.zip,.gz,.bz2,.dmg,.rar,.7z,.arj,.tar,.ogg,.avi,.mp4';
  if FileExists(appdir + 'ssmc.ini') then
  begin
    conf := TIniFile.Create(appdir + 'ssmc.ini');
    options.editorFont := conf.ReadString('program','editorfont','');
    options.editorFontSize := conf.ReadInteger('program','editorfontsize',0);
    options.disableCustomTheme :=  conf.ReadBool('program','disabletheme',false);
    options.currentUserAgent := conf.ReadString('program','useragent',defaultUserAgent);
    if (AnsiPos('Open Sitemap Builder', options.currentUserAgent) > 0) and (AnsiPos(APPVER, options.currentUserAgent) < 1) then
      options.currentUserAgent := DefaultUserAgent;
    s := conf.ReadString('program','ignoredfiles',s);
    arr := explode(',',s,0);
    for i := 0 to High(arr) do
      options.ignoreFiles.Add(arr[i]);
    conf.Free;
  end
  else
  begin
    options.editorFont := '';
    options.editorFontSize := 10;
    options.disableCustomTheme := true;
    arr := explode(',',s,0);
    for i := 0 to High(arr) do
      options.ignoreFiles.Add(arr[i]);
  end;
  if FileExists(appdir + 'ua.txt') then
  begin
    options.userAgents.LoadFromFile(appdir + 'ua.txt');
    if not InTStrings(DefaultUserAgent, options.userAgents) then
      options.userAgents.Add(DefaultUserAgent);
  end
  else
  begin
    options.userAgents.Add(defaultUserAgent);
  end;
end;

procedure TfrmMain.saveConfig;
var
  conf: TIniFile;
  ignoredFilesCSV: String;
begin
  conf := TIniFile.Create(appdir + 'ssmc.ini');
  conf.WriteString('program','editorfont',options.editorFont);
  conf.WriteInteger('program','editorfontsize',options.editorFontSize);
  conf.WriteBool('program','disabletheme',options.disableCustomTheme);
  conf.WriteString('program','useragent',options.currentUserAgent);
  // Filter out CRs from Windows-based offerings
  ignoredFilesCSV := AnsiReplaceStr(options.ignoreFiles.Text,#13,'');
  ignoredFilesCSV := AnsiReplaceStr(ignoredFilesCSV,#10,',');
  conf.WriteString('program','ignoredfiles',TrimRightSet(ignoredFilesCSV,[',']));
  conf.Free;
  options.userAgents.SaveToFile(appdir + 'ua.txt');
end;

procedure TfrmMain.applyConfig;
begin
  if options.editorFont <> '' then
  begin
    textHTML.Font.Name := options.editorFont;
    textXML.Font.Name := options.editorFont;
    textCSV.Font.Name := options.editorFont;
    textList.Font.name := options.editorFont;
  end;
  if options.editorFontSize > 0 then
  begin
    textHTML.Font.Size := options.editorFontSize;
    textXML.Font.Size := options.editorFontSize;
    textCSV.Font.Size := options.editorFontSize;
    textList.Font.Size := options.editorFontSize;
  end;
end;

{ Add URL to the drop down list }
procedure TfrmMain.addURL(url: String);
var
  x: integer;
  add: boolean;
begin
  add := true;
  // Check to see if the URL already exists
  for x := 0 to textURL.Items.Count-1 do
  begin
    if textURL.Items[x] = url then add := false;
  end;
  // If the URL was not found add it to the list
  if add then
  begin
    textURL.Items.Add(textURL.Text);
    textURL.Items.SaveToFile(appdir + 'history.txt');
  end;
end;

{ Position the status panel at the centre of the form }
procedure TfrmMain.positionPanel;
begin
  if updatePanel.Visible then
    updatePanel.Left := (frmMain.ClientWidth - updatePanel.Width) - 5;
  if not options.disableCustomTheme then
    with workPanel do
    begin
      Left := (frmMain.Width div 2) - (Width div 2);
      Top := (frmMain.Height div 2) - (Height div 2);
    end;
  if options.disableCustomTheme then
    with workPanelS do
    begin
      Left := (frmMain.Width div 2) - (Width div 2);
      Top := (frmMain.Height div 2) - (Height div 2);
    end;
end;

end.


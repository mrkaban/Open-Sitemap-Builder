unit options;
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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, Spin, Buttons, ExtCtrls, Menus;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    checkClearHistory: TCheckBox;
    checkDisableTheme: TCheckBox;
    menuRemoveItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    textFileType: TEdit;
    Panel1: TPanel;
    textUserAgent: TComboBox;
    Label3: TLabel;
    textEditorFont: TEditButton;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    listIgnoreFiles: TListBox;
    textEditorFontSize: TSpinEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure listIgnoreFilesClick(Sender: TObject);
    procedure menuRemoveItemClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure textEditorFontButtonClick(Sender: TObject);
    procedure textFileTypeEnter(Sender: TObject);
    procedure textFileTypeKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    oldText: String;
  public
    { public declarations }
    procedure addUserAgent(ua: String);
  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.lfm}

procedure TfrmOptions.textEditorFontButtonClick(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    textEditorFont.Text := FontDialog1.Font.Name;
    if textEditorFontSize.Value <> FontDialog1.Font.Size then
      textEditorFontSize.Value := FontDialog1.Font.Size;
  end;
end;

procedure TfrmOptions.textFileTypeEnter(Sender: TObject);
begin
  oldText := textFileType.Text;
end;

procedure TfrmOptions.textFileTypeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if listIgnoreFiles.ItemIndex > -1 then
    begin
      if listIgnoreFiles.Items[listIgnoreFiles.ItemIndex] = oldText then
        listIgnoreFiles.Items[listIgnoreFiles.ItemIndex] := textFileType.Text
      else
        listIgnoreFiles.Items.Add(textFileType.Text);
    end
    else
      listIgnoreFiles.Items.Add(textFileType.Text);
    textFileType.Text := '';
    oldText := '';
  end;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  addUserAgent(textUserAgent.Text);
  ModalResult := mrOK;
end;

procedure TfrmOptions.listIgnoreFilesClick(Sender: TObject);
begin
  if listIgnoreFiles.ItemIndex > -1 then
  begin
    textFileType.Text := listIgnorefiles.Items[listIgnoreFiles.ItemIndex];
  end;
end;

procedure TfrmOptions.menuRemoveItemClick(Sender: TObject);
begin
  if listIgnoreFiles.ItemIndex > -1 then
    listIgnoreFiles.Items.Delete(listIgnoreFiles.ItemIndex);
end;

procedure TfrmOptions.PopupMenu1Popup(Sender: TObject);
begin
  if listIgnoreFiles.ItemIndex < 0 then menuRemoveItem.Enabled := false
  else menuRemoveItem.Enabled := true;
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  addUserAgent(textUserAgent.Text);
  ModalResult := mrCancel;
end;

procedure TfrmOptions.addUserAgent(ua: String);
var
  i: integer;
  add: boolean;
begin
  add := true;
  if textUserAgent.Items.Count > 0 then
  begin
    for i := 0 to textUserAgent.Items.Count -1 do
    begin
      if textUserAgent.Items[i] = ua then add := false;
    end;
    if add then
    begin
      textUserAgent.Items.Insert(0,ua);
    end;
  end
  else
    textUserAgent.Items.Add(ua);
end;

end.


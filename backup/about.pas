unit about;
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLIntF, ExtCtrls, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnOK: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    imgSF: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure imgSFClick(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.Image1Click(Sender: TObject);
begin
  OpenURL('http://www.lazarus.freepascal.org');
end;

procedure TfrmAbout.Image2Click(Sender: TObject);
begin
  OpenURL('https://twitter.com/hippy2094');
end;

procedure TfrmAbout.Image3Click(Sender: TObject);
begin
  OpenURL('https://xn--90abhbolvbbfgb9aje4m.xn--p1ai/');
end;

procedure TfrmAbout.imgSFClick(Sender: TObject);
begin
  OpenURL('https://sourceforge.net/p/simplesitemapcreator/');
end;

procedure TfrmAbout.Label4Click(Sender: TObject);
begin
  OpenURL('http://www.deadlogic.co.nr');
end;

procedure TfrmAbout.Label5Click(Sender: TObject);
begin
  OpenURL('https://xn--90abhbolvbbfgb9aje4m.xn--p1ai/');
end;

procedure TfrmAbout.Label7Click(Sender: TObject);
begin
  OpenUrl('http://mythcode.org');
end;

procedure TfrmAbout.Label8Click(Sender: TObject);
begin
  OpenUrl('http://www.ararat.cz/synapse/');
end;

procedure TfrmAbout.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Label5.Left := (Label2.Left + Label2.Width) + Label2.Canvas.GetTextWidth(' ');
end;

end.


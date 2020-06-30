unit miscfunc;
(*
Simple Sitemap Creator

Copyright (C) 2010 Matthew Hipkin <http://www.matthewhipkin.co.uk>
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
  Classes, SysUtils, httpsend, ssl_openssl, {$IFDEF MSWINDOWS} Windows,{$ENDIF}
  StrUtils, Graphics, SynHighlighterHTML, typinfo, resolve;

type
  TArray = array of string;
  TLinkItem = record
    title: String;
    link: String;
    referrer: String;
    modtime: String;
    parsed: Boolean;
    rescode: integer;
  end;
  TConfigOptions = record
    editorFont: String;
    editorFontSize: Integer;
    disableCustomTheme: Boolean;
    currentUserAgent: String;
    userAgents: TStrings;
    ignoreFiles: TStrings;
  end;

const
  APPVER = '1.3.4';
  CURRVER = 20171119;

{$IFDEF MSWINDOWS}function getWinVer: String;{$ENDIF}
function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;
function implode(cDelimiter: String; arr: TArray): String;
function sortLinks(Item1, Item2: Pointer): Integer;
function InTStrings(s: String; h: TStrings): Boolean;
procedure saveDebug(s: String);
function getURL(url: String; userAgent: String; var response: String; var headers: String): integer;
function getHEAD(url: String; userAgent: String): integer;
procedure GetHead(url: String; UserAgent: String; var response: Integer; var headers: String); overload;
function MonthToNum(s: String): String;
function getDate(header: String): String;
function defaultUserAgent: String;
function HTMLToColour(sColor: string): TColor;
procedure SetDefaultTheme(var highlighter: TSynHTMLSyn);
function GetLocation(headers: TStrings): String;
function CompareURLs(url1, url2: String): Integer;

implementation

{$IFDEF MSWINDOWS}
function getWinVer: String;
var
  VerInfo: TOSVersioninfo;
  nt: String;
begin
  nt := '';
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  if VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then nt := 'NT ';
  Result := 'Windows '+nt+IntToStr(VerInfo.dwMajorVersion) + '.' + IntToStr(VerInfo.dwMinorVersion);
end;
{$ENDIF}

function explode(cDelimiter,  sValue : string; iCount : integer) : TArray;
var
  s : string;
  i,p : integer;
begin
  s := sValue; i := 0;
  while length(s) > 0 do
  begin
    inc(i);
    SetLength(result, i);
    p := pos(cDelimiter,s);
    if ( p > 0 ) and ( ( i < iCount ) OR ( iCount = 0) ) then
    begin
      result[i - 1] := copy(s,0,p-1);
      s := copy(s,p + length(cDelimiter),length(s));
    end else
    begin
      result[i - 1] := s;
      s :=  '';
    end;
  end;
end;

function implode(cDelimiter: String; arr: TArray): String;
var
  i: integer;
begin
  Result := '';
  for i := Low(arr) to High(arr) do
  begin
    Result := Result + arr[i] + cDelimiter;
  end;
  Result := TrimRightSet(Result,[' ',cDelimiter[1]]);
end;

function sortLinks(Item1, Item2: Pointer): Integer;
var
  Comp1: ^TLinkItem absolute Item1;
  Comp2: ^TLinkItem absolute Item2;
begin
  { This needs to be more complex if I am to sort by subdirectory }
  Result := CompareText(Comp1^.link,Comp2^.link);
end;

function InTStrings(s: String; h: TStrings): Boolean;
var
  i: integer;
begin
  Result := false;
  for i :=0 to h.Count -1 do
  begin
    if s = h[i] then Result := true;
  end;
end;

procedure saveDebug(s: String);
var
  dFile: TStrings;
  f: String;
begin
  f := 'debug.txt';
  dFile := TStringList.Create;
  if FileExists(f) then dFile.LoadFromFile(f);
  dFile.Add(s);
  dFile.SaveToFile(f);
  dFile.Free;
end;

{ Get the contents of specified link }
function getURL(url: String; userAgent: String; var response: String; var headers: String): integer;
var
  http: THTTPSend;
  l: TStrings;
  newurl: String;
begin
  http := THTTPSend.Create;
  l := TStringList.Create;
  if Length(userAgent) > 0 then http.UserAgent := userAgent
  else http.UserAgent := defaultUserAgent;
  http.HTTPMethod('GET', url);
  if http.ResultCode = 200 then
  begin
    l.LoadFromStream(Http.Document);
    response := l.Text;
    headers := http.Headers.Text;
  end
  else response := '';
  Result := http.ResultCode;
  http.Free;
  l.Free;
end;

function getHEAD(url: String; userAgent: String): integer;
var
  http: THTTPSend;
begin
  http := THTTPSend.Create;
  http.UserAgent := userAgent;
  http.HTTPMethod('HEAD', url);
  Result := http.ResultCode;
  http.Free;
end;

procedure GetHead(url: String; UserAgent: String; var response: Integer; var headers: String); overload;
var
  http: THTTPSend;
begin
  http := THTTPSend.Create;
  http.UserAgent := userAgent;
  http.HTTPMethod('HEAD', url);
  response := http.ResultCode;
  headers := http.Headers.Text;
  http.Free;
end;

function MonthToNum(s: String): String;
type
  months = (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
var
  m: integer;
begin
  m := GetEnumValue(typeinfo(months),s) + 1;
  if m < 10 then Result := '0' + IntToStr(m)
  else Result := IntToStr(m);
end;

function getDate(header: String): String;
var
  lines: TStrings;
  x: integer;
  modline: String;
  ele: TArray;
  today: String;
begin
  DateTimeToString(today,'yyyy-mm-dd',Now);
  lines := TStringList.Create;
  lines.Text := header;
  modline := '';
  for x := 0 to lines.Count -1 do
  begin
    if AnsiStartsStr('Last-Modified',lines[x]) then
    begin
      modline := lines[x];
      break;
    end;
  end;
  if modline <> '' then
  begin
    modline := Copy(modline,Length('Last-Modified') + 3, Length(modline) - Length('Last-Modified'));
    ele := explode(' ',trim(modline),0);
    ele[2] := MonthToNum(ele[2]);
    Result := ele[3] + '-' + ele[2] + '-' + ele[1] + ' ' + ele[4];
  end
  else Result := today;
  lines.Free;
end;

function defaultUserAgent: String;
var
  OS: String;
begin
  OS := 'Unknown';
  {$ifdef WINDOWS}
  OS := getWinVer;
  {$endif}
  {$ifdef LINUX}
  OS := 'Linux';
  {$endif}
  {$ifdef FREEBSD}
  OS := 'FreeBSD';
  {$endif}
  {$ifdef NETBSD}
  OS := 'NetBSD';
  {$endif}
  {$ifdef DARWIN}
  OS := 'Mac OS X';
  {$endif}
  {$ifdef SUNOS}
  OS := 'SunOS';
  {$endif}
  Result := 'Mozilla/4.0 (compatible; Simple Sitemap Creator '+APPVER+'; ' + OS + '; '+IntToStr(CURRVER)+'; +https://bit.do/simplesitemap)';
end;

function HTMLToColour(sColor: string): TColor;
begin
  Result :=
    RGBToColor(
      StrToInt( '$'+Copy( sColor, 2, 2 ) ),
      StrToInt( '$'+Copy( sColor, 4, 2 ) ),
      StrToInt( '$'+Copy( sColor, 6, 2 ) )
    );
end;

procedure SetDefaultTheme(var highlighter: TSynHTMLSyn);
begin
  highlighter.CommentAttri.Style := [fsItalic];
  highlighter.CommentAttri.Foreground := HTMLToColour('#C0C0C0');
  highlighter.CommentAttri.Background := clNone;
  highlighter.AndAttri.Style := [];
  highlighter.AndAttri.Foreground := HTMLToColour('#808040');
  highlighter.AndAttri.Background := clNone;
  highlighter.SymbolAttri.Style := [];
  highlighter.SymbolAttri.Foreground := HTMLToColour('#808040');
  highlighter.SymbolAttri.Background := clNone;
  highlighter.IdentifierAttri.Style := [];
  highlighter.IdentifierAttri.Foreground := HTMLToColour('#0000B9');
  highlighter.IdentifierAttri.Background := clNone;
  highlighter.DOCTYPEAttri.Style := [];
  highlighter.DOCTYPEAttri.Foreground := HTMLToColour('#0080FF');
  highlighter.DOCTYPEAttri.Background := clNone;
  highlighter.KeyAttri.Style := [];
  highlighter.KeyAttri.Foreground := HTMLToColour('#008080');
  highlighter.KeyAttri.Background := clNone;
  highlighter.UnDefKeyAttri.Style := [];
  highlighter.UnDefKeyAttri.Foreground := HTMLToColour('#008080');
  highlighter.UnDefKeyAttri.Background := clNone;
  highlighter.TextAttri.Style := [];
  //highlighter.TextAttri.Foreground := HTMLToColour('#0080C0');
  highlighter.TextAttri.Foreground := HTMLToColour('#000000');
  highlighter.TextAttri.Background := clNone;
  highlighter.ValueAttri.Style := [];
  highlighter.ValueAttri.Foreground := HTMLToColour('#800000');
  highlighter.ValueAttri.Background := clNone;
end;

function GetLocation(headers: TStrings): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to headers.Count -1 do
  begin
    if Copy(headers[i], 1, 8) = 'Location' then
      Result := Copy(headers[i], 11, Length(headers[i]) - 11);
  end;
end;

function CompareURLs(url1, url2: String): Integer;
var
  u1, u2: TURIParser;
begin
  u1 := TURIParser.Create(nil);
  u2 := TURIParser.Create(nil);
  u1.ParseURI(url1);
  u2.ParseURI(url2);
  Result := CompareText(u1.Host,u2.Host);
  u1.Free;
  u2.Free;
end;

end.


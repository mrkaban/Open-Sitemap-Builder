unit scanthread;
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
  Classes, SysUtils, StrUtils, resolve, XMLParser, miscfunc;

type
  TScanItemCompleteEvent = procedure(l: TLinkItem) of Object;
  TScanCountUpdateEvent = procedure(c: Integer) of Object;
  TScanThread = class(TThread)
    private
      FOnScanItemComplete: TScanItemCompleteEvent;
      FOnCountUpdate: TScanCountUpdateEvent;
      FLinkCount: Integer;
      FCurrentLink: TLinkItem;
      FHasFinished: Boolean;
      FUserAgent: String;
      FBaseURL: String;
      Links: TList;
      procedure addLink(link: String; title: String; ref: String; header: String);
      procedure SetStatus(url: String; code: integer);
      procedure setTitle(url: String; title: String);
      procedure parseLinks(url: String);
      procedure ScanItemComplete;
      procedure CountUpdate;
    protected
      procedure Execute; override;
    public
      stop: Boolean;
      ignoreFiles: TStrings;
      constructor Create(CreateSuspended: boolean);
      property OnScanItemComplete: TScanItemCompleteEvent read FOnScanItemComplete write FOnScanItemComplete;
      property OnCountUpdate: TScanCountUpdateEvent read FOnCountUpdate write FOnCountUpdate;
      property LinkCount: Integer read FLinkCount;
      property UserAgent: String read FUserAgent write FUserAgent;
      property BaseURL: String read FBaseURL write FBaseURL;
    end;
      
implementation

{ TScanThread }

constructor TScanThread.Create(CreateSuspended: Boolean);
begin
  FLinkCount := 0;
  Links := TList.Create;
  ignoreFiles := TStringList.Create;
  FHasFinished := false;
  stop := false;
  inherited Create(CreateSuspended);
end;

procedure TScanThread.ScanItemComplete;
begin
  if Assigned(FOnScanItemComplete) then
  begin
    FOnScanItemComplete(FCurrentLink);
  end;
end;

procedure TScanThread.CountUpdate;
begin
  if Assigned(FOnCountUpdate) then
  begin
    FOnCountUpdate(FLinkCount);
  end;
end;

{ПОЛУЧИТЬ данную ссылку и проанализировать HTML для получения дополнительных тегов A}
procedure TScanThread.parseLinks(url: String);
var
  Parser: TXMLParser;
  html: String;
  header: String;
  link: String;
  title: String;
  add: Boolean;
  x,i: integer;
  tmp: String;
  r: integer;
begin
  // Если нажата кнопка отмены, выход из процедуры
  if stop = true then
  begin
    links.Clear;
    FHasFinished := true;
    exit;
  end;
  html := '';
  header := '';
  // Игнорировать определенные типы файлов
  if InTStrings(ExtractFileExt(url),ignoreFiles) then exit;
  // Получить указанный URL
  r := getURL(url,FUserAgent,html,header);
  SetStatus(url,r);
  if r <> 200 then
  begin
    exit;
  end;
  // Указать переменные
  Parser := TXMLParser.Create(html);
  while Parser.Next do
  begin
    if Parser.TagType = ttBeginTag then
    begin
      if Lowercase(Parser.Name) = 'a' then
      begin
        link := trim(Parser.Value['href']);
      end;
      if Lowercase(Parser.Name) = 'title' then
        title := Parser.ContentSpaceTrimText;
    end;
    add := true;
    setTitle(url,title);
    if inTStrings(Lowercase(ExtractFileExt(link)),ignoreFiles) then add := false;
    if add = true then
    begin
      // Обрезать звенья анкоров и т.д.
      x := Pos('#',link);
      if x > 0 then
      begin
        tmp := '';
        for i := 1 to x-1 do
          tmp := tmp + link[i];
        link := tmp;
      end;
      if link <> '' then
      begin
        addLink(link,title,url,header);
      end;
    end;
  end;
  Parser.Free;
end;

{ Попытайтесь добавить ссылку в список разбора, проверяя, является ли она локальной ссылкой
и существует ли уже ссылка

   Мы передаем ссылающийся URL в надежде, что сможем рассчитать путь к файлу }
procedure TScanThread.addLink(link: String; title: String; ref: String; header: String);
var
  x,i,j: integer;
  l: ^TLinkItem;
  tmp: TArray;
  proto: String;
  tmps: String;
  U: TURIParser;
  refU: TURiParser;
  t: TStrings;
  //GoodChars: String;
  BadChars: String;
begin
  // Мы должны были остановиться?
  if stop = true then exit;
  // Методы URL, которые нам не интересны
  if AnsiStartsStr('mailto:',link) then exit;
  if AnsiStartsStr('javascript:',link) then exit;
  if AnsiStartsStr('skype:',link) then exit;
  if AnsiStartsStr('ftp:',link) then exit;
  if AnsiStartsStr('news:',link) then exit;
  if AnsiStartsStr('gopher:',link) then exit;
  if AnsiStartsStr('file:',link) then exit;
  if AnsiStartsStr('//',link) then exit;
  // Не интересные анкоры
  if AnsiStartsStr('#',link) then exit;
  // Не интересные пустые ссылки
  if link = '' then exit;
  // Не интересные, которые начинаются с недопустимых символов
  //GoodChars := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&''()*+,;=`.';
  //if AnsiPos(link[1],GoodChars) < 1 then exit;
  BadChars := '{}';
  if AnsiPos(link[1],BadChars) > 0 then exit;
  // Ссылка начинается с http: // или https: //
  if (AnsiStartsStr('http://',link) = true) or (AnsiStartsStr('https://',link) = true) then
  begin
    // Если так, то это тот же URL, который указан в textURL.Text?
    if AnsiStartsStr(baseURL,link) = false then exit;
  end
  // Он не начинается с http: // или https: //, но является локальной ссылкой
  else
  begin
    U := TURIParser.Create(Nil);
    refU := TURIParser.Create(nil);
    U.ParseURI(baseURL);
    refU.ParseURI(ref);
    if U.Document <> '' then
    begin
      tmps := AnsiReplaceStr(ref,refU.Document,link);
      link := tmps;
    end
    else
    begin
      if refU.Document <> '' then
      begin
        if not AnsiStartsStr('/',link) then
          tmps := AnsiReplaceStr(ref,refU.Document,link)
        else
        begin
          tmps := baseURL + Copy(link,2,Length(link)-1);
        end;
      end
      else if AnsiStartsStr('/',link) then
      begin
        tmps := baseURL + link;
      end
      else
      begin
        tmps := ref + link;
      end;
      link := tmps;
    end;
    U.Free;
    refU.Free;
  end;
  // Nasty way to clean up double slashes
  if Pos('://',link) > 0 then proto := Copy(link,1,Pos('://',link)+2)
  else proto := '/';
  tmp := explode('/',link,0);
  tmps := proto;
  for x := 1 to High(tmp) do
  begin
    if tmp[x] <> '' then tmps := tmps + tmp[x] + '/';
  end;
  if not AnsiEndsStr('/',link) then tmps := Copy(tmps,1,Length(tmps)-1);
  link := tmps;
  // Finally check for any relative paths
  link := AnsiReplaceStr(link,'./','');
  if Pos('..',link) > 0 then
  begin
    U := TURIParser.Create(nil);
    t := TStringList.Create;
    U.ParseURI(link);
    tmps := U.Path;
    tmp := explode('/',tmps,0);
    t.Clear;
    j := 0;
    for i := 0 to High(tmp) do
    begin
      j := t.Count;
      if tmp[i] <> '..' then t.Add(tmp[i])
      else t.Delete(j-1);
    end;
    tmps := '';
    for i := 0 to t.Count -1 do
    begin
      tmps := tmps + t[i] + '/';
    end;
    link := U.Protocol + '://' + U.Host + tmps + U.Document;
    U.Free;
  end;
  // Make sure link is not already in the list
  for x := 0 to links.Count -1 do
  begin
    l := links[x];
    if l^.link = link then exit;
  end;
  // Add the link info to the list of links to be parsed
  new(l);
  l^.title := title;
  l^.link := link;
  l^.rescode := -1;
  l^.referrer := ref;
  l^.parsed := false;
  l^.modtime := getDate(header);
  links.Add(l);
  // Set status caption to show number of links found
  if FLinkCount <> links.Count then
  begin
    FLinkCount := links.Count;
    Synchronize(@CountUpdate);
  end;
  // Parse link
  parseLinks(l^.link);
  if FCurrentLink.link <> l^.link then
  begin
    FCurrentLink := l^;
    Synchronize(@ScanItemComplete);
  end;
end;

procedure TScanThread.setTitle(url: String; title: String);
var
  i: integer;
  l: ^TLinkItem;
begin
  if links.Count > 0 then
  begin
    for i := 0 to links.Count -1 do
    begin
      l := links[i];
      if l^.link = url then break;
    end;
    l^.title := title;
  end;
end;

procedure TScanThread.SetStatus(url: String; code: integer);
var
  i: integer;
  l: ^TLinkItem;
begin
  if links.Count > 0 then
  begin
    for i := 0 to links.Count -1 do
    begin
      l := links[i];
      if l^.link = url then break;
    end;
    l^.rescode := code;
  end;
end;               

procedure TScanThread.Execute;
begin
  FHasFinished := false;
  parseLinks(FBaseURL);
  FHasFinished := true;
end;

end.

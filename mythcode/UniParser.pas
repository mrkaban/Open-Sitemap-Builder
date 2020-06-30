unit UniParser;
{$mode delphi}{$H+}
// Version 2.0
//
// License: http://www.mozilla.org/MPL/
// Site: http://www.mythcode.org
// Author: Dzianis Koshkin
// E-mail: dzianis.k@gmail.com
//
// (C) 2005-2008 MYTHcode.org

interface

uses SysUtils, Classes;

type

TUniParser = class
protected
  PInit: PChar;
  PFine: PChar;
  PItem: PChar;
  PNext: PChar;
  PStep: PChar;
public
  constructor Create(const S: PChar); overload;
  constructor Create(const S: string); overload;
  procedure Restart;
  function Next: boolean; overload;
  function Next(const S: Char): boolean; overload;
  function Next(const S: TSysCharSet): boolean; overload;
  function Next(const S: string): boolean; overload;
  function Next(const S: array of string; out Index: Integer): boolean; overload;
  function Next(const S: TStrings; out Index: Integer): boolean; overload;
  function Item: string;
  function Separator: string;
  function Step: string;
  function Head: string;
  function Tail: string;
  function TrimItem: string;
end;

implementation

const
  Nullator: Char = #0;

procedure TUniParser.Restart;
begin
  PItem:=PInit;
  PNext:=PInit;
  PStep:=PInit;
end;

constructor TUniParser.Create(const S: PChar);
begin
  if S=nil
  then PInit:=@Nullator
  else PInit:=S;
  PFine:=nil;
  Restart;
end;

constructor TUniParser.Create(const S: string);
begin
  Create(Pointer(S));
  PFine:=PInit+Length(S);
end;

function TUniParser.Next: boolean;
begin
  PItem:=PStep;
  PNext:=PItem;
  if PNext^<>#0 then
  begin
    Inc(PNext);
    PStep:=PNext;
    Result:=True;
    Exit;
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: Char): boolean;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    if PNext^=S then
    begin
      PStep:=PNext+1;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: TSysCharSet): boolean;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    if (PNext^ in S) then
    begin
      PStep:=PNext+1;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: string): boolean;
var
  L: Integer;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    L:=Length(S);
    if CompareMem(PNext,Pointer(S),L) then
    begin
      if L=0 then L:=1;
      PStep:=PNext+L;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: array of string; out Index: Integer): boolean;
var
  L: Integer;
procedure GetIndex;
begin
  Index:=Low(S);
  while Index<=High(S) do
  begin
    L:=Length(S[Index]);
    if CompareMem(PNext,Pointer(S[Index]),L) then Exit;
    Inc(Index);
  end;
  Index:=-1;
end;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    GetIndex;
    if Index<>-1 then
    begin
      if L=0 then L:=1;
      PStep:=PNext+L;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Next(const S: TStrings; out Index: Integer): boolean;
var
  L: Integer;
procedure GetIndex;
begin
  Index:=0;
  if S<>nil then
  while Index<S.Count do
  begin
    L:=Length(S[Index]);
    if CompareMem(PNext,Pointer(S[Index]),L) then Exit;
    Inc(Index);
  end;
  Index:=-1;
end;
begin
  PItem:=PStep;
  PNext:=PItem;
  while PNext^<>#0 do
  begin
    GetIndex;
    if Index<>-1 then
    begin
      if L=0 then L:=1;
      PStep:=PNext+L;
      Result:=True;
      Exit;
    end;
    Inc(PNext);
  end;
  PStep:=PNext;
  Result:=False;
end;

function TUniParser.Item: string;
begin
  SetLength(Result, PNext-PItem);
  Move(PItem^, Pointer(Result)^, Length(Result));
end;

function TUniParser.Step: string;
begin
  SetLength(Result, PStep-PItem);
  Move(PItem^, Pointer(Result)^, Length(Result));
end;

function TUniParser.TrimItem: string;
var
  Init: PChar;
  Fine: PChar;
begin
  Init:=PItem;
  Fine:=PNext-1;
  while (Init^<'!') do Inc(Init);
  while (Fine^<'!') do Dec(Fine);
  SetLength(Result, Succ(Fine-Init));
  Move(Init^, Pointer(Result)^, Length(Result));
end;

function TUniParser.Separator: string;
begin
  SetLength(Result, PStep-PNext);
  Move(PNext^, Pointer(Result)^, Length(Result));
end;

function TUniParser.Head: string;
begin
  SetLength(Result, PItem-PInit);
  Move(PInit^, Pointer(Result)^, Length(Result));
end;

function TUniParser.Tail: string;
begin
  if PFine=nil then
  begin
    PFine:=PStep;
    while PFine<>#0 do Inc(PFine);
  end;
  SetLength(Result, PFine-PStep);
  Move(PStep^, Pointer(Result)^, Length(Result));
end;

end.

unit XMLParser;
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

type

TTagType = (ttBeginTag, ttEndTag, ttTermTag, ttCommentTag, ttDefineTag, ttUnknownTag);
           {<tag>       </tag>    <tag/>     <! >          <? >         < >         }

TToken = record
  From: PChar;
  Before: PChar;
end;

TXMLParser = class
protected
  FDocument: string;
  PName: PChar;
  PSpace: PChar;
  PAttribute: PChar;
  PBefore: PChar;
  PInitial: PChar;
  PFinal: PChar;
  POpen: PChar;
  PClose: PChar;
  PNextOpen: PChar;
  PBeginSelected: PChar;
  PEndSelected: PChar;
  FTagType: TTagType;
  FLastToken: Integer;
  FTokens: array of TToken;
  FLastAttribute: Integer;
  FAttributes: array of Integer;
  procedure PreAttribute;
  procedure InitAttributes;
  procedure SetDocument(const S: string); overload;
  function GetLastAttribute: Integer;
  function GetTagCode: string;
  function GetTagText: string;
  function GetNameCode: string;
  function GetName: string;
  function GetSpace: string;
  function GetContentCode: string;
  function GetContentText: string;
  function GetContentTrimText: string;
  function GetContentSpaceTrimText: string;
  function GetStepCode: string;
  function GetHead: string;
  function GetTail: string;
  function GetSelected: string;
  function GetValue(Name: string): string;
  function GetAttribute(Index: Integer): string;
public
  Compare: function(const A, B: string): Integer;
  constructor Create(const S: string = '');
  function Next: boolean;
  procedure BeginSelect;
  procedure EndSelect;
  property Document: string read FDocument write SetDocument;
  property TagType: TTagType read FTagType;
  property Name: string read GetName;         // <[p]:html> Paragraph </p:html>
  property NameCode: string read GetNameCode; // <[p:html] id="1"> Paragraph </p:html>
  property Space: string read GetSpace;       // <p:[html]> Paragraph </p:html>
  property TagCode: string read GetTagCode;   // [<p id="1">] Paragraph </p:html>
  property TagText: string read GetTagText;   // <[p id="1"]> Paragraph </p:html>
  property ContentCode: string read GetContentCode; // <p>[ Paragraph ]</p>
  property ContentText: string read GetContentText; // <p>[ Paragraph ]</p>
  property ContentTrimText: string read GetContentTrimText; // <p>#13#10[ Paragraph ]#13#10</p>
  property ContentSpaceTrimText: string read GetContentSpaceTrimText; // <p> [Paragraph] </p>
  property StepCode: string read GetStepCode;
  property Head: string read GetHead;
  property Tail: string read GetTail;
  property Selected: string read GetSelected;
  property LastAttribute: Integer read GetLastAttribute;
  property Attribute[Index: Integer]: string read GetAttribute; //<p [id]="1"> Paragraph </p>
  property Value[Name: string]: string read GetValue; default; //<p id="[1]"> Paragraph </p>
end;

function HTMLDecode(const S: string): string;

implementation

uses SysUtils, SubUtils, UniParser;

function Get(const Token: TToken): string; overload;
begin
  with Token do
  begin
    SetLength(Result, Before-From);
    Move(From^, Pointer(Result)^, Length(Result));
  end;
end;

function CodeToChar(S: string; var C: string): Boolean;
var
  I: Integer;
  E: Integer;
begin
  Result:=False;
  if (Length(S)>1) and (S[1]='#') then
  begin
    Val(Sub(S,1), I, E);
    if (E=0) and (0<I) and (I<=255) then
    begin
      C:=Char(I);
      Result:=True;
    end
  end;
end;

function HTMLDecode(const S: string): string;
var
  UniParser: TUniParser;
  BeforeItem: string;
  FindItem: string;
  Lexem: string;
begin
  Result:='';
  UniParser:=TUniParser.Create(S);
  with UniParser do
  begin
    while Next('&') do
    begin
       BeforeItem:=Item;
       if Next(';') then
       begin
         FindItem:=Item;
         if FindItem='amp' then Lexem:=';' else
         if FindItem='lt' then Lexem:='<' else
         if FindItem='gt' then Lexem:='>' else
         if FindItem='quot' then Lexem:='"' else
         if CodeToChar(FindItem, Lexem) then else Lexem:='&'+FindItem+';';
         Result:=Result+BeforeItem+Lexem;
       end
       else Result:=Result+BeforeItem+'&'+Step;
    end;
    if Result='' then Result:=S else Result:=Result+Item;
  end;
  UniParser.Free;
end;

procedure TXMLParser.SetDocument(const S: string);
begin
  if S='' then FDocument:=' ' else FDocument:=S;
  PInitial:=Pointer(FDocument);
  PFinal:=@PInitial[Length(FDocument)];
  POpen:=PInitial;
  PClose:=PInitial;
  PNextOpen:=PInitial;
  PBeginSelected:=PInitial;
  PEndSelected:=PInitial;
  FTagType:=ttUnknownTag;
end;

constructor TXMLParser.Create(const S: string = '');
begin
  SetDocument(S);
  Compare:=CompareText;
end;

function TXMLParser.Next: Boolean;
begin

  Result:=false;
  if (PNextOpen=PFinal) then Exit;
  PAttribute:=nil;
  FLastToken:=-1;
  FLastAttribute:=-1;
  POpen:=PNextOpen;
  PClose:=POpen;
  Inc(PClose);

  while (PClose^<>'>') and (PClose<PFinal) do Inc(PClose);

  if PClose[-1]='/' then
  begin
    FTagType:=ttTermTag;
    PName:=POpen+1;
    PBefore:=PClose-1;
  end else
  begin
    case POpen[1] of
      '/':
      begin
        FTagType:=ttEndTag;
        PName:=POpen+2;
      end;
      '!':
      begin
        FTagType:=ttCommentTag;
        PName:=POpen+2;
      end;
      '?':
      begin
        FTagType:=ttDefineTag;
        PName:=POpen+2;
      end;
      else
      begin
        FTagType:=ttBeginTag;
        PName:=POpen+1;
      end;
    end;
    PBefore:=PClose;
  end;

  PNextOpen:=PClose;
  while (PNextOpen^<>'<') and (PNextOpen<PFinal) do Inc(PNextOpen);
  Result:=True;

end;

function TXMLParser.GetName: string;
begin
  if PAttribute=nil then PreAttribute;
  Result:=Get(PName,PSpace);
end;

function TXMLParser.GetSpace: string;
begin
  if PAttribute=nil then PreAttribute;
  Result:=Sub(PSpace, PAttribute);
end;

function TXMLParser.GetNameCode;
begin
  if PAttribute=nil then PreAttribute;
  Result:=Get(PName,PAttribute);
end;

function TXMLParser.GetTagCode: string;
begin
  Result:=Get(POpen, PClose+1);
end;

function TXMLParser.GetTagText;
begin
  Result:=Sub(POpen, PClose);
end;

function TXMLParser.GetContentCode: string;
begin
  Result:=Sub(PClose, PNextOpen);
end;

function TXMLParser.GetStepCode: string;
begin
  Result:=Get(POpen, PNextOpen);
end;

function TXMLParser.GetContentText;
begin
  Result:=HTMLDecode(Sub(PClose, PNextOpen));
end;

function TXMLParser.GetHead;
begin
  Result:=Get(PInitial, PClose+1);
end;

function TXMLParser.GetTail;
begin
  Result:=Sub(PClose, PFinal+1);
end;

procedure TXMLParser.BeginSelect;
begin
  PBeginSelected:=PClose;
end;

procedure TXMLParser.EndSelect;
begin
  PEndSelected:=POpen;
end;

function TXMLParser.GetSelected: string;
begin
  Result:=Sub(PBeginSelected, PEndSelected);
end;

function TXMLParser.GetLastAttribute: Integer;
begin
  if FLastAttribute<0 then InitAttributes;;
  Result:=FLastAttribute;
end;

function TXMLParser.GetAttribute(Index: Integer): string;
begin
  Result:=Get(FTokens[FAttributes[Index]]);
end;

function TXMLParser.GetContentTrimText;
var
  From, Before: PChar;
begin
  From:=PClose+1;
  Before:=PNextOpen-1;
  while (From<Before) and (From^<' ') do Inc(From);
  while (From<Before) and (Before^<' ') do Dec(Before);
  Result:=HTMLDecode(Get(From, Before+1));
end;

function TXMLParser.GetContentSpaceTrimText;
var
  From, Before: PChar;
begin
  From:=PClose+1;
  Before:=PNextOpen-1;
  while (From<Before) and (From^<'!') do Inc(From);
  while (From<Before) and (Before^<'!') do Dec(Before);
  Result:=HTMLDecode(Get(From,Before+1));
end;

procedure TXMLParser.PreAttribute;
begin
  PSpace:=nil;
  PAttribute:=PName;
  while (PAttribute^>'!') and (PAttribute<PBefore) do
  begin
    if PAttribute^=':' then PSpace:=PAttribute;
    Inc(PAttribute);
  end;
  if PSpace=nil then PSpace:=PAttribute;
end;

function TXMLParser.GetValue(Name: string): string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to GetLastAttribute do
  begin
    if Compare(Get(FTokens[FAttributes[i]]),Name)=0 then
    begin
      Result:=Get(FTokens[Succ(FAttributes[i])]);
      Exit;
    end;
  end;
end;

procedure TXMLParser.InitAttributes;
var
  P1: PChar;
  P2: PChar;

procedure AddToken;
begin
  Inc(FLastToken);
  if Length(FTokens)<=FLastToken then SetLength(FTokens, Succ(FLastToken*2));
  with FTokens[FLastToken] do
  begin
    From:=P1;
    Before:=P2;
  end;
end;

procedure AddAttribute;
begin
  Inc(FLastAttribute);
  if Length(FAttributes)<Succ(FLastAttribute) then SetLength(FAttributes, Succ(FLastAttribute*2));
  FAttributes[FLastAttribute]:=FLastToken;
end;

label
  Iteration;

begin

  if PAttribute=nil then PreAttribute;

  P1:=PAttribute;

  while P1<PBefore do
  case P1^ of

    '"':
    begin
      P2:=P1+1;
      while (P2<PBefore) and (P2^<>'"') do Inc(P2);
      Inc(P1);
      AddToken;
      P1:=P2+1;
    end;

    '''':
    begin
      P2:=P1+1;
      while (P2<PBefore) and (P2^<>'''') do Inc(P2);
      Inc(P1);
      AddToken;
      P1:=P2+1;
    end;

    else
    begin
      if P1^>' ' then
      begin
        P2:=P1;
        while (P2<PBefore) do
        begin
          if P2^='=' then
          begin
            AddToken;
            AddAttribute;
            goto Iteration;
          end else
          if (P2^<'!') then
          begin
            AddToken;
            goto Iteration;
          end;
          Inc(P2);
        end;
        AddToken;
        Exit;
        Iteration: P1:=P2+1;
      end else Inc(P1);
    end;
  end;

  AddToken;

end;

end.

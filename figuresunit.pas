unit FiguresUnit;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ActnList, GraphMath, ScaleUnit, LCLType, LCLIntf, LCL, StdCtrls, Grids,
  Buttons, Math, Spin, FPCanvas, TypInfo, Windows, Laz2_DOM,
  laz2_XMLRead, laz2_XMLWrite;

type

  TFigureClass = class of TFigure;
  tempPointsArray = array[0..3] of TPoint;
  PolygonPointsArray = array of TPoint;
  StringArray = array of string;
  LoadFile = array of string;

  TFigure = class
    Selected: boolean;
    Index: integer;
    CL: TClass;
    Region: HRGN;
    Points: array of TFloatPoint;
    class procedure SaveFile(FileName: string);
    class function LoadFile(FileName: string): boolean;
    class function LoadFigure(ANode: TDOMNode): boolean; virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure SetRegion; virtual; abstract;
    procedure DrawSelection(AFigure: TFigure; Canvas: TCanvas; Width: integer); virtual;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; virtual; abstract;

  end;

  TLittleFigure = class(TFigure)
    PenColor: TColor;
    PenStyle: TPenStyle;
    Width: integer;
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
  end;

  TBigFigure = class(TLittleFigure)
    BrushColor: TColor;
    BrushStyle: TBrushStyle;
    RoundingRadiusX: integer;
    RoundingRadiusY: integer;
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
  end;

  TPolyLine = class(TLittleFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

  TLine = class(TLittleFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;overload;
  end;

  TEllipse = class(TBigFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

  TRectangle = class(TBigFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

  TRectangleMagnifier = class(TLittleFigure)
    BrushStyle: TBrushStyle;
    BrushColor: TColor;
    procedure Draw(ACanvas: TCanvas); override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
  end;

  TRoundedRectangle = class(TBigFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure SetRegion; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

procedure LineRegion(p1, p2: TPoint; var tempPoints: array of TPoint; Width: integer);
function FiguresToXML(): TXMLDocument;
function XMLToFigures(Doc: TXMLDocument): boolean;

var
  Figures: array of TFigure;
  layer: array of Tfigure;
  ClassesFigures: array of TFigure;
  CtrlButtonState: boolean = False;
  IsSaved: boolean = True;


implementation

{Save}
class procedure TFigure.SaveFile(FileName: string);
var
  Doc: TXMLDocument;
begin
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit;
  try
    Doc := FiguresToXML();
    WriteXML(Doc, FileName);
  finally
    Doc.Free;
  end;
end;

function FiguresToXML(): TXMLDocument;
var
  Doc: TXMLDocument;
  FiguresNode: TDOMNode;
  i: integer;
begin
  Doc := TXMLDocument.Create;
  FiguresNode := Doc.CreateElement('Figures');
  Doc.AppendChild(FiguresNode);
  FiguresNode := Doc.DocumentElement;
  for i := 0 to High(Figures) do
    if figures[i].CL <> TRectangleMagnifier then
      FiguresNode.AppendChild(Figures[i].SaveFigure(Doc));
  Result := Doc;
end;

function TPolyLine.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TPolyLine');
  TDOMElement(Result).SetAttribute('Width', IntToStr(Width));
  TDOMElement(Result).SetAttribute('PenStyle', IntToStr(Ord(PenStyle)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PenColor));
  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TLine.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TLine');
  TDOMElement(Result).SetAttribute('Width', IntToStr(Width));
  TDOMElement(Result).SetAttribute('PenStyle', IntToStr(Ord(PenStyle)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PenColor));
  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TRectangle.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TRectangle');
  TDOMElement(Result).SetAttribute('Width', IntToStr(Width));
  TDOMElement(Result).SetAttribute('PenStyle', IntToStr(Ord(PenStyle)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PenColor));
  TDOMElement(Result).SetAttribute('BrushStyle', IntToStr(Ord(BrushStyle)));
  TDOMElement(Result).SetAttribute('BrushColor', IntToStr(BrushColor));

  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TEllipse.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TEllipse');
  TDOMElement(Result).SetAttribute('Width', IntToStr(Width));
  TDOMElement(Result).SetAttribute('PenStyle', IntToStr(Ord(PenStyle)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PenColor));
  TDOMElement(Result).SetAttribute('BrushStyle', IntToStr(Ord(BrushStyle)));
  TDOMElement(Result).SetAttribute('BrushColor', IntToStr(BrushColor));

  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TRoundedRectangle.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
var
  i: int64;
begin
  Result := ADoc.CreateElement('TRoundedRectangle');
  TDOMElement(Result).SetAttribute('Width', IntToStr(Width));
  TDOMElement(Result).SetAttribute('PenStyle', IntToStr(Ord(PenStyle)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PenColor));
  TDOMElement(Result).SetAttribute('BrushStyle', IntToStr(Ord(BrushStyle)));
  TDOMElement(Result).SetAttribute('BrushColor', IntToStr(BrushColor));
  TDOMElement(Result).SetAttribute('RadiusX', IntToStr(RoundingRadiusX));
  TDOMElement(Result).SetAttribute('RadiusY', IntToStr(RoundingRadiusY));
  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TRectangleMagnifier.SaveFigure(ADoc: TXMLDocument): TDOMNode;
begin
  Result := ADoc.CreateElement('');
end;

procedure TLittleFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color := PenColor;
  ACanvas.Pen.Style := PenStyle;
  ACanvas.Pen.Width := Width;
end;

procedure TBigFigure.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Brush.Style := BrushStyle;
  ACanvas.Brush.Color := BrushColor;
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  i: integer;
begin
  inherited;
  for i := 1 to high(Points) - 1 do
    ACanvas.Line(WorldToScreen(Points[i]), WorldToScreen(Points[i + 1]));
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TRectangleMagnifier.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Frame(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y);
end;

procedure TRoundedRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(WorldToScreen(Points[0]).x, WorldToScreen(Points[0]).Y,
    WorldToScreen(Points[1]).x, WorldToScreen(Points[1]).Y,
    RoundingRadiusX, RoundingRadiusY);
end;

procedure SetOffset(APoint: TFloatPoint);
begin
  Offset := APoint;
end;

procedure TBigFigure.SetRegion;
begin
end;

procedure TLittleFigure.SetRegion;
begin
end;

procedure TRectangle.SetRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateRectRgn(RegionRect.Left, RegionRect.Top, RegionRect.Right,
    RegionRect.Bottom);
end;

procedure TEllipse.SetRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateEllipticRgn(RegionRect.Left, RegionRect.Top,
    RegionRect.Right, RegionRect.Bottom);
end;

procedure TRoundedRectangle.SetRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[1]);
  Region := CreateRoundRectRgn(RegionRect.Left, RegionRect.Top,
    RegionRect.Right, RegionRect.Bottom, RoundingRadiusX, RoundingRadiusY);
end;

procedure TLine.SetRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1, p2: TPoint;
begin
  p1 := WorldToScreen(Points[0]);
  p2 := WorldToScreen(Points[1]);
  LineRegion(p1, p2, RegionPoints, Width);
  Region := CreatePolygonRgn(RegionPoints, 3, 2);
end;

procedure TPolyLine.SetRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1, p2: TPoint;
  curRgn: HRGN;
  i: integer;
begin
  for i := 0 to high(Points) - 1 do
  begin
    p1 := WorldToScreen(Points[i]);
    p2 := WorldToScreen(Points[i + 1]);
    LineRegion(p1, p2, RegionPoints, Width);
    if (i = low(Points)) then
      Region := CreatePolygonRgn(RegionPoints, 3, 2);
    curRgn := CreatePolygonRgn(RegionPoints, 3, 2);
    CombineRgn(Region, Region, curRgn, RGN_OR);
    DeleteObject(curRgn);
  end;
end;

procedure TFigure.DrawSelection(AFigure: TFigure; Canvas: TCanvas; Width: integer);
var
  Point1, Point2, a: TFloatPoint;
  i: integer;
  max, min: TFloatPoint;
begin
  if length(AFigure.Points) = 2 then
  begin
    Point1.X := AFigure.Points[0].X;
    Point1.Y := AFigure.Points[0].Y;
    Point2.X := AFigure.Points[1].X;
    Point2.Y := AFigure.Points[1].Y;
    if (Point1.X > Point2.X) then
    begin
      a.X := Point1.X;
      Point1.X := Point2.X;
      Point2.X := a.X;
    end;
    if (Point1.Y > Point2.Y) then
    begin
      a.Y := Point1.Y;
      Point1.Y := Point2.Y;
      Point2.Y := a.Y;
    end;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psDash;
    Canvas.Frame(WorldToScreen(Point1).x - 5 - Width div 2,
      WorldToScreen(Point1).y - 5 - Width div 2,
      WorldToScreen(Point2).x + 5 + Width div 2, WorldToScreen(Point2).y +
      5 - Width div 2);

  end
  else
  begin
    max := AFigure.Points[0];
    min := AFigure.Points[0];
    for i := 0 to length(AFigure.Points) - 1 do
    begin
      if AFigure.Points[i].X > max.x then
        max.x := AFigure.Points[i].X;
      if AFigure.Points[i].Y > max.y then
        max.y := AFigure.Points[i].y;
      if AFigure.Points[i].X < min.x then
        min.x := AFigure.Points[i].X;
      if AFigure.Points[i].Y < min.y then
        min.y := AFigure.Points[i].Y;
    end;

    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psDash;
    Canvas.Frame(WorldToScreen(min).x - 5 - Width div 2, WorldToScreen(min).y -
      5 - Width div 2,
      WorldToScreen(max).x + 5 + Width div 2, WorldToScreen(max).y +
      5 + Width div 2);
  end;
end;

procedure LineRegion(p1, p2: TPoint; var tempPoints: array of TPoint; Width: integer);
begin
  if (abs(p2.x - p1.x) > 45) then
  begin
    tempPoints[0].x := p1.x - Width div 2;
    tempPoints[0].y := p1.y - 5 - Width;
    tempPoints[1].x := p2.x + Width div 2;
    tempPoints[1].y := p2.y - 5 - Width;
    tempPoints[2].x := p2.x + Width div 2;
    tempPoints[2].y := p2.y + 5 + Width;
    tempPoints[3].x := p1.x - Width div 2;
    tempPoints[3].y := p1.y + 5 + Width;
  end
  else
  begin
    tempPoints[0].x := p1.x - 5 - Width;
    tempPoints[0].y := p1.y - Width div 2;
    tempPoints[1].x := p2.x - 5 - Width;
    tempPoints[1].y := p2.y + Width div 2;
    tempPoints[2].x := p2.x + 5 + Width;
    tempPoints[2].y := p2.y + Width div 2;
    tempPoints[3].x := p1.x + 5 + Width;
    tempPoints[3].y := p1.y - Width div 2;
  end;
end;

procedure AddFigure(AFigure: TFigure);
begin
  SetLength(ClassesFigures, Length(ClassesFigures) + 1);
  ClassesFigures[High(ClassesFigures)] := AFigure;
end;

{ Load }

class function TFigure.LoadFile(FileName: string): boolean;
var
  Doc: TXMLDocument;
begin
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit(False);
  try
    ReadXMLFile(Doc, FileName);
    Result := XMLToFigures(Doc);
  finally
    Doc.Free;
  end;
end;

function XMLToFigures(Doc: TXMLDocument): boolean;
var
  FigNode: TDOMNode;
  i: integer;
  f: TFigure;
begin
  Result := True;
  if Doc.DocumentElement.NodeName <> 'Figures' then
    Exit(False);
  for f in Figures do
    f.Destroy;
  SetLength(Figures, 0);
  try
    FigNode := Doc.DocumentElement.FirstChild;
    while FigNode <> nil do
    begin
      for i := 0 to High(ClassesFigures) do
        if FigNode.NodeName = ClassesFigures[i].ClassName then
          if not ClassesFigures[i].LoadFigure(FigNode) then
          begin
            exit(False);
          end;
      FigNode := FigNode.GetNextNodeSkipChildren;
    end;

  except
    exit(False);
  end;
end;

class function TPolyLine.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TPolyLine;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TPolyLine.Create;
    f.Width:=strtoint(ANode.Attributes.Item[0].NodeValue);
    f.PenStyle:=CasePenStyle(strtoint(ANode.Attributes.Item[1].NodeValue));
    f.PenStyle:=psSolid;
    f.PenColor:=strtoint(ANode.Attributes.Item[2].NodeValue);
    PNode := ANode;
    for i := 3 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      SetLength(f.Points,Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;



class function TRectangle.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TRectangle;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TRectangle.Create;
    f.Width:=strtoint(ANode.Attributes.Item[0].NodeValue);
    f.PenStyle:=CasePenStyle(strtoint(ANode.Attributes.Item[1].NodeValue));
    f.PenStyle:=psSolid;
    f.BrushStyle:=bsClear;
    f.PenColor:=strtoint(ANode.Attributes.Item[2].NodeValue);
    f.BrushColor:=strtoint(ANode.Attributes.Item[3].NodeValue);
    f.BrushStyle:=CaseBrushStyle(strtoint(ANode.Attributes.Item[4].NodeValue));
    F := TRectangle.Create;
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      SetLength(f.Points,Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;

class function TEllipse.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TEllipse;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TEllipse.Create;
    f.Width:=strtoint(ANode.Attributes.Item[0].NodeValue);
    f.PenStyle:=CasePenStyle(strtoint(ANode.Attributes.Item[1].NodeValue));
    f.PenStyle:=psSolid;
    f.BrushStyle:=bsClear;
    f.PenColor:=strtoint(ANode.Attributes.Item[2].NodeValue);
    f.BrushColor:=strtoint(ANode.Attributes.Item[3].NodeValue);
    f.BrushStyle:=CaseBrushStyle(strtoint(ANode.Attributes.Item[4].NodeValue));
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      SetLength(f.Points,Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;



class function TLine.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TLine;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TLine.Create;
    f.Width:=strtoint(ANode.Attributes.Item[0].NodeValue);
    f.PenStyle:=CasePenStyle(strtoint(ANode.Attributes.Item[1].NodeValue));
   // f.BrushStyle:=bsClear;
    f.PenColor:=strtoint(ANode.Attributes.Item[2].NodeValue);
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      setlength(f.points,length(f.points)+1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
  end;

  class function TRoundedRectangle.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TRoundedRectangle;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TRoundedRectangle.Create;
    f.Width:=strtoint(ANode.Attributes.Item[0].NodeValue);
   // f.PenStyle:=psSolid;
  //  f.BrushStyle:=bsClear;
    f.PenStyle:=CasePenStyle(strtoint(ANode.Attributes.Item[1].NodeValue));
    f.PenColor:=strtoint(ANode.Attributes.Item[2].NodeValue);
    f.BrushStyle:=CaseBrushStyle(strtoint(ANode.Attributes.Item[3].NodeValue));
    f.BrushColor:=strtoint(ANode.Attributes.Item[4].NodeValue);
    f.RoundingRadiusY:=strtoint(ANode.Attributes.Item[5].NodeValue);
    f.RoundingRadiusX:=strtoint(ANode.Attributes.Item[6].NodeValue);
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      SetLength(f.Points,Length(f.Points)+1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;




initialization
  AddFigure(TPolyLine.Create);
  AddFigure(TLine.Create);
  AddFigure(TRectangle.Create);
  AddFigure(TEllipse.Create);
  AddFigure(TRoundedRectangle.Create);
end.

unit ToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FiguresUnit, Graphics, GraphMath, ScaleUnit, ExtCtrls, StdCtrls, Spin, ColorBox,
  LCLType, LCLIntf, LCL;

type

  TParam = class
    procedure CreateObjects(Panel: TPanel; pos: integer); virtual; abstract;
  end;

  TPenColorParam = class(Tparam)
    procedure ChangePenColor(Sender: TObject);
    procedure CreateObjects(Panel: TPanel; pos: integer); override;
  end;

  TBrushColorParam = class(TParam)
    procedure ChangeBrushColor(Sender: TObject);
    procedure CreateObjects(Panel: TPanel; pos: integer); override;
  end;

  TWidthParam = class(TParam)
    procedure ChangeWidth(Sender: TObject);
    procedure CreateObjects(Panel: TPanel; pos: integer); override;
  end;

  TRoundingRadiusParamX = class(TParam)
    procedure ChangeRoundX(Sender: TObject);
    procedure CreateObjects(Panel: TPanel; pos: integer); override;
  end;

  TRoundingRadiusParamY = class(TParam)
    procedure ChangeRoundY(Sender: TObject);
    procedure CreateObjects(Panel: TPanel; pos: integer); override;
  end;

  TBrushStyleParam = class(TParam)
  const
    BStyles: array [0..7] of TBrushStyle = (bsSolid, bsClear,
      bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

    procedure ChangeBrushStyle(Sender: TObject);
    procedure CreateObjects(Panel: TPanel; pos: integer); override;
  end;

  TPenStyleParam = class(TParam)
    procedure ChangePenStyle(Sender: TObject);
    procedure CreateObjects(Panel: TPanel; pos: integer); override;
  const
    PStyles: array[0..5] of TPenStyle = (psSolid, psClear, psDot,
      psDash, psDashDot, psDashDotDot);
  end;

  TFigureTool = class
    Points: array of TFloatPoint;
    Icons: string;
    Param: array of TParam;
    procedure MouseDown(AX: integer; AY: integer); virtual; abstract;
    procedure MouseMove(X: integer; Y: integer); virtual; abstract;
    procedure Mouseup(X: integer; Y: integer; ACanvas: TCanvas); virtual; abstract;
    procedure ParamListCreate(); virtual; abstract;
    procedure ParamsCreate(Panel: TPanel);
  end;

  TLittleFigureTool = class(TFigureTool)
    procedure ParamListCreate(); override;
    procedure Mouseup(X: integer;Y: integer; ACanvas: TCanvas); override;
  end;

  TBigFigureTool = class(TLittleFigureTool)
    procedure ParamListCreate(); override;
    procedure Mouseup(X: integer;Y: integer; ACanvas: TCanvas); override;
  end;

  TPolyLineTool = class(TLittleFigureTool)
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
  end;

  TLineTool = class(TLittleFigureTool)
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
  end;

  TEllipceTool = class(TBigFigureTool)
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
  end;

  TRectangleTool = class(TBigFigureTool)
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
  end;

  TPaw = class(TFigureTool)
    FirstPoint: TPoint;
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure ParamListCreate(); override;
  end;

  Tmagnifier = class(TFigureTool)
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
  end;

  TRoundedRectangleTool = class(TBigFigureTool)
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure ParamListCreate(); override;
  end;

  TSelectTool = class(TFigureTool)
    procedure MouseDown(X: integer; Y: integer); override;
    procedure MouseMove(X: integer; Y: integer); override;
    procedure MouseUp(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
    procedure SelectParamListCreate();
  end;

  TMoverTool = class(TFigureTool)
    APoint: TPoint;
    procedure MouseDown(X: integer;Y: integer); override;
    procedure MouseMove(X: integer;Y: integer); override;
    procedure Mouseup(X: integer; Y: integer; ACanvas: TCanvas); override;
    procedure ParamListCreate(); override;
  end;


var
  Tool: array of TFigureTool;
  APenColor, ABrushColor: TColor;
  AWidth, ARadiusX, ARadiusY: integer;
  APenStyle: TPenStyle;
  ABrushStyle: TBrushStyle;
  SelectedBStyleIndex, SelectedPStyleIndex: integer;
  SelectedCreateParamFlag: Boolean;
  SelectedFigure: TFigureTool;
  Invalidate_: procedure of Object;

implementation

procedure TPenColorParam.CreateObjects(Panel: TPanel; pos: integer);
var
  ColorLabel: TLabel;
  PenColor: TColorBox;
begin
  ColorLabel := TLabel.Create(Panel);
  ColorLabel.Caption := 'Цвет карандаша';
  ColorLabel.Top := pos;
  ColorLabel.Left := pos;
  ColorLabel.Parent := Panel;

  PenColor := TColorBox.Create(panel);
  PenColor.Top := pos + 20;
  PenColor.Left := pos;
  PenColor.Parent := Panel;
  PenColor.Selected := APenColor;
  PenColor.OnChange := @ChangePenColor;
end;

procedure TPenColorParam.ChangePenColor(Sender: TObject);
var
  i: Integer;
begin
  APenColor := (Sender as TColorBox).Selected;
  for I:=0 to High(Figures) do
    if Figures[i].Selected then (Figures[i] as TLittleFigure).PenColor := APenColor;
    Invalidate_;  {chek this}
end;

procedure TPenStyleParam.CreateObjects(Panel: TPanel; pos: integer);
var
  StyleLabel: TLabel;
  PenStyle: TComboBox;
  i: integer;
  s: string;
begin
  StyleLabel := TLabel.Create(Panel);
  StyleLabel.Caption := 'Стиль линии';
  StyleLabel.Top := pos;
  StyleLabel.Parent := Panel;

  PenStyle := TComboBox.Create(panel);
  for i := 0 to 5 do
  begin
    WriteStr(s, PStyles[i]);
    PenStyle.Items.Add(s);
  end;
  PenStyle.Top := pos + 20;
  PenStyle.Parent := Panel;
  PenStyle.ReadOnly := True;
  PenStyle.ItemIndex := SelectedPStyleIndex;
  PenStyle.OnChange := @ChangePenStyle;
end;

procedure TPenStyleParam.ChangePenStyle(Sender: TObject);
var
  i: integer;
begin
  APenStyle := PStyles[(Sender as TComboBox).ItemIndex];
  SelectedPStyleIndex := (Sender as TComboBox).ItemIndex;
  for I:=0 to High(Figures) do
  if Figures[i].Selected then (Figures[i] as TLittleFigure).PenStyle := APenStyle;
   Invalidate_; {too}
end;

procedure TWidthParam.CreateObjects(Panel: TPanel; pos: integer);
var
  WidthLabel: TLabel;
  WidthParam: TSpinEdit;
begin
  WidthLabel := TLabel.Create(Panel);
  WidthLabel.Caption := 'Ширина карандаша';
  WidthLabel.Top := pos - 120;
  WidthLabel.Left := pos;
  WidthLabel.Parent := Panel;

  WidthParam := TSpinEdit.Create(Panel);
  WidthParam.Top := pos - 100;
  WidthParam.Left := pos;
  WidthParam.MinValue := 1;
  WidthParam.Parent := Panel;
  WidthParam.Value := AWidth;
  WidthParam.OnChange := @ChangeWidth;
end;

procedure TWidthParam.ChangeWidth(Sender: TObject);
var
  i: integer;
begin
  AWidth := (Sender as TSpinEdit).Value;
  for I:=0 to High(Figures) do
  if Figures[i].Selected then (Figures[i] as TLittleFigure).Width := AWidth;
  Invalidate_; {too}
end;

procedure TBrushColorParam.CreateObjects(Panel: TPanel; pos: integer);
var
  ColorLabel: TLabel;
  BrushColor: TColorBox;
begin
  ColorLabel := TLabel.Create(Panel);
  ColorLabel.Caption := 'Цвет заливки';
  ColorLabel.Top := pos - 120;
  ColorLabel.Left := pos + 60;
  ColorLabel.Parent := Panel;

  BrushColor := TColorBox.Create(Panel);
  BrushColor.Left := pos + 60;
  BrushColor.Top := pos - 100;
  BrushColor.Parent := Panel;
  BrushColor.Selected := ABrushColor;
  BrushColor.OnChange := @ChangeBrushColor;
end;

procedure TBrushColorParam.ChangeBrushColor(Sender: TObject);
var
  i: Integer;
begin
  ABrushColor := (Sender as TColorBox).Selected;
  for I:=0 to High(Figures) do
    if (Figures[i].Selected) and not (Figures[i].Index = 1)
      then (Figures[i] as TBigFigure).BrushColor := ABrushColor;
   Invalidate_; {t00}
end;

procedure TBrushStyleParam.CreateObjects(Panel: TPanel; pos: integer);
var
  StyleLabel: TLabel;
  BrushStyle: TComboBox;
  i: integer;
  s: string;
begin
  StyleLabel := TLabel.Create(Panel);
  StyleLabel.Caption := 'Стиль заливки ';
  StyleLabel.Top := pos - 240;
  StyleLabel.Left := pos;
  StyleLabel.Parent := Panel;

  BrushStyle := TComboBox.Create(panel);
  for i := 0 to 5 do
  begin
    WriteStr(s, BStyles[i]);
    BrushStyle.Items.Add(s);
  end;
  BrushStyle.Top := pos - 220;
  BrushStyle.Left := pos;
  BrushStyle.Parent := Panel;
  BrushStyle.ItemIndex := SelectedBStyleIndex;
  BrushStyle.ReadOnly := True;
  BrushStyle.OnChange := @ChangeBrushStyle;
end;

procedure TBrushStyleParam.ChangeBrushStyle(Sender: TObject);
var
  i: integer;
begin
  ABrushStyle := BStyles[(Sender as TComboBox).ItemIndex];
  SelectedBStyleIndex := (Sender as TComboBox).ItemIndex;
  for I:=0 to High(Figures) do
    if (Figures[i].Selected) and not (Figures[i].Index = 1)
      then (Figures[i] as TBigFigure).BrushStyle := ABrushStyle;
   Invalidate_; {t00}
end;

procedure TRoundingRadiusParamX.CreateObjects(Panel: TPanel; pos: integer);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusX: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(Panel);
  RoundingRadiusLabel.Caption := 'Радиус округления X';
  RoundingRadiusLabel.Top := pos - 300;
  RoundingRadiusLabel.Left := pos + 60;
  RoundingRadiusLabel.Parent := Panel;

  RoundingRadiusX := TSpinEdit.Create(Panel);
  RoundingRadiusX.Top := pos - 280;
  RoundingRadiusX.Left := pos + 60;
  RoundingRadiusX.MinValue := 0;
  RoundingRadiusX.OnChange := @ChangeRoundX;
  RoundingRadiusX.Parent := Panel;
  RoundingRadiusX.Value := ARadiusX;
end;

procedure TRoundingRadiusParamX.ChangeRoundX(Sender: TObject);
var
  i: Integer;
begin
  ARadiusX := (Sender as TSpinEdit).Value;
  for I:=0 to High(Figures) do
    if (Figures[i].Selected) and (Figures[i].Index = 3)
      then (Figures[i] as TRoundedRectangle).RoundingRadiusX := ARadiusX;
   Invalidate_; {t00}
end;

procedure TRoundingRadiusParamY.CreateObjects(Panel: TPanel; pos: integer);
var
  RoundingRadiusLabel: TLabel;
  RoundingRadiusY: TSpinEdit;
begin
  RoundingRadiusLabel := TLabel.Create(Panel);
  RoundingRadiusLabel.Caption := 'Радиус округления Y';
  RoundingRadiusLabel.Top := pos - 300;
  RoundingRadiusLabel.Left := pos;
  RoundingRadiusLabel.Parent := Panel;

  RoundingRadiusY := TSpinEdit.Create(Panel);
  RoundingRadiusY.Top := pos - 280;
  RoundingRadiusY.Left := pos;
  RoundingRadiusY.MinValue := 0;
  RoundingRadiusY.Parent := Panel;
  RoundingRadiusY.Value := ARadiusY;
  RoundingRadiusY.OnChange := @ChangeRoundY;
end;

procedure TRoundingRadiusParamY.ChangeRoundY(Sender: TObject);
var
  i: Integer;
begin
  ARadiusY := (Sender as TSpinEdit).Value;
  for I:=0 to High(Figures) do
    if (Figures[i].Selected) and (Figures[i].Index = 3)
      then (Figures[i] as TRoundedRectangle).RoundingRadiusY := ARadiusY;
   Invalidate_; {t00}
end;

procedure TLittleFigureTool.ParamListCreate();
begin
  SetLength(Param, Length(Param) + 3);
  Param[High(Param) - 2] := TPenColorParam.Create();
  Param[High(Param) - 1] := TPenStyleParam.Create();
  Param[High(Param)] := TWidthParam.Create();
end;

procedure TBigFigureTool.ParamListCreate();
begin
  inherited;
  SetLength(Param, Length(Param) + 2);
  Param[High(Param) - 1] := TBrushColorParam.Create();
  Param[High(Param)] := TBrushStyleParam.Create();
end;

procedure TRoundedRectangleTool.ParamListCreate();
begin
  inherited;
  SetLength(Param, Length(Param) + 2);
  Param[High(Param) - 1] := TRoundingRadiusParamX.Create();
  Param[High(Param)] := TRoundingRadiusParamY.Create();
end;

procedure TPaw.ParamListCreate();
begin
end;

procedure Tmagnifier.ParamListCreate();
begin
end;

procedure TSelectTool.ParamListCreate();
begin
end;

procedure TFigureTool.ParamsCreate(Panel: TPanel);
var
  i, pos: integer;
begin
  for i := 0 to high(Param) do
  begin
    Param[i].CreateObjects(Panel, i * 60);
  end;
end;

procedure RegisterTool(ATool: TFigureTool; S: string);
begin
  Setlength(Tool, Length(Tool) + 1);
  Tool[high(Tool)] := ATool;
  Tool[high(Tool)].Icons := s;
  Atool.ParamListCreate();
end;

procedure TBigFigureTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

procedure TLittleFigureTool.MouseUp(X: Integer;Y: Integer; ACanvas: TCanvas);
begin
end;

procedure TMagnifier.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
  RectZoom(AHeightPB, AWidthPB, (Figures[high(Figures)] as TLittleFigure).Points[0],
    (Figures[high(Figures)] as TLittleFigure).Points[1]);
  SetLength(Figures, Length(Figures) - 1);
end;

procedure TMagnifier.MouseDown(X: integer; Y: integer);
var
  AFigure: TRectangleMagnifier;
begin
  SetLength(Figures, Length(figures) + 1);
  Figures[high(Figures)] := TRectangleMagnifier.Create();
  AFigure := (Figures[high(Figures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
end;

procedure TMagnifier.MouseMove(X: integer; Y: integer);
begin
  (Figures[high(Figures)] as TLittleFigure).Points[1] := ScreenToWorld(Point(X, Y));
end;

procedure TPaw.MouseDown(X: integer; Y: integer);
begin
  FirstPoint := Point(X, Y);
end;

procedure TPaw.MouseMove(X: integer; Y: integer);
begin
  offset.x += FirstPoint.X - X;
  offset.y += FirstPoint.Y - Y;
  FirstPoint := Point(X, Y);
end;

procedure TPolyLineTool.MouseDown(X: integer; Y: integer);
var
  AFigure: TLittleFigure;

begin
  Setlength(Figures, Length(Figures) + 1);
  Figures[high(Figures)] := TPolyLine.Create();
  AFigure := (Figures[high(Figures)] as TLittleFigure);
  SetLength((Figures[high(Figures)] as TLittleFigure).Points, Length(
    (Figures[high(Figures)] as TLittleFigure).points) + 1);
  (Figures[high(Figures)] as TLittleFigure).Points[high(
    (Figures[high(Figures)] as TLittleFigure).Points)] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  AFigure.Index := 1;
  MaxMin(Point(X, Y));
end;

procedure TLineTool.MouseDown(X: integer; Y: integer);
var
  AFigure: TLittleFigure;
begin
  Setlength(Figures, length(Figures) + 1);
  Figures[high(Figures)] := TLine.Create();
  AFigure := (Figures[high(Figures)] as TLittleFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.PenStyle := APenStyle;
  AFigure.Index := 1;
  MaxMin(Point(X, Y));
end;

procedure TRectangleTool.MouseDown(X: integer; Y: integer);
var
  AFigure: TBigFigure;
begin
  Setlength(Figures, Length(figures) + 1);
  Figures[high(Figures)] := TRectangle.Create();
  AFigure := (Figures[high(Figures)] as TBigFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  AFigure.Index := 2;
  MaxMin(Point(X, Y));
end;

procedure TRoundedRectangleTool.MouseDown(X: integer; Y: integer);
var
  AFigure: TBigFigure;
begin
  Setlength(Figures, Length(figures) + 1);
  Figures[high(Figures)] := TRoundedRectangle.Create();
  AFigure := (Figures[high(Figures)] as TBigFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.RoundingRadiusX := ARadiusX;
  AFigure.RoundingRadiusY := ARadiusY;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  AFigure.Index := 3;
  MaxMin(Point(X, Y));
end;

procedure TEllipceTool.MouseDown(X: integer; Y: integer);
var
  AFigure: TBigFigure;
begin
  SetLength(Figures, Length(figures) + 1);
  Figures[high(Figures)] := TEllipse.Create();
  AFigure := (Figures[high(Figures)] as TBigFigure);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X, Y));
  AFigure.Points[1] := ScreenToWorld(Point(X, Y));
  AFigure.PenColor := APenColor;
  AFigure.Width := AWidth;
  AFigure.BrushColor := ABrushColor;
  AFigure.PenStyle := APenStyle;
  AFigure.BrushStyle := ABrushStyle;
  AFigure.Index := 2;
  MaxMin(Point(X, Y));
end;

procedure TLineTool.MouseMove(X: integer; Y: integer);
begin
  (Figures[high(Figures)] as TLittleFigure).Points[1] := ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TEllipceTool.MouseMove(X: integer; Y: integer);
begin
  (Figures[high(Figures)] as TLittleFigure).Points[1] := ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TRectangleTool.MouseMove(X: integer; Y: integer);
begin
  (Figures[high(Figures)] as TLittleFigure).Points[1] := ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TRoundedRectangleTool.MouseMove(X: integer; Y: integer);
begin
  (Figures[high(Figures)] as TLittleFigure).Points[1] := ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TPolyLineTool.MouseMove(X: integer; Y: integer);
begin
  SetLength((Figures[high(Figures)] as TLittleFigure).points, length(
    (Figures[high(Figures)] as TLittleFigure).points) + 1);
  (Figures[high(Figures)] as TLittleFigure).Points[high(
    (Figures[high(Figures)] as TLittleFigure).Points)] := ScreenToWorld(Point(X, Y));
  MaxMin(Point(X, Y));
end;

procedure TSelectTool.MouseDown(X: integer; Y: integer);
var
  AFigure: TRectangleMagnifier;
begin
  SetLength(Figures, Length(figures) + 1);
  Figures[high(Figures)] := TRectangleMagnifier.Create();
  AFigure := (Figures[high(Figures)] as TRectangleMagnifier);
  SetLength(AFigure.Points, 2);
  AFigure.Points[0] := ScreenToWorld(Point(X,Y));
  AFigure.Points[1] := ScreenToWorld(Point(X,Y));
  SelectedCreateParamFlag := True;
end;

procedure TSelectTool.MouseMove(X: integer; Y: integer);
begin
  (Figures[high(Figures)] as TLittleFigure).Points[1] := ScreenToWorld(Point(X,Y));
end;

procedure TSelectTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
var
  i: integer;
  SelectRegion: HRGN;
  ToolRegio: HRGN;
begin
  SelectRegion := CreateRectRgn((WorldToScreen((Figures[high(Figures)] as TRectangleMagnifier).Points[0]).x),
                               (WorldToScreen((Figures[high(Figures)] as TRectangleMagnifier).Points[0]).y),
                               (WorldToScreen((Figures[high(Figures)] as TRectangleMagnifier).Points[1]).x),
                               (WorldToScreen((Figures[high(Figures)] as TRectangleMagnifier).Points[1]).y));
  with Figures[high(Figures)] do begin
    for i := 0 to high(Figures)-1 do
    begin
      DeleteObject(Figures[i].Region);
      Figures[i].SetRegion;
      ToolRegio := CreateRectRgn(1,1,2,2);
      if (CombineRgn(ToolRegio,Figures[i].Region,SelectRegion,RGN_AND) <> NULLREGION) then
      begin
        if Figures[i].Selected = false then
          Figures[i].Selected := true
        else
          Figures[i].Selected := false;
        end;
      DeleteObject(ToolRegio);
    end;
  end;
  SetLength(Figures, Length(figures) - 1);
  SelectParamListCreate();
end;

procedure TSelectTool.SelectParamListCreate();
var
  i: Integer;
  highIndex: Integer;
  f1: TLittleFigureTool;
  f2: TBigFigureTool;
  f3: TRoundedRectangleTool;
begin
  highIndex := 0;
  for i := 0 to high(Figures) do
  if Figures[i].Selected then
  if (Figures[i].Index > highIndex) then highIndex := Figures[i].Index;

  f1 := TLittleFigureTool.Create();
  f2 := TBigFigureTool.Create();
  f3 := TRoundedRectangleTool.Create();

  case highIndex of
    1:begin
        f1.ParamListCreate();
        SelectedFigure := f1;
      end;
    2:begin
        f2.ParamListCreate();
        SelectedFigure := f2;
      end;
    3:begin
        f3.ParamListCreate();
        SelectedFigure := f3;
      end;
   end;
end;

procedure TMoverTool.MouseDown(X: Integer; Y: Integer);
begin
  APoint := Point(X,Y);
End;

procedure TMoverTool.MouseMove(X: Integer; Y: Integer);
var
  i, j: integer;
  P: TPoint;
begin
  P.x := x - APoint.x;
  P.y := y - APoint.y;
  for i:=0 to High(Figures) do
  if Figures[i].Selected then
  for j:=0 to High(Figures[i].Points) do
  begin
    Figures[i].Points[j].X := Figures[i].Points[j].X + MoveTo(P).X;
    Figures[i].Points[j].Y := Figures[i].Points[j].Y + MoveTo(P).Y;
  end;
  APoint := Point(X,Y);
end;

procedure TMoverTool.ParamListCreate();
begin
end;

procedure TMoverTool.MouseUp(X: integer; Y: integer; ACanvas: TCanvas);
begin
end;

begin
  RegisterTool(TPolyLineTool.Create(), '0.png');
  RegisterTool(TLineTool.Create(), '1.png');
  RegisterTool(TRectangleTool.Create(), '2.png');
  RegisterTool(TEllipceTool.Create(), '3.png');
  RegisterTool(TPaw.Create(), '4.png');
  RegisterTool(Tmagnifier.Create(), '5.png');
  RegisterTool(TRoundedRectangleTool.Create(), '6.png');
  RegisterTool(TSelectTool.Create(), '7.png');
  RegisterTool(TMoverTool.Create(), '8.png');
  APenColor := clBlack;
  ABrushColor := clBlack;
  AWidth := 20;
  ARadiusX := 30;
  ARadiusY := 30;
  APenStyle := psSolid;
  SelectedPStyleIndex := 0;
  SelectedBStyleIndex := 0;
end.

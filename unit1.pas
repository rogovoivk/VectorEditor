unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ActnList, ColorBox, Spin, utools, figures_, Buttons;

type

  { TEditor }

  TEditor = class(TForm)
    Color_: TColorBox;
    ColorLabel: TLabel;
    BrushColor_: TColorBox;
    FuckingPanel: TPanel;
    ButtonPanel: TPanel;
    WidthLabel: TLabel;
    PB: TPaintBox;
    BrushLabel: TLabel;
    Width_: TSpinEdit;
    procedure ButtonsDown(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPaint(Sender: TObject);
    procedure Exit_Click(Sender: TObject);
    procedure AuthorClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);

  private
    { private declarations }
  public
    { public declarations }
  end;

type
  line = record
    color: TColor;
    Width: integer;
    points: array of TPoint;
  end;

var
  Editor: TEditor;
  Lines: array of line;
  isDrawing: boolean;
  CurrentTool: TFigureTool;

implementation

{$R *.lfm}

{ TEditor }

procedure TEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  IsDrawing := False;
  Invalidate;
end;

procedure TEditor.FormCreate(Sender: TObject);
var
  i: integer;
  ToolButton, Clear, Back_: TSpeedButton;
  ToolIcon: TBitMap;
begin
  Clear := TSpeedButton.Create(Editor);
  Clear.Width := 60;
  Clear.Height := 40;
  Clear.Top := 45;
  Clear.Left := 5;
  Clear.Parent := ButtonPanel;
  Clear.Tag := 4;
  Clear.OnClick := @ButtonsDown;
  Clear.Caption := 'Очистить';

  Back_ := TSpeedButton.Create(Editor);
  Back_.Width := 50;
  Back_.Height := 40;
  Back_.Top := 45;
  Back_.Left := 70;
  Back_.Parent := ButtonPanel;
  Back_.Tag := 5;
  Back_.OnClick := @ButtonsDown;
  Back_.Caption := 'Назад';

  for i := 0 to 3 do
  begin
    ToolButton := TSpeedButton.Create(Editor);
    ToolButton.Width := 40;
    ToolButton.Height := 40;
    ToolButton.Top := 0;
    ToolButton.Left := 5 + 5 * i + 40 * i;
    ToolButton.Parent := ButtonPanel;
    ToolButton.Tag := i;
    ToolButton.OnClick := @ButtonsDown;
    ToolIcon := TBitmap.Create;
    with TPicture.Create do
    begin
      LoadFromFile(Tool[i].Icons);
      ToolIcon.Assign(Graphic);
    end;
    ToolButton.Glyph := ToolIcon;
  end;
end;

procedure TEditor.ButtonsDown(Sender: TObject);
begin
  if (Sender as TSpeedbutton).tag < 4 then
    CurrentTool := Tool[(Sender as TSpeedbutton).tag]
  else
  if (Sender as TSpeedbutton).tag = 4 then
  begin
    setlength(Figures, 0);
    Invalidate;
  end
  else
  begin
    if length(figures) > 0 then
      setlength(Figures, length(figures) - 1);
    Invalidate;
  end;
end;

procedure TEditor.FormPaint(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(Figures) do
  begin
    Figures[i].Draw(PB.Canvas);
  end;
end;


procedure TEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  IsDrawing := True;
  CurrentTool.MouseDown(X, Y, Width_.Value, Color_.Selected, BrushColor_.Selected);
  Invalidate;
end;

procedure TEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if IsDrawing then
  begin
    CurrentTool.MouseMove(X, Y);
    Invalidate;
  end;
end;

begin
  CurrentTool := TPolyLineTool.Create();
end.

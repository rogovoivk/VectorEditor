unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ActnList, ColorBox, Spin, ToolsUnit, FiguresUnit, Buttons, ExtDlgs, ScaleUnit;

type

  { TEditor }

  TEditor = class(TForm)
    ShowAllButton: TButton;
    Clear: TButton;
    Back: TButton;
    ZoomLabel: TLabel;
    ScrollBarHorizontal: TScrollBar;
    ScrollBarVertical: TScrollBar;
    ToolPanel: TPanel;
    ButtonPanel: TPanel;
    MMenu: TMainMenu;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MExit: TMenuItem;
    SMAbout: TMenuItem;
    PB: TPaintBox;
    ZoomSpinEdit: TSpinEdit;
    procedure BackClick(Sender: TObject);
    procedure ButtonsDown(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure SMAboutClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure ToolPanelClick(Sender: TObject);
    procedure ZoomChange(Sender: TObject);


  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Editor: TEditor;
  isDrawing: boolean;
  CurrentTool: TFigureTool;

implementation

{$R *.lfm}

{ TEditor }


procedure TEditor.MExitClick(Sender: TObject);
begin
  Close();
end;

procedure TEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsDrawing := False;
  CurrentTool.MouseUp(X, Y);
  Invalidate;
end;

procedure TEditor.FormCreate(Sender: TObject);
var
  i: integer;
  ToolButton: TSpeedButton;
  ToolIcon: TBitMap;
begin
  CurrentTool := TPolyLineTool.Create();
  Zoom := 100;

  for i:=0 to high(Tool) do begin
  ToolButton := TSpeedButton.Create(Editor);
  ToolButton.Width := 40;
  ToolButton.Height := 40;
  ToolButton.Top := (i div 4) * 50;
  ToolButton.Left := (i mod 4) * 80 ;
  ToolButton.Parent := ButtonPanel;
  ToolButton.Tag := i;
  ToolButton.OnClick := @ButtonsDown;
  if i=0 then ToolButton.Click();
  ToolIcon := TBitmap.Create;
  with TPicture.create do
    begin
    LoadFromFile(Tool[i].Icons);
    ToolIcon.Assign(Graphic);
    end;
  ToolButton.Glyph := ToolIcon;
  end;
end;

procedure TEditor.ButtonsDown(Sender: TObject);
var
  Parampanel: TPanel;
begin
  CurrentTool := Tool[(Sender as TSpeedbutton).tag];
  ParamPanel := TPanel.Create(Editor);
  ParamPanel.Top := 1;
  Parampanel.LeFt := 350;
  ParamPanel.Width := 550;
  ParamPanel.Height := 120;
  ParamPanel.Parent := ToolPanel;
  CurrentTool.ParamsCreate(ParamPanel);
  Invalidate;
end;

procedure TEditor.BackClick(Sender: TObject);
begin
  if Length(Figures)<>0 then SetLength(Figures, Length(figures) - 1);
  Invalidate;
end;


procedure TEditor.ClearClick(Sender: TObject);
begin
  SetLength(Figures,0);
  Invalidate;
end;

procedure TEditor.FormPaint(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(Figures) do
  Figures[i].Draw(PB.Canvas);
  ScrollBarVertical.Max := trunc(MaxPoint.Y);
  ScrollBarVertical.Min := trunc(MinPoint.Y);
  ScrollBarHorizontal.Max := trunc(MaxPoint.X);
  ScrollBarHorizontal.Min := trunc(MinPoint.X);
  ZoomSpinEdit.Value := zoom;
  AHeightPB := PB.Height;
  AWidthPB := PB.Width;
end;

procedure TEditor.SMAboutClick(Sender: TObject);
begin
  ShowMessage('Роговой Владислав - Б8103б - векторный редактор - тест гита');
end;

procedure TEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsDrawing := true;
  CurrentTool.MouseDown(X, Y);
  MaxMin(ScreenToWorld(Point(X,Y)));
  Invalidate;
end;

procedure TEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
  );
begin
  if IsDrawing then  begin
    CurrentTool.MouseMove(X, Y);
    MaxMin(ScreenToWorld(Point(X,Y)));
    Invalidate;
  end;
end;

procedure TEditor.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Offset := Point(ScrollBarHorizontal.Position, ScrollBarVertical.Position);
  Invalidate;
end;

procedure TEditor.ShowAllButtonClick(Sender: TObject);
begin
  RectZoom(pB.Height,PB.Width,MinPoint,MaxPoint);
  Invalidate;
  ScrollBarVertical.Max:=trunc(MaxPoint.Y);
  ScrollBarVertical.Min:=trunc(MinPoint.Y);
  ScrollBarHorizontal.Max:=trunc(MaxPoint.X);
  ScrollBarHorizontal.Min:=trunc(MinPoint.X);
  Offset.X:=0;
  Offset.Y:=0;
end;

procedure TEditor.ToolPanelClick(Sender: TObject);
begin

end;

procedure TEditor.ZoomChange(Sender: TObject);
begin
  Zoom := ZoomSpinEdit.Value;
  Invalidate;
end;

begin
end.

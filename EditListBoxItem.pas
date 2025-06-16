unit EditListBoxItem;

interface

uses
  System.Classes,System.StrUtils, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
   Vcl.Forms, Vcl.Dialogs, Vcl.Graphics,  System.Generics.Collections, Vcl.WinXPickers, System.JSON, Windows;

type
  TVariablePair = class
    NameEdit: TEdit;
    ValueEdit: TEdit;
    DeleteButton: TButton;
  end;
  TRangePair = class
  public
    NameEdit: TEdit;
    StartEdit: TEdit;
    EndEdit: Tedit;
    DeleteButton: TButton;
  end;


  TEditListBoxItem = class(TPanel)
  private
    FRanges: TList<TRangePair>;
    FMaxRanges: Integer;
    FColorButton: TButton;
    FSelectedColor: TColor;
    FColorDisplay: TPanel;
    FColorDialog: TColorDialog;
    FDependentVarEdit: TEdit;
    FMainEdit: TEdit;
    FDependentLabel : TLabel;
    FDeleteButton: TButton;
    FAddVarButton: TButton;
    FAddRangeButton : TButton;
    FEvaluateButton: TButton;
    FHideButton: TButton;
    FVariables: TObjectList<TVariablePair>;

    procedure OnColorClick(Sender: TObject);
    procedure OnAddVariableClick(Sender: TObject);
    procedure OnDeleteVariableClick(Sender : TObject);
    procedure OnDeleteRangeClick(Sender: TObject);
    procedure OnAddRangeClick(Sender: TObject);
    procedure OnDependedVarEdit(Sender : TObject);
    procedure OnVariableNameEdit(Sender : TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);


    procedure SetSelectedColor(const Value: TColor);

    procedure AddRange(out outRangePair : TRangePair);
    procedure AddVarible(out outVariablePair : TVariablePair);


    procedure Arrange;
  public

    IsGraphHidden : Boolean;

    procedure setMethods(onhidemethod : TNotifyEvent; oncalcmethod : TNotifyEvent);

    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;

    property DependentVarEdit : TEdit read FDependentVarEdit write FDependentVarEdit;

    property MaxRanges : Integer read FMaxRanges write FMaxRanges;


    procedure GetRanges(out result : TList<TRangePair>);

    function IfChangedGetMainEditData(out resultStr : string) : Boolean;

    procedure GetVariables(out result : TObjectList<TVariablePair>);

    procedure FromJson(json_data : TJSONObject);

    procedure ToJson(out json_data : TJSONObject);


  end;

procedure SetDeleteButtonFunction(EditListBoxItem : TEditListBoxItem; func : TNotifyEvent);

implementation

uses Unit1, System.SysUtils, MathUtils;



function ColorToHexStr(Color: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x', [
    GetRValue(Color),
    GetGValue(Color),
    GetBValue(Color)
  ]);
end;


function HexStrToColor(const Hex: string): TColor;
var
  R, G, B: Integer;
begin
  if (Length(Hex) = 7) and (Hex[1] = '#') then
  begin
    R := StrToInt('$' + Copy(Hex, 2, 2));
    G := StrToInt('$' + Copy(Hex, 4, 2));
    B := StrToInt('$' + Copy(Hex, 6, 2));
    Result := RGB(R, G, B);
  end
  else
    Result := clWhite;
end;



procedure TEditListBoxItem.AddRange(out outRangePair: TRangePair);
var
  RangePair: TRangePair;
begin
  if FRanges.Count >= FMaxRanges then
  begin
    ShowMessage('Достигнуто максимальное количество диапазонов.');
    Exit;
  end;

  RangePair := TRangePair.Create;


  RangePair.NameEdit := TEdit.Create(Self);
  RangePair.NameEdit.Parent := Self;
  RangePair.NameEdit.Text := 'x';


  RangePair.StartEdit := TEdit.Create(Self);
  RangePair.StartEdit.Parent := Self;
  RangePair.StartEdit.Text := '0';
  RangePair.StartEdit.OnKeyPress := EditKeyPress;


  RangePair.EndEdit := TEdit.Create(Self);
  RangePair.EndEdit.Parent := Self;
  RangePair.EndEdit.Text := '10';
  RangePair.EndEdit.OnKeyPress := EditKeyPress;

  RangePair.DeleteButton := TButton.Create(Self);
  RangePair.DeleteButton.Parent := Self;
  RangePair.DeleteButton.Caption := '✖';
  RangePair.DeleteButton.OnClick := OnDeleteRangeClick;
  RangePair.DeleteButton.Tag := NativeInt(RangePair.NameEdit);

  FRanges.Add(RangePair);
  self.Height := Self.Height + RangePair.NameEdit.Height;
  Arrange;
  CallListBoxArrange;
  outRangePair := RangePair;
end;

procedure TEditListBoxItem.AddVarible(out outVariablePair: TVariablePair);
var
  VarPair: TVariablePair;
begin
  VarPair := TVariablePair.Create;
  VarPair.NameEdit := TEdit.Create(Self);
  VarPair.NameEdit.Parent := Self;
  VarPair.NameEdit.OnChange := OnVariableNameEdit;

  VarPair.ValueEdit := TEdit.Create(Self);
  VarPair.ValueEdit.Parent := Self;
  VarPair.ValueEdit.OnKeyPress := EditKeyPress;

  VarPair.DeleteButton := TButton.Create(Self);
  VarPair.DeleteButton.Parent := Self;
  VarPair.DeleteButton.Caption := '✖';
  VarPair.DeleteButton.OnClick := OnDeleteVariableClick;
  VarPair.DeleteButton.Tag := NativeInt(VarPair.NameEdit);

  self.Height := Self.Height + VarPair.NameEdit.Height;
  FVariables.Add(VarPair);
  Arrange;
  CallListBoxArrange;
  outVariablePair := VarPair;
end;


procedure TEditListBoxItem.FromJson(json_data: TJSONObject);
var
  DependentVarStr, MathExprStr: string;
  RangesObj, VariablesObj: TJSONObject;
  RangeNames: TJSONArray;
  i: Integer;
  RangeName: string;
  RangeValue, RangeData: TJSONValue;
  VariableValue: TJSONValue;
  RangePair: TRangePair;
  VariablePair: TVariablePair;
  ColorStr: string;
begin

  if json_data.TryGetValue<string>('color', ColorStr) then
    FSelectedColor := HexStrToColor(ColorStr);
    FColorDisplay.Color := FSelectedColor;

  if json_data.TryGetValue<string>('DependentVar', DependentVarStr) then
    FDependentVarEdit.Text := DependentVarStr;
    FDependentVarEdit.Modified := True;

  if json_data.TryGetValue<string>('math_expression', MathExprStr) then
    FMainEdit.Text := MathExprStr;
    FMainEdit.Modified := True;

  RangesObj := json_data.Values['ranges'] as TJSONObject;
  if Assigned(RangesObj) then
  begin
    for i := 0 to RangesObj.Count - 1 do
    begin
      RangeName := RangesObj.Pairs[i].JsonString.Value;
      RangeData := RangesObj.Pairs[i].JsonValue;
      if not (RangeData is TJSONObject) then Continue;

      AddRange(RangePair);
      RangePair.NameEdit.Text := RangeName;
      RangePair.NameEdit.Modified := True;
      RangePair.StartEdit.Text := TJSONObject(RangeData).Values['start'].Value;
      RangePair.StartEdit.Modified := True;
      RangePair.EndEdit.Text := TJSONObject(RangeData).Values['end'].Value;
      RangePair.EndEdit.Modified := True;
    end;
  end;

  VariablesObj := json_data.Values['variables'] as TJSONObject;
  if Assigned(VariablesObj) then
  begin
    for i := 0 to VariablesObj.Count - 1 do
    begin
      AddVarible(VariablePair);
      VariablePair.NameEdit.Text := VariablesObj.Pairs[i].JsonString.Value;
      VariablePair.NameEdit.Modified := True;
      VariableValue := VariablesObj.Pairs[i].JsonValue;
      VariablePair.ValueEdit.Text := VariableValue.Value;
      VariablePair.ValueEdit.Modified := True;
    end;
  end;
end;

procedure TEditListBoxItem.ToJson(out json_data : TJSONObject);
var
  I : Integer;
  ranges, variables, range : TJSONObject;
  item : TRangePair;
  variable : TVariablePair;
begin
  json_data := TJSONObject.Create;
  ranges := TJSONObject.Create;
  variables := TJSONObject.Create;
  json_data.AddPair('ranges', ranges);
  json_data.AddPair('variables', variables);
  json_data.AddPair('DependentVar', FDependentVarEdit.Text);
  json_data.AddPair('math_expression', FMainEdit.Text);
  json_data.AddPair('color', ColorToHexStr(FSelectedColor));


  for I := 0 to FRanges.Count-1 do
  begin
    item := FRanges.Items[I];
    range := TJSONObject.Create;
    range.AddPair('start' , item.StartEdit.Text);
    range.AddPair('end' , item.EndEdit.Text);
    ranges.AddPair(item.NameEdit.Text, range);
  end;

  for I := 0 to FVariables.Count-1 do
    begin
      variable := FVariables.Items[I];
      variables.AddPair(variable.NameEdit.Text, variable.ValueEdit.Text);
    end;
end;

procedure TEditListBoxItem.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', '-', #8]) then
    Key := #0;

  if (Key = '-') and ((TEdit(Sender).SelStart <> 0) or (Pos('-', TEdit(Sender).Text) > 0)) then
    Key := #0;
end;

procedure TEditListBoxItem.setMethods(onhidemethod : TNotifyEvent; oncalcmethod : TNotifyEvent);
  begin
    FHideButton.OnClick := onhidemethod;
    FEvaluateButton.OnClick := oncalcmethod;
  end;


procedure TEditListBoxItem.GetRanges(out result : TList<TRangePair>);
  begin
    result := FRanges;
  end;

procedure SetDeleteButtonFunction(EditListBoxItem : TEditListBoxItem; func : TNotifyEvent);
  begin
    EditListBoxItem.FDeleteButton.OnClick := func;
  end;

procedure TEditListBoxItem.GetVariables(out result : TObjectList<TVariablePair>);
begin
  result := FVariables;
end;

function TEditListBoxItem.IfChangedGetMainEditData(out resultStr : string) : Boolean;
begin
  if FMainEdit.Modified then
  begin
    resultStr := FMainEdit.Text;
    Result := True
  end;
end;

procedure TEditListBoxItem.OnVariableNameEdit(Sender: TObject);
  var
  edit : TVariablePair;
  buff : Integer;
  begin
    for edit in self.FVariables do
    if edit = Sender then
      if edit.NameEdit.Text = Self.DependentVarEdit.Text then
        raise Exception.Create('Переменная не может быть зависимой переменной')
      else if TryDecimalStrToInt(edit.NameEdit.Text, buff) then
        raise Exception.Create('Переменная должна быть буквенным');
  end;


constructor TEditListBoxItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRanges := TList<TRangePair>.Create;
  FMaxRanges := 2;

  Height := 150;
  BevelOuter := bvNone;
  FVariables := TObjectList<TVariablePair>.Create(True);
  FColorDialog := TColorDialog.Create(Self);
  FSelectedColor := clWhite;

  FColorButton := TButton.Create(Self);
  FColorButton.Parent := Self;
  FColorButton.Caption := '🎨';
  FColorButton.OnClick := OnColorClick;

  FColorDisplay := TPanel.Create(Self);
  FColorDisplay.Parent := Self;
  FColorDisplay.Width := 24;
  FColorDisplay.Height := 24;
  FColorDisplay.BevelOuter := bvLowered;
  FColorDisplay.Color := FSelectedColor;
  FColorDisplay.Left := FColorButton.Left + FColorButton.Width + 5;
  FColorDisplay.Top := FColorButton.Top;
  FColorDisplay.ParentBackground := False;
  FColorDisplay.ParentColor := False;

  FMainEdit := TEdit.Create(Self);
  FMainEdit.Parent := Self;

  FDependentVarEdit := TEdit.Create(self);
  FDependentVarEdit.Parent := Self;
  FDependentVarEdit.Text := 'y';
  FDependentVarEdit.OnChange := OnDependedVarEdit;

  FDependentLabel := TLabel.Create(self);
  FDependentLabel.Parent := self;
  FDependentLabel.Caption := 'З';

  FDeleteButton := TButton.Create(Self);
  FDeleteButton.Parent := Self;
  FDeleteButton.Caption := '✖';

  FAddVarButton := TButton.Create(Self);
  FAddVarButton.Parent := Self;
  FAddVarButton.Caption := '+ переменная';
  FAddVarButton.OnClick := OnAddVariableClick;

  FAddRangeButton := TButton.Create(Self);
  FAddRangeButton.Parent := Self;
  FAddRangeButton.Caption := '+ диапазон';
  FAddRangeButton.OnClick := OnAddRangeClick;


  FEvaluateButton := TButton.Create(Self);
  FEvaluateButton.Parent := Self;
  FEvaluateButton.Caption := 'Вычислить';

  FHideButton := TButton.Create(Self);
  FHideButton.Parent := Self;
  FHideButton.Caption := 'Скрыть';
  Arrange;
end;

procedure TEditListBoxItem.OnDependedVarEdit(Sender: TObject);
  begin
    if (Length(Self.DependentVarEdit.Text) >= 1) and not (self.DependentVarEdit.Text[1] in ['y','Y','x','X', 'z', 'Z']) then
    begin
      Self.DependentVarEdit.Text := 'y';
      raise Exception.Create('Зависимая переменная должна принадлежать оси');
    end;
  end;

destructor TEditListBoxItem.Destroy;
begin
  FVariables.Free;
  inherited;
end;

procedure TEditListBoxItem.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  FColorDisplay.Color := Value;
end;

procedure TEditListBoxItem.OnColorClick(Sender: TObject);
begin
  FColorDialog.Color := FSelectedColor;
  FColorDialog.Options := [cdFullOpen];

  if FColorDialog.Execute then
  begin
    SetSelectedColor(FColorDialog.Color);
  end;
end;

procedure TEditListBoxItem.Arrange;
var
  TopPos, i: Integer;
  VarPair: TVariablePair;
  RangePair : TRangePair;
begin
  FColorButton.SetBounds(10, 10, 30, 23);
  FColorDisplay.SetBounds(45, 10, 24, 24);
  FDependentLabel.SetBounds(72, 11, 25, 23);
  FDependentVarEdit.SetBounds(80, 10, 25,24);
  FMainEdit.SetBounds(115, 10, 200, 23);
  FDeleteButton.SetBounds(300, 10, 30, 23);

  FAddVarButton.SetBounds(10, 40, 120, 23);
  FAddRangeButton.SetBounds(130, 40, 120, 23);
  TopPos := 70;

  for i := 0 to FRanges.Count - 1 do
  begin
    RangePair := FRanges[i];
    RangePair.NameEdit.SetBounds(10, TopPos, 40, 23);
    RangePair.StartEdit.SetBounds(50, TopPos, 50, 23);
    RangePair.EndEdit.SetBounds(105, TopPos, 100, 23);
    RangePair.DeleteButton.SetBounds(195, TopPos, 30, 23);
    TopPos := TopPos + 30;
  end;

  for i := 0 to FVariables.Count - 1 do
  begin
    VarPair := FVariables[i];
    VarPair.NameEdit.SetBounds(10, TopPos+5, 80, 23);
    VarPair.ValueEdit.SetBounds(95, TopPos+5, 100, 23);
    VarPair.DeleteButton.SetBounds(200, TopPos+5, 30, 23);
    TopPos := TopPos + 30;
  end;

  FEvaluateButton.SetBounds(10, TopPos, 100, 25);
  FHideButton.SetBounds(120, TopPos, 80, 25);
end;

procedure TEditListBoxItem.OnAddVariableClick(Sender: TObject);
var
  buff : TVariablePair;
begin
  AddVarible(buff);
end;


procedure TEditListBoxItem.OnAddRangeClick(Sender: TObject);
var
  buff : TRangePair;
begin
  AddRange(buff);
end;

procedure TEditListBoxItem.OnDeleteRangeClick(Sender: TObject);
var
  i: Integer;
  Pair: TRangePair;
  TargetEdit: TEdit;
begin
  TargetEdit := TEdit(TButton(Sender).Tag);
  for i := FRanges.Count - 1 downto 0 do
  begin
    Pair := FRanges[i];
    if Pair.NameEdit = TargetEdit then
    begin
      self.Height := Self.Height - Pair.NameEdit.Height;
      Pair.NameEdit.Free;
      Pair.StartEdit.Free;
      Pair.EndEdit.Free;
      Pair.DeleteButton.Free;
      FRanges.Delete(i);
      Pair.Free;
      Break;
      Arrange;
      CallListBoxArrange;
    end;
  end;

  Arrange;
end;


procedure TEditListBoxItem.OnDeleteVariableClick(Sender: TObject);
var
  i: Integer;
  VarPair: TVariablePair;
begin
  for i := FVariables.Count - 1 downto 0 do
  begin
    VarPair := FVariables[i];
    if VarPair.DeleteButton = Sender then
    begin
      self.Height := Self.Height - VarPair.NameEdit.Height;
      VarPair.NameEdit.Free;
      VarPair.ValueEdit.Free;
      VarPair.DeleteButton.Free;

      FVariables.Delete(i);

      Arrange;
      CallListBoxArrange;
      Break;
    end;
  end;
end;

end.

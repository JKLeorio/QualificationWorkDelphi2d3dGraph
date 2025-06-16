unit EditListBox ;

interface

uses
  System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, EditListBoxItem,  System.Generics.Collections;

type

  TEditListBox = class(TScrollBox)
  private
    FEditListBoxItems: TObjectList<TEditListBoxItem>;
    FSpacing: Integer;
  public
    onBoxItemHideMethod : TNotifyEvent;
    onBoxItemCalcMethod : TNotifyEvent;
    procedure ArrangeEdits;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnDeleteClick(Sender: TObject);

    procedure AddEdit(out Item : TEditListBoxItem);
    procedure RemoveEdit(Edit: TEditListBoxItem);
    procedure ClearEdits;
    procedure LoadEditListBoxFromFile(const FileName: string; clean : Boolean);
    procedure SaveEditListBoxToFile(const FileName: string);

    property ListBoxItems : TObjectList<TEditListBoxItem> read FEditListBoxItems write FEditListBoxItems;
    property Spacing: Integer read FSpacing write FSpacing;
  end;




implementation

uses System.JSON, System.IOUtils, Unit1;

procedure TEditListBox.SaveEditListBoxToFile(const FileName: string);
var
  JSONArray: TJSONArray;
  i: Integer;
  item: TEditListBoxItem;
  json_data : TJSONObject;
begin
  JSONArray := TJSONArray.Create;
  try
    for i := 0 to ListBoxItems.Count - 1 do
    begin
      item := ListBoxItems[i];
      item.ToJson(json_data);
      JSONArray.AddElement(json_data);
    end;

    TFile.WriteAllText(FileName, JSONArray.ToJSON);
  finally
    JSONArray.Free;
  end;
end;

procedure TEditListBox.LoadEditListBoxFromFile(const FileName: string; clean : Boolean);
var
  JSONArray: TJSONArray;
  i: Integer;
  NewItem: TEditListBoxItem;
  JSONObject: TJSONObject;
begin
  JSONArray := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName)) as TJSONArray;
  try

    for i := 0 to JSONArray.Count - 1 do
    begin
      JSONObject := JSONArray.Items[i] as TJSONObject;
      AddEdit(NewItem);
      NewItem.FromJSON(JSONObject);
    end;
  finally
    JSONArray.Free;
  end;
end;


constructor TEditListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditListBoxItems := TObjectList<TEditListBoxItem>.Create(True);
  FSpacing := 6;
  VertScrollBar.Visible := True;
  AutoScroll := True;
end;

destructor TEditListBox.Destroy;
begin
  ClearEdits;
  FEditListBoxItems.Free;
  inherited;
end;

procedure TEditListBox.AddEdit(out Item : TEditListBoxItem);
begin
  Item := TEditListBoxItem.Create(Self);
  Item.Parent := Self;
  Item.Visible := True;
  Item.setMethods(onBoxItemHideMethod, onBoxItemCalcMethod);
  FEditListBoxItems.Add(Item);
  SetDeleteButtonFunction(item, OnDeleteClick);
  ArrangeEdits;
end;

procedure TEditListBox.RemoveEdit(Edit: TEditListBoxItem);
var
  i: Integer;
  Item: TEditListBoxItem;
begin
  for i := FEditListBoxItems.Count - 1 downto 0 do
  begin
    Item := FEditListBoxItems[i];
    if Item = Edit then
    begin
      FEditListBoxItems.Delete(i);
      Break;
    end;
  end;
  ArrangeEdits;
end;

procedure TEditListBox.ClearEdits;
begin
  FEditListBoxItems.Clear;
  ArrangeEdits;
end;

procedure TEditListBox.OnDeleteClick(Sender: TObject);
var
  Edit: TEditListBoxItem;
  form : TForm1;
begin
  if Sender is TButton then
  begin
    Edit := TButton(Sender).Parent as TEditListBoxItem;
    form := GetParentForm(Self) as TForm1;
    if Assigned(Edit) then
    begin
      form.items_points.Remove(Edit);
      RemoveEdit(Edit);
    end
  end;
end;


procedure TEditListBox.ArrangeEdits;
var
  i, TopPos, Height: Integer;
  Item: TEditListBoxItem;
begin
  TopPos := 0;
  for i := 0 to FEditListBoxItems.Count - 1 do
  begin
    Item := FEditListBoxItems[i];
    Item.SetBounds(0, TopPos, ClientWidth+50, Item.Height);
    TopPos := TopPos + Item.Height-50;
  end;
end;


end.

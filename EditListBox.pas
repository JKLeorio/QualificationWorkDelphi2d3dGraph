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
    procedure ArrangeEdits;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnDeleteClick(Sender: TObject);

    procedure AddEdit;
    procedure RemoveEdit(Edit: TEditListBoxItem);
    procedure ClearEdits;

    property ListBoxItems : TObjectList<TEditListBoxItem> read FEditListBoxItems write FEditListBoxItems;
    property Spacing: Integer read FSpacing write FSpacing;
  end;




implementation



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

procedure TEditListBox.AddEdit;
var
  Item: TEditListBoxItem;
begin
  Item := TEditListBoxItem.Create(Self);
  Item.Parent := Self;
  Item.Visible := True;
  Item.onHideMethod := onBoxItemHideMethod;
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
begin
  if Sender is TButton then
  begin
    Edit := TButton(Sender).Parent as TEditListBoxItem;
    if Assigned(Edit) then
    begin
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
    Item.SetBounds(0, TopPos, ClientWidth, Item.Height);
    TopPos := TopPos + Item.Height;
  end;
end;


end.

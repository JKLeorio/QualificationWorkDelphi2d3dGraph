object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 751
  ClientWidth = 1149
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1149
    Height = 753
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Button2: TButton
        Left = 96
        Top = 542
        Width = 113
        Height = 27
        Caption = #1042#1099#1095#1080#1089#1083#1080#1090#1100' '#1074#1089#1105
        TabOrder = 0
        OnClick = CalculateAll
      end
      object Button3: TButton
        Left = 96
        Top = 484
        Width = 153
        Height = 41
        Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1077#1080#1079#1074#1077#1089#1090#1085#1086#1077
        TabOrder = 1
        OnClick = AddEditListBoxItem
      end
      object Panel2: TPanel
        Left = 96
        Top = 17
        Width = 393
        Height = 461
        TabOrder = 2
      end
      object Button1: TButton
        Left = 1034
        Top = 556
        Width = 75
        Height = 25
        Caption = 'FScreen'
        TabOrder = 3
      end
      object Panel1: TPanel
        Left = 516
        Top = 17
        Width = 593
        Height = 564
        Caption = 'Panel1'
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 393
        Top = 3
        Width = 745
        Height = 558
        Caption = 'Panel3'
        TabOrder = 0
      end
    end
  end
end

object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 754
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
    OnChange = pagecontrolchange
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Button2: TButton
        Left = 96
        Top = 606
        Width = 113
        Height = 27
        Caption = #1042#1099#1095#1080#1089#1083#1080#1090#1100' '#1074#1089#1105
        TabOrder = 0
        OnClick = CalculateAll
      end
      object Button3: TButton
        Left = 96
        Top = 548
        Width = 161
        Height = 45
        Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1075#1088#1072#1092#1080#1082
        TabOrder = 1
        OnClick = AddEditListBoxItem2d
      end
      object Panel2: TPanel
        Left = 96
        Top = 17
        Width = 337
        Height = 512
        BorderStyle = bsSingle
        TabOrder = 2
      end
      object Panel1: TPanel
        Left = 516
        Top = 10
        Width = 593
        Height = 576
        BorderStyle = bsSingle
        TabOrder = 4
      end
      object Button6: TButton
        Left = 965
        Top = 592
        Width = 43
        Height = 25
        Caption = '+'
        TabOrder = 5
        OnClick = ZoomOutBtnClick
      end
      object Button1: TButton
        Left = 1014
        Top = 592
        Width = 95
        Height = 25
        Caption = 'FScreen'
        TabOrder = 3
      end
      object Button7: TButton
        Left = 918
        Top = 592
        Width = 41
        Height = 25
        Caption = '-'
        TabOrder = 6
        OnClick = ZoomInBtnClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 512
        Top = 21
        Width = 578
        Height = 582
        BorderStyle = bsSingle
        TabOrder = 0
      end
      object Button4: TButton
        Left = 96
        Top = 620
        Width = 149
        Height = 29
        Caption = #1042#1099#1095#1080#1089#1083#1080#1090#1100' '#1074#1089#1105
        TabOrder = 1
      end
      object Panel4: TPanel
        Left = 96
        Top = 21
        Width = 329
        Height = 522
        BorderStyle = bsSingle
        ParentBackground = False
        TabOrder = 2
      end
      object Button5: TButton
        Left = 96
        Top = 560
        Width = 169
        Height = 43
        Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1075#1088#1072#1092#1080#1082
        TabOrder = 3
        OnClick = AddEditListBoxItem3d
      end
      object Button8: TButton
        Left = 992
        Top = 609
        Width = 98
        Height = 40
        Caption = 'Fullscreen'
        TabOrder = 4
      end
      object Button9: TButton
        Left = 888
        Top = 616
        Width = 75
        Height = 25
        Caption = 'YZ'
        TabOrder = 5
        OnClick = BtnViewYZClick
      end
      object Button10: TButton
        Left = 807
        Top = 616
        Width = 75
        Height = 25
        Caption = 'XY'
        TabOrder = 6
        OnClick = BtnViewXYClick
      end
      object Button11: TButton
        Left = 726
        Top = 616
        Width = 75
        Height = 25
        Caption = 'XZ'
        TabOrder = 7
        OnClick = BtnViewXZClick
      end
    end
  end
end

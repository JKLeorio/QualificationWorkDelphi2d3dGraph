object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 705
  ClientWidth = 1147
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1147
    Height = 705
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    OnChange = pagecontrolchange
    ExplicitWidth = 1145
    ExplicitHeight = 697
    object TabSheet1: TTabSheet
      Caption = '2D'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 357
        Height = 594
        Align = alLeft
        BorderStyle = bsSingle
        TabOrder = 0
        ExplicitHeight = 586
      end
      object Panel1: TPanel
        Left = 363
        Top = 0
        Width = 776
        Height = 594
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsSingle
        TabOrder = 1
        ExplicitWidth = 774
        ExplicitHeight = 586
      end
      object Panel5: TPanel
        Left = 0
        Top = 594
        Width = 1139
        Height = 81
        Align = alBottom
        BorderStyle = bsSingle
        TabOrder = 2
        ExplicitTop = 586
        ExplicitWidth = 1137
        object Panel6: TPanel
          Left = 1
          Top = 1
          Width = 354
          Height = 75
          Align = alLeft
          TabOrder = 0
          object Button2: TButton
            Left = 1
            Top = 36
            Width = 352
            Height = 38
            Align = alBottom
            Caption = #1042#1099#1095#1080#1089#1083#1080#1090#1100' '#1074#1089#1105
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -20
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = CalculateAll
          end
          object Button3: TButton
            Left = 1
            Top = 1
            Width = 352
            Height = 40
            Align = alTop
            Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1075#1088#1072#1092#1080#1082
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -20
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = AddEditListBoxItem2d
          end
        end
        object Panel7: TPanel
          Left = 359
          Top = 1
          Width = 775
          Height = 75
          Align = alRight
          TabOrder = 1
          ExplicitLeft = 357
          object Button1: TButton
            Left = 536
            Top = 1
            Width = 238
            Height = 73
            Align = alRight
            Caption = 'FScreen'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -20
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = FullScreen
          end
          object Button6: TButton
            Left = 1
            Top = 1
            Width = 272
            Height = 73
            Align = alLeft
            Caption = '+'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -47
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = ZoomOutBtnClick
          end
          object Button7: TButton
            Left = 273
            Top = 1
            Width = 263
            Height = 73
            Align = alClient
            Caption = '-'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -47
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = ZoomInBtnClick
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = '3D'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 364
        Top = 0
        Width = 775
        Height = 592
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsSingle
        TabOrder = 0
        ExplicitHeight = 589
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 358
        Height = 592
        Align = alLeft
        BorderStyle = bsSingle
        ParentBackground = False
        TabOrder = 1
      end
      object Panel8: TPanel
        Left = 0
        Top = 592
        Width = 1139
        Height = 83
        Align = alBottom
        BorderStyle = bsSingle
        TabOrder = 2
        object Panel9: TPanel
          Left = 362
          Top = 1
          Width = 772
          Height = 77
          Align = alRight
          TabOrder = 0
          object Button11: TButton
            Left = 1
            Top = 1
            Width = 214
            Height = 75
            Align = alLeft
            Caption = 'XZ'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -40
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = BtnViewXZClick
          end
          object Button9: TButton
            Left = 215
            Top = 1
            Width = 239
            Height = 75
            Align = alClient
            Caption = 'YZ'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -40
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = BtnViewYZClick
          end
          object Button8: TButton
            Left = 673
            Top = 1
            Width = 98
            Height = 75
            Align = alRight
            Caption = 'Fullscreen'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -20
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = FullScreen
          end
          object Button10: TButton
            Left = 454
            Top = 1
            Width = 219
            Height = 75
            Align = alRight
            Caption = 'XY'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -40
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 3
            OnClick = BtnViewXYClick
          end
        end
        object Panel10: TPanel
          Left = 1
          Top = 1
          Width = 355
          Height = 77
          Align = alLeft
          TabOrder = 1
          object Button5: TButton
            Left = 1
            Top = 1
            Width = 353
            Height = 43
            Align = alTop
            Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1075#1088#1072#1092#1080#1082
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -20
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            OnClick = AddEditListBoxItem3d
            ExplicitLeft = 8
            ExplicitTop = 3
            ExplicitWidth = 347
          end
          object Button4: TButton
            Left = 1
            Top = 37
            Width = 353
            Height = 39
            Align = alBottom
            Caption = #1042#1099#1095#1080#1089#1083#1080#1090#1100' '#1074#1089#1105
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -20
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            OnClick = CalculateAll
            ExplicitTop = 40
            ExplicitWidth = 347
          end
        end
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 568
    Top = 392
    object N1: TMenuItem
      Caption = #1060#1072#1081#1083
      object N4: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083
        OnClick = SaveMenuItemClick
      end
      object N5: TMenuItem
        Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1079' '#1092#1072#1081#1083#1072
        OnClick = LoadMenuItemClick
      end
    end
    object N2: TMenuItem
      Caption = #1057#1087#1088#1072#1074#1082#1072
      OnClick = HelpMenuItemClick
    end
    object N3: TMenuItem
      Caption = #1054#1073' '#1072#1074#1090#1086#1088#1077
      OnClick = AboutMenuItemClick
    end
  end
end

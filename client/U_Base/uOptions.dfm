object frmOptions: TfrmOptions
  Left = 198
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 209
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    396
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 232
    Top = 181
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOkClick
  end
  object Button2: TButton
    Left = 312
    Top = 181
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 381
    Height = 162
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akBottom]
    TabIndex = 0
    TabOrder = 2
    object tsGeneral: TTabSheet
      Caption = 'General'
      object lLLanguage: TLabel
        Left = 28
        Top = 36
        Width = 51
        Height = 13
        Caption = 'Language:'
      end
      object cmLanguage: TComboBox
        Left = 104
        Top = 32
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object cbAutoTabs: TCheckBox
        Left = 28
        Top = 68
        Width = 221
        Height = 17
        Caption = 'Autoselect info tabs'
        TabOrder = 1
      end
      object cbPersonImages: TCheckBox
        Left = 28
        Top = 88
        Width = 197
        Height = 17
        Caption = 'Show person images'
        TabOrder = 2
      end
    end
  end
end

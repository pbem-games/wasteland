object frmFactions: TfrmFactions
  Tag = 3
  Left = 206
  Top = 103
  HelpContext = 9
  Anchors = []
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Factions'
  ClientHeight = 436
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000008FFFFFF000000000
    080000F0000000008FFFFFF00000000080000F8000000000FFFF880000000000
    80000F800000000008FFFFF000000000800F0FF000000000FFFFF00000000000
    FFFFF0000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000E00F0000E00F0000F00F0000E00F0000E00F0000E01F0000E00F
    0000F00F0000E00F0000E00F0000E01F0000E03F0000FFFF0000FFFF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 316
    Top = 4
    Width = 97
    Height = 397
    Caption = 'Faction'
    TabOrder = 0
    object lLAttitude: TLabel
      Left = 12
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Attitude:'
    end
    object lLColor: TLabel
      Left = 12
      Top = 70
      Width = 27
      Height = 13
      Caption = 'Color:'
    end
    object cmAttitude: TComboBox
      Left = 12
      Top = 40
      Width = 73
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmAttitudeChange
      Items.Strings = (
        'Hostile'
        'Unfriendly'
        'Neutral'
        'Friendly'
        'Ally')
    end
    object ColorBtn: TColorBtn
      Left = 12
      Top = 84
      Width = 34
      Height = 17
      Color = -1
      ColorDialog = ColorDialog
      Transparency = True
      OnClick = ColorBtnClick
    end
  end
  object Button2: TButton
    Left = 336
    Top = 408
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    TabOrder = 1
    OnClick = Button2Click
  end
  object FactionGrid: TPowerGrid
    Left = 4
    Top = 4
    Width = 305
    Height = 429
    Color = clBlack
    ColCount = 3
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoSortOnClick, pgoRowSelect]
    RowCount = 1
    Sorted = True
    SortBy = 0
    StickySelect = True
    TopRow = 1
    OnDrawCell = FactionGridDrawCell
    OnSelectCell = FactionGridSelectCell
    ColWidths = (
      169
      40
      71)
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 392
    Top = 116
  end
end

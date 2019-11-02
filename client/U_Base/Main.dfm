object frmMain: TfrmMain
  Left = 198
  Top = 103
  Width = 556
  Height = 454
  Caption = 'Wasteland Advisor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 548
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 548
      Height = 2
      Align = alTop
    end
    object lCoords: TLabel
      Left = 8
      Top = 6
      Width = 37
      Height = 15
      AutoSize = False
      Caption = '0, 0'
    end
    object ToolBar1: TToolBar
      Left = 40
      Top = 2
      Width = 493
      Height = 29
      Align = alNone
      Caption = 'ToolBar1'
      EdgeBorders = []
      Images = frmResources.BtnImages
      Indent = 6
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object ToolButton1: TToolButton
        Left = 6
        Top = 2
        Action = actLoadReport
      end
      object ToolButton2: TToolButton
        Left = 29
        Top = 2
        Action = actPasteReport
      end
      object ToolButton3: TToolButton
        Left = 52
        Top = 2
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 60
        Top = 2
        Action = actViewOrders
      end
      object ToolButton5: TToolButton
        Left = 83
        Top = 2
        Action = actSaveOrders
      end
      object ToolButton6: TToolButton
        Left = 106
        Top = 2
        Action = actCopyOrders
      end
      object ToolButton7: TToolButton
        Left = 129
        Top = 2
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 137
        Top = 2
        Action = actFactions
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 29
    Width = 548
    Height = 379
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 1
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 347
      Height = 377
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 254
        Width = 347
        Height = 3
        Cursor = crVSplit
        Align = alBottom
      end
      object HexMap: THexMap
        Left = 0
        Top = 0
        Width = 347
        Height = 254
        Align = alClient
        Color = clBlack
        GridColor = clGray
        HexSize = 48
        CellWidth = 48
        CellHeight = 42
        Margin = 15
        OnDragOver = HexMapDragOver
        OnDrawHex = HexMapDrawHex
        OnDrawExtra = HexMapDrawExtra
        OnMouseDown = HexMapMouseDown
        OnMouseMove = HexMapMouseMove
        OnSelectHex = HexMapSelectHex
      end
      object gUnits: TPowerGrid
        Left = 0
        Top = 257
        Width = 347
        Height = 120
        Align = alBottom
        Color = clBlack
        ColCount = 12
        DefaultRowColor = clBlack
        DefaultRowHeight = 19
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoRowSelect]
        RowCount = 2
        Sorted = False
        SortBy = 0
        StickySelect = True
        TopRow = 1
        OnDrawCell = gUnitsDrawCell
        OnDragOver = gUnitsDragOver
        OnSelectCell = gUnitsSelectCell
        ColWidths = (
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64)
      end
    end
    object PageControl: TPageControl
      Left = 348
      Top = 1
      Width = 199
      Height = 377
      ActivePage = tsPerson
      Align = alRight
      TabIndex = 1
      TabOrder = 1
      object tsRegion: TTabSheet
        Caption = 'Region'
        object pRegionGrids: TPanel
          Left = 0
          Top = 93
          Width = 191
          Height = 256
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnResize = pRegionGridsResize
          object lResources: TLabel
            Left = 4
            Top = 0
            Width = 61
            Height = 13
            Caption = 'Resources'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lJunk: TLabel
            Left = 4
            Top = 136
            Width = 28
            Height = 13
            Caption = 'Junk'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object gJunk: TPowerGrid
            Left = 1
            Top = 16
            Width = 189
            Height = 117
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDrawCell = gItemsDrawCell
            ColWidths = (
              46
              139)
          end
          object gResources: TPowerGrid
            Left = 1
            Top = 154
            Width = 189
            Height = 121
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDrawCell = gItemsDrawCell
            ColWidths = (
              46
              139)
          end
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 191
          Height = 93
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object lForecast: TLabel
            Left = 56
            Top = 68
            Width = 129
            Height = 13
            AutoSize = False
            Caption = 'clear, 10'#176'C, 100 mR/h'
          end
          object lLForecast: TLabel
            Left = 4
            Top = 68
            Width = 44
            Height = 13
            Caption = 'Forecast:'
          end
          object lLTurn: TLabel
            Left = 4
            Top = 48
            Width = 25
            Height = 13
            Caption = 'Turn:'
          end
          object lTurn: TLabel
            Left = 56
            Top = 48
            Width = 33
            Height = 13
            Caption = 'current'
          end
          object lLHex: TLabel
            Left = 4
            Top = 28
            Width = 22
            Height = 13
            Caption = 'Hex:'
          end
          object lHex: TLabel
            Left = 56
            Top = 28
            Width = 129
            Height = 13
            AutoSize = False
            Caption = 'glowing ruins (13,13)'
          end
          object lLand: TLabel
            Left = 8
            Top = 4
            Width = 177
            Height = 17
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Great Wastes'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
      object tsPerson: TTabSheet
        Caption = 'Person'
        ImageIndex = 1
        object pPerson: TPanel
          Left = 0
          Top = 29
          Width = 191
          Height = 320
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnResize = pPersonResize
          object gItems: TPowerGrid
            Left = 0
            Top = 0
            Width = 189
            Height = 53
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDrawCell = gItemsDrawCell
            OnDragOver = gItemsDragOver
            OnEndDrag = gItemsEndDrag
            OnMouseDown = gItemsMouseDown
            OnSelectCell = gItemsSelectCell
            ColWidths = (
              41
              144)
          end
          object gSkills: TPowerGrid
            Left = 0
            Top = 88
            Width = 189
            Height = 69
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDrawCell = gSkillsDrawCell
            ColWidths = (
              135
              50)
          end
          object pcPerson: TPageControl
            Left = 0
            Top = 160
            Width = 189
            Height = 141
            ActivePage = tsOrders
            TabIndex = 0
            TabOrder = 2
            TabPosition = tpBottom
            object tsOrders: TTabSheet
              Caption = 'Orders'
              object mOrders: TMemo
                Left = 0
                Top = 21
                Width = 181
                Height = 94
                Align = alClient
                ScrollBars = ssVertical
                TabOrder = 0
                OnExit = mOrdersExit
              end
              object ToolBar2: TToolBar
                Left = 0
                Top = 0
                Width = 181
                Height = 21
                ButtonHeight = 18
                ButtonWidth = 19
                Caption = 'ToolBar2'
                EdgeBorders = []
                Flat = True
                Images = frmResources.SmallImages
                Indent = 140
                TabOrder = 1
                object btnCheck: TToolButton
                  Left = 140
                  Top = 0
                  Hint = 'Check'
                  Caption = 'btnCheck'
                  ImageIndex = 0
                  ParentShowHint = False
                  ShowHint = True
                  OnClick = btnCheckClick
                end
                object btnClear: TToolButton
                  Left = 159
                  Top = 0
                  Hint = 'Clear'
                  Caption = 'btnClear'
                  ImageIndex = 1
                  ParentShowHint = False
                  ShowHint = True
                  OnClick = btnClearClick
                end
              end
            end
            object tsEvents: TTabSheet
              Caption = 'Events'
              ImageIndex = 1
              object gEvents: TPowerGrid
                Left = 0
                Top = 0
                Width = 181
                Height = 115
                Align = alClient
                ColCount = 2
                DefaultRowColor = clBlack
                Editing = False
                FixedRows = 0
                Images = frmResources.BtnImages
                ImageCol = 0
                LinesColor = clWindow
                Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoNoSelect, pgoStretchLastCol, pgoMultilineCells]
                RowCount = 0
                Sorted = False
                SortBy = 0
                StickySelect = False
                TopRow = 0
                ColWidths = (
                  19
                  158)
              end
            end
          end
          object pGive: TPanel
            Left = 0
            Top = 56
            Width = 189
            Height = 29
            BevelOuter = bvNone
            TabOrder = 3
            object lGive: TLabel
              Left = 100
              Top = 6
              Width = 25
              Height = 13
              Caption = 'Give:'
            end
            object imgPerson: TImage
              Left = 8
              Top = 4
              Width = 16
              Height = 16
              Hint = 'team / enter / leave (drag to target or to map)'
              ParentShowHint = False
              Picture.Data = {
                07544269746D617036040000424D360400000000000036000000280000001000
                0000100000000100200000000000000400000000000000000000000000000000
                0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000000000FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
                0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0000000000000000000000000000000000FFFF0000B5B500000000
                0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000084FFFF0084FFFF0000000000FFFF0000B5B500000000
                0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000000000000000000000000000000000FFFF0000FFFF0000FFFF0000B5B5
                000000000000FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00
                FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
                0000B5B5000000000000000000000000000000000000FF00FF00FF00FF00FF00
                FF0000000000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000
                00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00
                FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
                000000000000FFFFFF00FFFFFF00FFFFFF000000000000000000FF00FF00FF00
                FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
                000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00000000000000
                000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
                00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
                0000FF00FF000000000000000000000000000000000000000000000000000000
                00000000000000000000FFFFFF00FFFFFF00848484008484840000000000FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000000000000000000000000000000000000000000000000000000000FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00}
              ShowHint = True
              Transparent = True
              OnEndDrag = imgPersonEndDrag
              OnMouseDown = imgPersonMouseDown
            end
            object eGiveAmt: TIntEdit
              Left = 132
              Top = 2
              Width = 57
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
          end
        end
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 191
          Height = 29
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object lUnitNum: TLabel
            Left = 4
            Top = 4
            Width = 37
            Height = 13
            AutoSize = False
            Caption = '12345'
          end
          object lUnitName: TLabel
            Left = 40
            Top = 4
            Width = 145
            Height = 17
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'John Connor'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
      object tsObject: TTabSheet
        Caption = 'Object'
        ImageIndex = 2
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 191
          Height = 53
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lStructName: TLabel
            Left = 40
            Top = 4
            Width = 145
            Height = 17
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'T-75'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lStructNum: TLabel
            Left = 4
            Top = 4
            Width = 37
            Height = 13
            AutoSize = False
            Caption = '12345'
          end
          object lLStructType: TLabel
            Left = 4
            Top = 28
            Width = 27
            Height = 13
            Caption = 'Type:'
          end
          object lStructType: TLabel
            Left = 40
            Top = 28
            Width = 21
            Height = 13
            Caption = 'tank'
          end
        end
        object Panel7: TPanel
          Left = 0
          Top = 53
          Width = 191
          Height = 296
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object gStructItems: TPowerGrid
            Left = 0
            Top = 0
            Width = 191
            Height = 296
            Align = alClient
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDrawCell = gItemsDrawCell
            ColWidths = (
              41
              146)
          end
        end
      end
    end
  end
  object MainMenu: TMainMenu
    Images = frmResources.BtnImages
    Left = 20
    Top = 24
    object itmFile: TMenuItem
      Caption = 'File'
      object CreateGame1: TMenuItem
        Action = actNewGame
      end
      object itmOpenGame: TMenuItem
        Action = actOpenGame
      end
      object SaveGame1: TMenuItem
        Action = actSaveGame
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object LoadReport1: TMenuItem
        Action = actLoadReport
      end
      object PasteReport1: TMenuItem
        Action = actPasteReport
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ViewOrders1: TMenuItem
        Action = actViewOrders
      end
      object SaveOrders1: TMenuItem
        Action = actSaveOrders
      end
      object SaveGameAs1: TMenuItem
        Action = actSaveOrdersAs
      end
      object CopyOrderstoClipboard1: TMenuItem
        Action = actCopyOrders
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Options1: TMenuItem
        Action = actOptions
      end
    end
    object itmTools: TMenuItem
      Caption = 'Tools'
      object Factions1: TMenuItem
        Action = actFactions
      end
    end
    object itmMap: TMenuItem
      Caption = 'Map'
      object itmFogType: TMenuItem
        Caption = 'Fog Type'
        ImageIndex = 2
        object itmFogDisabled: TMenuItem
          Caption = 'Disabled'
          RadioItem = True
          OnClick = itmFogTypeClick
        end
        object itmFogVisible: TMenuItem
          Caption = 'Visible Regions'
          RadioItem = True
          OnClick = itmFogTypeClick
        end
        object itmFogVisited: TMenuItem
          Caption = 'Visited Regions'
          RadioItem = True
          OnClick = itmFogTypeClick
        end
      end
      object itmFlags: TMenuItem
        Caption = 'Flags'
        OnClick = itmFlagsClick
      end
    end
  end
  object ActionList: TActionList
    Images = frmResources.BtnImages
    Left = 60
    Top = 24
    object actLoadReport: TAction
      Caption = 'Load Report...'
      Hint = 'Load Report'
      ImageIndex = 0
      OnExecute = actLoadReportExecute
    end
    object actPasteReport: TAction
      Caption = 'Paste Report'
      Hint = 'Paste Report'
      ImageIndex = 1
      OnExecute = actPasteReportExecute
    end
    object actOptions: TAction
      Caption = 'Options'
      Hint = 'Options'
      OnExecute = actOptionsExecute
    end
    object actNewGame: TAction
      Caption = 'New Game'
      OnExecute = actNewGameExecute
    end
    object actOpenGame: TAction
      Caption = 'Open Game...'
      OnExecute = actOpenGameExecute
    end
    object actSaveGame: TAction
      Caption = 'Save Game As...'
      OnExecute = actSaveGameExecute
    end
    object actViewOrders: TAction
      Caption = 'View Orders'
      Hint = 'View Orders'
      ImageIndex = 4
      OnExecute = actViewOrdersExecute
    end
    object actCopyOrders: TAction
      Caption = 'Copy Orders to Clipboard'
      Hint = 'Copy Orders to Clipboard'
      ImageIndex = 5
      OnExecute = actCopyOrdersExecute
    end
    object actSaveOrdersAs: TAction
      Caption = 'Save Orders As...'
      Hint = 'Save Orders As...'
      OnExecute = actSaveOrdersAsExecute
    end
    object actSaveOrders: TAction
      Caption = 'Save Orders'
      Hint = 'Save Orders'
      ImageIndex = 6
      OnExecute = actSaveOrdersExecute
    end
    object actFactions: TAction
      Caption = 'Factions'
      Hint = 'Factions'
      ImageIndex = 7
      OnExecute = actFactionsExecute
    end
  end
end

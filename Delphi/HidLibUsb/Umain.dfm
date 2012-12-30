object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 400
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object MemoDebug: TMemo
    Left = 40
    Top = 48
    Width = 297
    Height = 217
    TabOrder = 0
    WordWrap = False
  end
  object Button3: TButton
    Left = 40
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Init'
    TabOrder = 1
    OnClick = Button3Click
  end
  object ComboBox1: TComboBox
    Left = 40
    Top = 8
    Width = 145
    Height = 24
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 2
    Text = 'Arduino'
    Items.Strings = (
      'Arduino'
      'MBed Controller')
  end
  object btnEnable: TButton
    Left = 200
    Top = 288
    Width = 137
    Height = 25
    Caption = 'Enable Demo mode'
    TabOrder = 3
    OnClick = btnEnableClick
  end
  object btnDisable: TButton
    Left = 200
    Top = 328
    Width = 137
    Height = 25
    Caption = 'Disable Demo Mode'
    TabOrder = 4
    OnClick = btnDisableClick
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 216
    Top = 368
  end
end

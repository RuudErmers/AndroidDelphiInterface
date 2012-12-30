object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Adb Host Simulator'
  ClientHeight = 585
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Memo1: TMemo
    Left = 40
    Top = 8
    Width = 473
    Height = 473
    TabOrder = 0
    WordWrap = False
  end
  object btnInit: TButton
    Left = 40
    Top = 504
    Width = 75
    Height = 25
    Caption = 'Init'
    TabOrder = 1
    OnClick = btnInitClick
  end
  object btnEnable: TButton
    Left = 376
    Top = 504
    Width = 137
    Height = 25
    Caption = 'Enable Demo mode'
    TabOrder = 2
    OnClick = btnEnableClick
  end
  object btnDisable: TButton
    Left = 376
    Top = 544
    Width = 137
    Height = 25
    Caption = 'Disable Demo Mode'
    TabOrder = 3
    OnClick = btnDisableClick
  end
  object btnOpen: TButton
    Left = 144
    Top = 504
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 4
    OnClick = btnOpenClick
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 480
    Top = 504
  end
end

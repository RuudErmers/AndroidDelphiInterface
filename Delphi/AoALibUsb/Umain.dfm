object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 572
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
  object Label1: TLabel
    Left = 48
    Top = 16
    Width = 25
    Height = 16
    Caption = 'VID:'
  end
  object Label2: TLabel
    Left = 48
    Top = 48
    Width = 24
    Height = 16
    Caption = 'PID:'
  end
  object Memo1: TMemo
    Left = 40
    Top = 88
    Width = 473
    Height = 241
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
  object Button4: TButton
    Left = 144
    Top = 344
    Width = 113
    Height = 25
    Caption = 'Accesory Mode'
    TabOrder = 2
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 96
    Top = 16
    Width = 121
    Height = 24
    TabOrder = 3
    Text = '6353'
  end
  object Edit2: TEdit
    Left = 96
    Top = 48
    Width = 121
    Height = 24
    TabOrder = 4
    Text = '57069'
  end
  object RadioButton1: TRadioButton
    Left = 280
    Top = 24
    Width = 113
    Height = 17
    Caption = 'ADB on $2D00'
    TabOrder = 5
  end
  object rbadbon2d01: TRadioButton
    Left = 280
    Top = 47
    Width = 113
    Height = 17
    Caption = 'ADB on $2D01'
    Checked = True
    TabOrder = 6
    TabStop = True
  end
  object PageControl1: TPageControl
    Left = 40
    Top = 375
    Width = 473
    Height = 193
    ActivePage = TabSheet1
    TabOrder = 7
    object TabSheet1: TTabSheet
      Caption = 'Microchip'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object cb1: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Button 1'
        TabOrder = 0
        OnClick = cb1Click
      end
      object cb2: TCheckBox
        Left = 16
        Top = 40
        Width = 97
        Height = 17
        Caption = 'Button 2'
        TabOrder = 1
        OnClick = cb1Click
      end
      object cb3: TCheckBox
        Left = 16
        Top = 63
        Width = 97
        Height = 17
        Caption = 'Button 3'
        TabOrder = 2
        OnClick = cb1Click
      end
      object SpinEdit1: TSpinEdit
        Left = 168
        Top = 15
        Width = 157
        Height = 26
        MaxValue = 100
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = SpinEdit1Change
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'String Protocol'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Button1: TButton
        Left = 176
        Top = 41
        Width = 137
        Height = 25
        Caption = 'Enable Demo mode'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 176
        Top = 10
        Width = 137
        Height = 25
        Caption = 'Disable Demo Mode'
        TabOrder = 1
        OnClick = Button2Click
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 480
    Top = 344
  end
end

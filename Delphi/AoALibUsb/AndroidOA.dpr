program AndroidOA;

uses
  Forms,
  Umain in 'Umain.pas' {FormMain},
  ULibUSBAoa in 'ULibUSBAoa.pas',
  libusbK in '..\libusbK.pas',
  ULibUSBDevice in '..\ULibUSBDevice.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

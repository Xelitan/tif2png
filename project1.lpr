program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Classes, Graphics, SysUtils, TifImage;

const PROG = 'tif2png';
      VERSION = '1.0';

function Convert(InName, OutName: String): Integer;
var Pic: TPicture;
    Img: TGraphic;
    Ext: String;
    Bmp: TBitmap;
    H: TTifImage;
begin
  Result := 0;

  try
    H := TTifImage.Create;
    H.LoadFromFile(InName);

    Bmp := TBitmap.Create;
    Bmp.Assign(H.ToBitmap);
    H.Free;
  except
    Writeln('Conversion error');
    Exit(1);
  end;

  Ext := LowerCase(ExtractFileExt(OutName));

  if Ext = '.bmp' then Img := TBitmap.Create
  else if Ext = '.jpg' then Img := TJPEGImage.Create
  else if Ext = '.ppm' then Img := TPortableAnyMapGraphic.Create
  else if Ext = '.png' then Img := TPortableNetworkGraphic.Create;

  Img.Assign(Bmp);
  Bmp.Free;

  Img.SaveToFile(OutName);
  Img.Free;
end;

begin
  if ParamCount <> 2 then begin
    Writeln('===================================================');
    Writeln('  ', PROG, ' - .TIFF to .PNG image converter');
    Writeln('  github.com/Xelitan/', PROG);
    Writeln('  version: ', VERSION);
    Writeln('  license: BSD 3-Clause'); //similar to LibTIFF
    Writeln('===================================================');
    Writeln('  Usage: ', PROG, ' INPUT OUTPUT');
    Writeln('  Output format is guessed from extension.');
    Writeln('  Supported: bmp,jpg,png,ppm');
    ExitCode := 0;
    Exit;
  end;

  ExitCode := Convert(ParamStr(1), ParamStr(2));
end.




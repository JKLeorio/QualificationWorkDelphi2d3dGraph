unit MathUtils;

interface
  function TryDecimalStrToInt(const S: string; out Value: Integer): Boolean;

implementation

uses System.SysUtils;

function TryDecimalStrToInt(const S: string; out Value: Integer): Boolean;
  begin
    value := 10;
    result := ( pos( '$', S ) = 0 ) and TryStrToInt(S, Value );
  end;


end.

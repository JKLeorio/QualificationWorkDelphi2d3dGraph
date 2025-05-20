unit Parser;

interface

uses
  Generics.Collections, MathUtils;

type
  TGraphPoint = record
    x : Float64;
    y : Float64;
    z : Float64
  end;



  TFunction = reference to function(a, b : Float64) : Float64;

  TOperator = record
    func : TFunction;
    priority : Word;
  end;

  TUnaryFunc = reference to function(x: Float64): Float64;



  TMathExpressionCalc = class
    OPERATORS : TDictionary<string, TOperator>;
    UNARY_OPERATORS : TDictionary<string, TUnaryFunc>;
    variables : TDictionary<string, float64>;
    private
    public
      constructor Create;
      destructor Destroy;
      function Parse(math_expression : string; out resultData : TQueue<string>; variables : TDictionary<string, float64>) : Boolean;
      
      function CalculationBySymbol2D(translated_expression : TQueue<string>;
       variables : TDictionary<string, float64>; dependentVar : string;
       range_variable:string; out resultPoint : TGraphPoint) : Boolean;
       
      function CalcPointsByRange(rstart : integer; rend : integer; variables : TDictionary<string,
       float64>; dependentVar : string; range_variable : string;
       out points : TArray<TGraphPoint>; math_expression_copy : array of string) : Boolean;
        
      procedure SetVariables(variables : TDictionary<string, float64>);
  end;

  function SolveExpressionSymPy(const Equation, Variable: string): string;
  function OccurrencesOfChar(const S: string; const C: char): integer;
  function GetRightSideOfAssignment(expr: string): string; 

implementation
  uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes,
    System.Math,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls;



function GetRightSideOfAssignment(expr: string): string;
  var
    rightPart: string;
    equalPos: Integer;
  begin
    equalPos := Pos('=', expr);
    if equalPos > 0 then
      rightPart := Copy(expr, equalPos + 1, Length(expr) - equalPos)
    else
      rightPart := expr;
    Result := rightPart;
  end;
  
function GetDosOutput(const CommandLine: string): string;
  var
    SA: TSecurityAttributes;
    StdOutRead, StdOutWrite: THandle;
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    Buffer: array[0..255] of AnsiChar;
    BytesRead: DWORD;
    AppRunning: DWORD;
    SL: TStringList;
  begin
    Result := '';
    SL := TStringList.Create;
    try
      ZeroMemory(@SA, SizeOf(SA));
      SA.nLength := SizeOf(SA);
      SA.bInheritHandle := True;

      if CreatePipe(StdOutRead, StdOutWrite, @SA, 0) then
      begin
        ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
        StartupInfo.cb := SizeOf(StartupInfo);
        StartupInfo.hStdOutput := StdOutWrite;
        StartupInfo.hStdError := StdOutWrite;
        StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        StartupInfo.wShowWindow := SW_HIDE;

        if CreateProcess(nil, PChar(CommandLine), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
        begin
          CloseHandle(StdOutWrite);
          repeat
            AppRunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
            repeat
              BytesRead := 0;
              ReadFile(StdOutRead, Buffer, 255, BytesRead, nil);
              if BytesRead > 0 then
              begin
                Buffer[BytesRead] := #0;
                SL.Text := SL.Text + string(Buffer);
              end;
            until BytesRead = 0;
          until AppRunning <> WAIT_TIMEOUT;
          CloseHandle(ProcessInfo.hProcess);
          CloseHandle(ProcessInfo.hThread);
        end;
        CloseHandle(StdOutRead);
      end;
      Result := Trim(SL.Text);
    finally
      SL.Free;
    end;
  end;

function SolveExpressionSymPy(const Equation, Variable: string): string;
var
  PythonExe, ScriptPath, Cmd: string;
begin
  PythonExe := 'python';
  ScriptPath := ExtractFilePath(Application.ExeName) + 'solve_expr.py';
  Cmd := Format('%s "%s" "%s" "%s"', [PythonExe, ScriptPath, Equation, Variable]);
  Result := GetDosOutput(Cmd);
end;


function OccurrencesOfChar(const S: string; const C: char): integer;
  var
    i: Integer;
  begin
    result := 0;
    for i := 1 to Length(S) do
      if S[i] = C then
        inc(result);
  end;

  procedure TMathExpressionCalc.SetVariables(variables : TDictionary<string, float64>);
  begin
    Self.variables := variables;
  end;

  constructor TMathExpressionCalc.Create;
  var
    addition : TOperator;
    subtraction : TOperator;
    multiplication : TOperator;
    division : TOperator;
    cpower : TOperator;

  begin
    inherited Create;
    with addition do
    begin
      func := function(a,b : Float64): Float64 begin Result:= a + b end;
      priority := 1;
    end;
    with subtraction do
    begin
      func := function(a,b : Float64): Float64 begin Result:= a - b end;
      priority := 1;
    end;
    with multiplication do
    begin
      func := function(a,b : Float64): Float64 begin Result:= a * b end;
      priority := 2;
    end;
    with division do
    begin
      func := function(a,b : Float64): Float64 begin if (a <> 0) or (b <> 0) then
        Result:= a / b
      else
        raise Exception.Create('division by zero') end;
      priority := 2;
    end;
    with cpower do
    begin
      func := function(a, b: Float64): Float64 begin Result := Power(a, b) end;
      priority := 3;
    end;

    OPERATORS := TDictionary<string, TOperator>.Create;
    OPERATORS.Add('+', addition);
    OPERATORS.Add('-', subtraction);
    OPERATORS.Add('*', multiplication);
    OPERATORS.Add('/', division);
    OPERATORS.Add('**', cpower);

    UNARY_OPERATORS := TDictionary<string, TUnaryFunc>.Create;
    UNARY_OPERATORS.Add('sin', function(x: Float64): Float64 begin Result := Sin(x); end);
    UNARY_OPERATORS.Add('cos', function(x: Float64): Float64 begin Result := Cos(x); end);
    UNARY_OPERATORS.Add('sqrt', function(x: Float64): Float64 begin Result := Sqrt(x); end);
    UNARY_OPERATORS.Add('neg', function(x: Float64): Float64 begin Result := -x; end);
  end;

  destructor TMathExpressionCalc.Destroy;
  begin
    self.OPERATORS.Free;
    inherited Destroy;
  end;

  function TMathExpressionCalc.Parse(math_expression: string; out resultData : TQueue<string>; variables : TDictionary<string, float64>) : Boolean;

  var
  i: Integer;
  c, next: Char;
  resultExpression : TQueue<string>;
  stack : TStack<string>;
  character : Char;
  buff : Integer;
  StackBuff : string;
  number : string;
  E : Exception;
  a,b : Word;
  funcName : string;
  item, elem : string;


  procedure PushNumber(var number : string);
  begin
    if Length(number) <> 0 then
    begin
      resultExpression.Enqueue(number);
      number := '';

      while (stack.Count > 0) and UNARY_OPERATORS.ContainsKey(stack.Peek) do
        resultExpression.Enqueue(stack.Pop);
    end;
  end;


  function IsValid(math_expression: string): Boolean;
    var
      i: Integer;
      c: Char;
      token, prevToken: string;
      openBrackets, closeBrackets: Integer;
      isLastTokenOperator: Boolean;
      buff : float64;
    begin
      openBrackets := 0;
      closeBrackets := 0;
      prevToken := '';
      isLastTokenOperator := False;

      i := 1;
      while i <= Length(math_expression) do
      begin
        c := math_expression[i];


        if c = ' ' then
        begin
          Inc(i);
          Continue;
        end;


        if c = '(' then
        begin
          Inc(openBrackets);
          isLastTokenOperator := True;
        end
        else if c = ')' then
        begin
          Inc(closeBrackets);
          if isLastTokenOperator then
          begin
            Result := False;
            Exit;
          end;
        end


        else if OPERATORS.ContainsKey(c) then
        begin
          if (prevToken = '') or (isLastTokenOperator) then
          begin
            Result := False;
            Exit;
          end;
          isLastTokenOperator := True;
        end

        else
        begin
          token := '';
          while (i <= Length(math_expression)) and
                (math_expression[i] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '_']) do
          begin
            token := token + math_expression[i];
            Inc(i);
          end;
          Dec(i);

          if token <> '' then
          begin
            if not TryStrToFloat(token, buff) and
               not variables.ContainsKey(token) and
               not UNARY_OPERATORS.ContainsKey(token) then
            begin
              Result := False;
              Exit;
            end;

            isLastTokenOperator := False;
            prevToken := token;
          end;
        end;

        Inc(i);
      end;

      if openBrackets <> closeBrackets then
      begin
        Result := False;
        Exit;
      end;

      if isLastTokenOperator then
      begin
        Result := False;
        Exit;
      end;

      Result := True;
    end;


  begin
  i := 1;
  stack := TStack<string>.Create;
  resultExpression := TQueue<string>.Create;
  math_expression := StringReplace(math_expression, ' ', '', [rfReplaceAll]);
  math_expression := GetRightSideOfAssignment(math_expression);
  ShowMessage(math_expression);
  while i <= Length(math_expression) do
    begin
      character := math_expression[i];
      if character in ['a'..'z', 'A'..'Z'] then
      begin
        funcName := '';
        while (i <= Length(math_expression)) and (math_expression[i] in ['a'..'z', 'A'..'Z']) do
        begin
          funcName := funcName + math_expression[i];
          Inc(i);
        end;

        if UNARY_OPERATORS.ContainsKey(funcName) then
        begin
          stack.Push(funcName);
          Continue;
        end
        else if variables.ContainsKey(funcName) then
        begin
          number := funcName;
          PushNumber(number);
        end
        else
          raise Exception.Create('Unknown function or variable: ' + funcName);
        Dec(i);
      end

      else if (character in ['0'..'9']) or (character = '.') then
      begin
        number := number + character;
      end

      else if (character = '*') and (i < Length(math_expression)) and (math_expression[i+1] = '*') then
      begin
        if Length(number) <> 0 then PushNumber(number);
        while (stack.Count > 0) and (OPERATORS.ContainsKey(stack.Peek)) and (OPERATORS[stack.Peek].priority >= 3) do
          resultExpression.Enqueue(stack.Pop);
        stack.Push('**');
        Inc(i);
      end

      else if OPERATORS.ContainsKey(character) then
      begin
        if (i = 1) or (math_expression[i - 1] = '(') or OPERATORS.ContainsKey(math_expression[i - 1]) then
        begin
          stack.Push('neg');
        end
        else
        begin
          if Length(number) <> 0 then PushNumber(number);
          while (stack.Count > 0) and (OPERATORS.ContainsKey(stack.Peek)) and
                (OPERATORS[stack.Peek].priority >= OPERATORS[character].priority) do
            resultExpression.Enqueue(stack.Pop);
          stack.Push(character);
        end;
      end

      else if character = '(' then
      begin
        stack.Push('(');
      end
      else if character = ')' then
      begin
        if Length(number) <> 0 then PushNumber(number);
        while (stack.Count > 0) and (stack.Peek <> '(') do
          resultExpression.Enqueue(stack.Pop);
        if (stack.Count = 0) then
          raise Exception.Create('Unmatched closing parenthesis');
        stack.Pop;

        if (stack.Count > 0) and UNARY_OPERATORS.ContainsKey(stack.Peek) then
          resultExpression.Enqueue(stack.Pop);
      end;

      Inc(i);
    end;

  if Length(number) > 0 then
    PushNumber(number);

  while stack.Count > 0 do
    begin
      if stack.Peek = '(' then
        raise Exception.Create('Unmatched opening parenthesis');
      resultExpression.Enqueue(stack.Pop);
    end;
  Result := True;
  resultData := resultExpression;


  end;



function TMathExpressionCalc.CalculationBySymbol2D(
  translated_expression: TQueue<string>;
  variables: TDictionary<string, float64>;
  dependentVar: string;
  range_variable: string;
  out resultPoint: TGraphPoint
): Boolean;
  var
    stack: TStack<Float64>;
    token: string;
    value, a, b: Float64;
    funcBin: TFunction;
    funcUnary: TUnaryFunc;
    str : string;
  begin
    Result := False;
    stack := TStack<Float64>.Create;
      while translated_expression.Count > 0 do
      begin
        token := translated_expression.Peek;

        if TryStrToFloat(token, value) then
        begin
          stack.Push(value);
          translated_expression.Extract;
          token := '';
        end
        else if variables.ContainsKey(token) then
        begin
          stack.Push(variables[token]);
          translated_expression.Extract;
          token := '';
        end
        else if OPERATORS.ContainsKey(token) then
        begin
          if stack.Count < 2 then
            raise Exception.Create('Not enough operands for operator "' + token + '"');
          a := stack.Extract;
          b := stack.Extract;
          funcBin := OPERATORS[token].func;
          stack.Push(funcBin(b, a));
          translated_expression.Extract;
          token := '';
        end
        else if UNARY_OPERATORS.ContainsKey(token) then
        begin
          if stack.Count < 1 then
            raise Exception.Create('Not enough operand for function "' + token + '"');
          a := stack.Extract;
          funcUnary := UNARY_OPERATORS[token];
          stack.Push(funcUnary(a));
          translated_expression.Extract;
          token := '';
        end
        else
          raise Exception.Create('Unknown token: "' + token + '"');
      end;


      if stack.Count = 1 then
        begin
        if dependentVar = 'x' then
          begin
            resultPoint.x := stack.Extract;
            resultPoint.y := variables.Items['y'];
            resultPoint.z := variables.Items['z'];
            Result := True;
          end
        else if dependentVar = 'y' then
          begin
            resultPoint.y := stack.Extract;
            resultPoint.x := variables.Items['x'];
            resultPoint.z := variables.Items['z'];
            Result := True;
          end
        else if dependentVar = 'z' then
           begin
            resultPoint.y := variables.Items['y'];
            resultPoint.x := variables.Items['x'];
            resultPoint.z := stack.Extract;
            Result := True;
           end
        else
          raise Exception.Create('Unknown res_variable: "' + dependentVar + '"');
    end;
    stack.Free;
  end;



  function TMathExpressionCalc.CalcPointsByRange(rstart : integer; rend : integer;
   variables : TDictionary<string, float64>;
   dependentVar : string; range_variable : string;
    out points : TArray<TGraphPoint>; math_expression_copy : array of string) : Boolean;
  var
    I, I2 : Integer;
    point : TGraphPoint;
    Y : Float64;
    parsed_expression : TQueue<string>;
    elem : string;
    begin
    if (rstart < -200) then
      rstart := -200
    else if (rstart > 200) then
      rstart := 200;
    if (rend < -200) then
      rend := -200
    else if (rend > 200) then
      rend := 200;

    parsed_expression := TQueue<string>.Create;
    SetLength(points, Abs(rstart - rend)+1);
    if rstart < rend then
      begin
        for I := 0 to Abs(rstart - rend)-1 do
        begin
        for I2:=0 to Length(math_expression_copy)-1 do
          begin
            parsed_expression.Enqueue(math_expression_copy[I2]);
          end;
          variables.AddOrSetValue(range_variable, I+rstart);
          if CalculationBySymbol2D(parsed_expression, variables, dependentVar, range_variable, point) then
          begin
            points[I] := point;
          end;
        end;
        for I2:=0 to Length(math_expression_copy)-1 do
          begin
            parsed_expression.Enqueue(math_expression_copy[I2]);
          end;
          variables.AddOrSetValue(range_variable, I+rstart+1);
          if CalculationBySymbol2D(parsed_expression, variables, dependentVar, range_variable, point) then points[I] := point;
      end
      else raise Exception.Create('Ошибка диапазона');
    Result := True;
    end;




end.

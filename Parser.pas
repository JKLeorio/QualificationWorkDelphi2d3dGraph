unit Parser;

interface

uses
  Generics.Collections, MathUtils;

type
  TPoint2d = record
    x : Float64;
    y : Float64;
  end;

  TPoint3d = record
    x : Float64;
    y : Float64;
    z : Float64;
  end;


  TPoints2d = array of TPoint2d;

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
      function Parse(math_expression : string; out resultData : TQueue<string>) : Boolean;
      function CalculationBySymbol2D(translated_expression : TQueue<string>; variables : TDictionary<string, float64>; res_variable : string; out resultPoint : TPoint2d) : Boolean;
      function CalcPointsByRange(rstart : integer; rend : integer; variables : TDictionary<string, float64>; res_variable : string; parsed_expression_data : TQueue<string>; out points : TPoints2d) : Boolean;
      procedure SetVariables(variables : TDictionary<string, float64>);
  end;

  function SolveExpressionSymPy(const Equation, Variable: string): string;
  function OccurrencesOfChar(const S: string; const C: char): integer;

implementation
  uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes,
    System.Math,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls;

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
    variables := TDictionary<string, float64>.Create;
    variables.Add('x',0);
    variables.Add('y',0);
    variables.Add('z',0);
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
  end;

  destructor TMathExpressionCalc.Destroy;
  begin
    self.OPERATORS.Free;
    inherited Destroy;
  end;

  function TMathExpressionCalc.Parse(math_expression: string; out resultData : TQueue<string>) : Boolean;

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


  procedure PushNumber(var number : string);
    begin
      if Assigned(resultExpression) then
      begin
        resultExpression.Enqueue(number);
        number := '';
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

        // Пропуск пробелов
        if c = ' ' then
        begin
          Inc(i);
          Continue;
        end;

        // Скобки
        if c = '(' then
        begin
          Inc(openBrackets);
          isLastTokenOperator := True; // ожидаем операнд после (
        end
        else if c = ')' then
        begin
          Inc(closeBrackets);
          if isLastTokenOperator then
          begin
            Result := False;
            Exit; // не может быть ) сразу после оператора
          end;
        end

        // Операторы
        else if OPERATORS.ContainsKey(c) then
        begin
          if (prevToken = '') or (isLastTokenOperator) then
          begin
            Result := False;
            Exit; // два оператора подряд или начинается с оператора
          end;
          isLastTokenOperator := True;
        end

        // Число/переменная/функция
        else
        begin
          token := '';
          while (i <= Length(math_expression)) and
                (math_expression[i] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '_']) do
          begin
            token := token + math_expression[i];
            Inc(i);
          end;
          Dec(i); // компенсация инкремента

          if token <> '' then
          begin
            // Если это переменная, число или функция
            if not TryStrToFloat(token, buff) and
               not variables.ContainsKey(token) and
               not UNARY_OPERATORS.ContainsKey(token) then
            begin
              Result := False;
              Exit; // неизвестный токен
            end;

            isLastTokenOperator := False;
            prevToken := token;
          end;
        end;

        Inc(i);
      end;

      // Скобки должны быть сбалансированы
      if openBrackets <> closeBrackets then
      begin
        Result := False;
        Exit;
      end;

      // Последний токен не должен быть оператором
      if isLastTokenOperator then
      begin
        Result := False;
        Exit;
      end;

      Result := True;
    end;


  begin
  i := 1;
  while i <= Length(math_expression) do
    begin
      character := math_expression[i];

      // Функция
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
        end
        else
          raise Exception.Create('Unknown function or variable: ' + funcName);
        Dec(i);
      end

      // Число или переменная (цифры/точки)
      else if (character in ['0'..'9']) or (character = '.') then
      begin
        number := number + character;
      end

      // Поддержка оператора "**"
      else if (character = '*') and (i < Length(math_expression)) and (math_expression[i+1] = '*') then
      begin
        if Length(number) <> 0 then PushNumber(number);
        while (stack.Count > 0) and (OPERATORS.ContainsKey(stack.Peek)) and (OPERATORS[stack.Peek].priority >= 3) do
          resultExpression.Enqueue(stack.Pop);
        stack.Push('**');
        Inc(i); // пропускаем второй *
      end

      // Обычные операторы
      else if OPERATORS.ContainsKey(character) then
      begin
        if Length(number) <> 0 then PushNumber(number);
        while (stack.Count > 0) and (OPERATORS.ContainsKey(stack.Peek)) and (OPERATORS[stack.Peek].priority >= OPERATORS[character].priority) do
          resultExpression.Enqueue(stack.Pop);
        stack.Push(character);
      end

      // Скобки
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
        stack.Pop; // удаляем '('

        // после скобки может быть функция
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

  end;


//  function TMathExpressionCalc.Parse(math_expression: string; out resultData : TQueue<string>) : Boolean;
//  var
//    resultExpression : TQueue<string>;
//    stack : TStack<string>;
//    character : Char;
//    buff : Integer;
//    StackBuff : string;
//    number : string;
//    E : Exception;
//    a,b : Word;
//
//    procedure PushNumber(var number : string);
//    begin
//      if Assigned(resultExpression) then
//      begin
//        resultExpression.Enqueue(number);
//        number := '';
//      end;
//    end;
//
//    function IsValid(math_expression : string ) : Boolean;
//      var
//
//      operands_count, operator_count : Word;
//      character : Char;
//      buff : Integer;
//      strBuff : string;
//      I: Integer;
//      closeBracketCount, openBracketCount : Word;
//
//      begin
//      for I := 1 to Length(math_expression) do
//        begin
//          character := math_expression[I];
//          if OPERATORS.ContainsKey(character) then
//            begin
//              if Length(strBuff) > 0 then
//              begin
//                Inc(operands_count);
//                strBuff := '';
//              end;
//              Inc(operator_count);
//            end
//          else if character in ['(',')'] then
//          begin
//            if character = '(' then
//              Inc(openBracketCount)
//            else
//              Inc(closeBracketCount);
//
//            if Length(strBuff) > 0 then
//            begin
//              Inc(operands_count);
//              strBuff := '';
//            end;
//          end
//          else if (variables.ContainsKey(character)) or (TryDecimalStrToInt(character, buff)) then
//          begin
//            strBuff := strBuff + character;
//          end;
//        end;
//      if Length(strBuff) > 0 then
//        begin
//          Inc(operands_count);
//          strBuff := '';
//        end;
//
//      if (operands_count-operator_count = 1) and (openBracketCount = closeBracketCount) then
//          Result := True;
//      end;
//
//  begin
//    try
//      stack := TStack<string>.Create;
//      resultExpression := TQueue<string>.Create;
//      math_expression := StringReplace(math_expression, ' ', '', [rfReplaceAll]);
//
//      if not IsValid(math_expression) then exit;
//
//      for character in math_expression do
//      begin
//        if (variables.ContainsKey(character)) or (TryDecimalStrToInt(character, buff)) then
//        begin
//          number := number + character;
//        end
//        else if (character = '.') and (Length(number)>0) then
//        if (OccurrencesOfChar(number, '.') <> 0) then
//          raise Exception.Create('number with more than two floating points')
//        else
//          begin
//            number := number + character;
//          end
//        else if OPERATORS.ContainsKey(character) then
//        begin
//          if Length(number) <> 0 then PushNumber(number);
//          with OPERATORS do
//          begin
//            while (stack.Count <> 0) and (stack.Peek <> '(') and (Items[stack.Peek].priority >= Items[character].priority) do
//            begin
//              resultExpression.Enqueue(stack.Extract);
//            end;
//            stack.Push(character);
//          end;
//        end
//        else if character = '(' then
//        begin
//          if Length(number) <> 0 then PushNumber(number);
//          stack.Push(character);
//        end
//        else if character = ')' then
//        begin
//          if Length(number) <> 0 then PushNumber(number);
//
//          while (stack.Count <> 0) do
//          begin
//            StackBuff := stack.Extract;
//            if StackBuff = '(' then break;
//            resultExpression.Enqueue(StackBuff);
//          end;
//        end;
//      end;
//      if Length(number) <> 0 then PushNumber(number);
//
//      while stack.Count <> 0 do
//      begin
//        resultExpression.Enqueue(stack.Extract);
//      end;
//      resultExpression.TrimExcess;
//      resultData := resultExpression;
//      Result := True;
//
//    except
//    on E : Exception do
//      raise Exception.Create('error in parse  -' + E.Message);
//    end;
//  end;

//function TMathExpressionCalc.CalculationBySymbol2D(translated_expression: TQueue<string>; variables: TDictionary<string, float64>;  res_variable : string; out resultPoint : TPoint2d) : Boolean;
//  var
//    stack : TStack<Float64>;
//    buff : Float64;
//    number : string;
//    character : string;
//    E: Exception;
//    func : TFunction;
//    a,b : Float64;
//    c : Integer;
//  begin
//    stack := TStack<Float64>.Create;
//    while translated_expression.Count <> 0 do
//      begin
//        if (stack.Count > 1) and (OPERATORS.ContainsKey(translated_expression.Peek)) then
//        begin
//          func := OPERATORS.Items[translated_expression.Extract].func;
//          a := stack.Extract;
//          b := stack.Extract;
//          stack.Push(func(b, a));
//        end
//        else
//        begin
//          if TryStrToFloat(translated_expression.Peek, buff) then
//          begin
//          stack.Push(buff);
//          translated_expression.Extract;
//          end
//          else if variables.ContainsKey(translated_expression.Peek[1]) then
//          begin
//            if variables.ContainsKey(translated_expression.Peek) then
//            begin
//              stack.Push(variables.Items[translated_expression.Peek]);
//            end;
//
//            translated_expression.Extract;
//          end
//          else
//          begin
//            break;
//          end;
//        end;
//        if stack.Count = 1 then
//        begin
//        if res_variable = 'x' then
//          begin
//            resultPoint.x := stack.Extract;
//            resultPoint.y :=  variables.Items[res_variable];
//            Result := True;
//          end
//        else if res_variable = 'y' then
//          begin
//            resultPoint.x := stack.Extract;
//            resultPoint.y := variables.Items[res_variable];
//            Result := True;
//          end;
//        end;
//      end;
//  end;


function TMathExpressionCalc.CalculationBySymbol2D(
  translated_expression: TQueue<string>;
  variables: TDictionary<string, float64>;
  res_variable: string;
  out resultPoint: TPoint2d
): Boolean;
  var
    stack: TStack<Float64>;
    token: string;
    value, a, b: Float64;
    funcBin: TFunction;
    funcUnary: TUnaryFunc;
  begin
    Result := False;
    stack := TStack<Float64>.Create;
    try
      while translated_expression.Count > 0 do
      begin
        token := translated_expression.Extract;

        if TryStrToFloat(token, value) then
        begin
          stack.Push(value);
        end
        else if variables.ContainsKey(token) then
        begin
          stack.Push(variables[token]);
        end
        else if OPERATORS.ContainsKey(token) then
        begin
          if stack.Count < 2 then
            raise Exception.Create('Not enough operands for operator "' + token + '"');
          a := stack.Extract;
          b := stack.Extract;
          funcBin := OPERATORS[token].func;
          stack.Push(funcBin(b, a));
        end
        else if UNARY_OPERATORS.ContainsKey(token) then
        begin
          if stack.Count < 1 then
            raise Exception.Create('Not enough operand for function "' + token + '"');
          a := stack.Extract;
          funcUnary := UNARY_OPERATORS[token];
          stack.Push(funcUnary(a));
        end
        else
          raise Exception.Create('Unknown token: "' + token + '"');
      end;

      if stack.Count <> 1 then
        raise Exception.Create('Invalid expression, result stack size <> 1');

      if stack.Count = 1 then
        begin
        if res_variable = 'x' then
          begin
            resultPoint.x := stack.Extract;
            resultPoint.y :=  variables.Items[res_variable];
            Result := True;
          end
        else if res_variable = 'y' then
          begin
            resultPoint.x := stack.Extract;
            resultPoint.y := variables.Items[res_variable];
            Result := True;
          end
        else
          raise Exception.Create('Unknown res_variable: "' + res_variable + '"');
        end;
      finally
      stack.Free;
    end;
  end;



  function TMathExpressionCalc.CalcPointsByRange(rstart : integer; rend : integer; variables : TDictionary<string, float64>; res_variable : string; parsed_expression_data : TQueue<string>; out points : TPoints2d) : Boolean;
  var
    I : Integer;
    point : TPoint2d;
    Y : Float64;
    parsed_expression : TQueue<string>;
    elem : string;
  begin
  parsed_expression := parsed_expression_data;
  if parsed_expression.Count <> 0 then
    begin

    SetLength(points, Abs(rstart - rend));
    if rstart < rend then
      begin
        for I := 0 to Abs(rstart - rend)-1 do
        begin
          variables.AddOrSetValue(res_variable, I-rstart);
          if CalculationBySymbol2D(parsed_expression, variables, res_variable, point) then
          begin
            ShowMessage(point.x.ToString+point.y.ToString);
            points[I] := point;
          end;
        end;
      end
      else raise Exception.Create('0');
    end
    else raise Exception.Create('1');
  Result := True;
  end;




end.

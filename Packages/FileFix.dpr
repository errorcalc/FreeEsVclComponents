program FileFix;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes;

var
  Path: string;
  Lines: TStringList;
  Text, NewText: string;
  Number, I: Integer;
begin
  try
    if not FindCmdLineSwitch('path', Path) then
      halt(1);

    Lines := TStringList.Create();
    try
      Lines.LoadFromFile(Path);

      if FindCmdLineSwitch('delete-with', Text) then
      begin
        Text := Text.Replace('^^', '"');
        while True do
        begin
          Number := -1;
          for I := 0 to Lines.Count - 1 do
          begin
            if Lines[I].Contains(Text) then
            begin
              Number := I;
              break;
            end;
          end;

          if Number = -1 then
            break;
          Lines.Delete(Number);
        end;
      end;

      if FindCmdLineSwitch('replace-with', Text) then
      begin
        if not FindCmdLineSwitch('to', NewText) then
          halt(1);
        Text := Text.Replace('^^', '"');
        NewText := NewText.Replace('^^', '"');
        while True do
        begin
          Number := -1;
          for I := 0 to Lines.Count - 1 do
          begin
            if Lines[I].Contains(Text) then
            begin
              Number := I;
              break;
            end;
          end;

          if Number = -1 then
            break;

          Lines[Number] := Lines[Number].Substring(0, Lines[I].IndexOf(Text)) + NewText;
        end;
      end;

      if FindCmdLineSwitch('insert-after', Text) then
      begin
        if not FindCmdLineSwitch('line', NewText) then
          halt(1);
        Text := Text.Replace('^^', '"');
        NewText := NewText.Replace('^^', '"');
        Number := 0;
        while Number <= Lines.Count - 1 do
        begin
          if Lines[Number].Contains(Text) then
          begin
            Lines.Insert(Number + 1, Lines[Number].Substring(0, Lines[Number].IndexOf(Text)) + NewText);
            Number := Number + 1;
          end;
          Number := Number + 1;
        end;
      end;

      Lines.SaveToFile(Path);
    finally
      Lines.Free();
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

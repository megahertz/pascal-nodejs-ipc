program child;

{$mode objfpc}

uses
  {$ifdef Unix}
    cthreads,
  {$endif}
  nodeipc,
  SynCommons;

function JsonParser(Message: RawByteString): Variant;
begin
  Result := _Json(Message);
end;

type
  TApplication = class
  private
    FNodeIpc: TNodeIpcProtocol;
    procedure OnMessage(Message: Variant);
  public
    procedure Run;
  end;

{ TApplication }

procedure TApplication.OnMessage(Message: Variant);
begin
  WriteLn('child: new message from ', Message.process, ' process');
end;

procedure TApplication.Run;
begin
  FNodeIpc := TNodeIpcProtocol.Create;
  FNodeIpc.OnMessage := @OnMessage;
  FNodeIpc.JsonParser := @JsonParser;
  FNodeIpc.Send(_Obj(['process', 'Free Pascal']));

  WriteLn('Press any key to exit');
  ReadLn();
end;

var
  App: TApplication;
begin
  App := TApplication.Create;
  App.Run;
end.

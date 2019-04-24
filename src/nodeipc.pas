{ Free Pascal library for communicating with a parent Node.js process
  through IPC. Licensed under MIT.
  @link https://github.com/megahertz/pascal-nodejs-ipc 
  @version 1.0.0 }
unit nodeipc;

{$mode objfpc}

interface

uses
  classes,
  pipes,
  syncobjs,
  sysutils;

type
  TOnMessage = procedure(Message: Variant) of object;
  TOnReceiveData = procedure(Data: RawByteString) of object;
  TJsonParser = function(Message: RawByteString): Variant;

  TIpcReaderThread = class(TThread)
  private
    FBuffer: TBytes;
    FOnReceiveData: TOnReceiveData;
    FReadStream: TInputPipeStream;
    procedure AppendBufferFromStream(Size: LongWord);
    procedure ProcessBuffer;
    procedure ProcessData(DataStart: Integer; DataEnd: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(NodeIpcFd: LongInt; OnReceiveData: TOnReceiveData);
    destructor Destroy; override;
  end;

  TNodeIpcProtocol = class
  private
    FIsAttached: Boolean;
    FJsonParser: TJsonParser;
    FMessageBufferLock: TCriticalSection;
    FMessageBuffer: array of string;
    FOnMessage: TOnMessage;
    FReadThread: TIpcReaderThread;
    FWriteStream: TOutputPipeStream;
    procedure SetOnMessage(AOnMessage: TOnMessage);
  protected
    procedure OnMessageReceived(Message: RawByteString); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Send(Message: Variant); virtual;
    property IsAttached: Boolean read FIsAttached;
    property JsonParser: TJsonParser write FJsonParser;
    property OnMessage: TOnMessage write SetOnMessage;
  end;

implementation

uses
  variants;

{ TIpcReaderThread }

procedure TIpcReaderThread.AppendBufferFromStream(Size: LongWord);
var
  StreamBuffer: TBytes;
  PreviousSize: Integer;
begin
  SetLength(StreamBuffer, Size);
  FReadStream.Read(StreamBuffer[0], Size);

  PreviousSize := Length(FBuffer);
  SetLength(FBuffer, PreviousSize + Size);
  Move(StreamBuffer[0], FBuffer[PreviousSize], Size);
end;

procedure TIpcReaderThread.ProcessBuffer;
var
  DataStart: Integer = 0;
  DataEnd: Integer = -1;
  I: Integer;
  Size: Integer;
begin
  Size := Length(FBuffer);

  for I := 0 to Size - 1 do
  begin
    if FBuffer[I] = 10 then
    begin
      DataEnd := I;
      ProcessData(DataStart, DataEnd);
      DataStart := DataEnd + 1;
    end;
  end;

  if DataEnd = -1 then
    exit;

  if DataStart >= Size then
  begin
    SetLength(FBuffer, 0);
    exit;
  end;

  for I := 0 to Size - DataStart - 1 do
    FBuffer[I] := FBuffer[DataStart + I];

  SetLength(FBuffer, Size - DataStart);
end;

procedure TIpcReaderThread.ProcessData(DataStart: Integer; DataEnd: Integer);
var
  DataString: RawByteString;
begin
  SetString(DataString, PAnsiChar(@FBuffer[DataStart]), DataEnd - DataStart);
  FOnReceiveData(DataString);
end;

procedure TIpcReaderThread.Execute;
var
  IncomingSize: LongWord;
begin
  while not Terminated do
  begin
    IncomingSize := FReadStream.NumBytesAvailable;
    if IncomingSize > 0 then
      AppendBufferFromStream(IncomingSize);

    if Length(FBuffer) > 0 then
      ProcessBuffer();

    Sleep(10);
  end;
end;

constructor TIpcReaderThread.Create(NodeIpcFd: LongInt;
  OnReceiveData: TOnReceiveData);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FOnReceiveData := OnReceiveData;
  FReadStream := TInputPipeStream.Create(NodeIpcFd);
end;

destructor TIpcReaderThread.Destroy;
begin
  FreeAndNil(FReadStream);
  SetLength(FBuffer, 0);
  inherited;
end;

{ TNodeIpcProtocol }

procedure TNodeIpcProtocol.SetOnMessage(AOnMessage: TOnMessage);
var
  Message: string;
begin
  FOnMessage := AOnMessage;

  FMessageBufferLock.Enter;
  try
    if Length(FMessageBuffer) < 1 then
      exit;

    for Message in FMessageBuffer do
      AOnMessage(Message);

    SetLength(FMessageBuffer, 0);
  finally
    FMessageBufferLock.Leave;
  end;
end;

procedure TNodeIpcProtocol.OnMessageReceived(Message: RawByteString);
var
  BufferSize: Integer;
begin
  if Assigned(FOnMessage) then
  begin
    if Assigned(FJsonParser) then
      FOnMessage(FJsonParser(Message))
    else
      FOnMessage(Message);
    exit;
  end;

  FMessageBufferLock.Enter;
  try
    BufferSize := Length(FMessageBuffer);
    SetLength(FMessageBuffer, BufferSize + 1);
    FMessageBuffer[BufferSize] := Message;
  finally
    FMessageBufferLock.Leave;
  end;
end;

constructor TNodeIpcProtocol.Create;
var
  NodeIpcFdStr: string;
  NodeIpcFd: LongInt;
begin
  inherited;

  FIsAttached := False;
  FMessageBufferLock := TCriticalSection.Create;

  NodeIpcFdStr := GetEnvironmentVariable('NODE_CHANNEL_FD');
  if NodeIpcFdStr = '' then
    exit;

  try
    NodeIpcFd := StrToInt(NodeIpcFdStr);
  except
    exit;
  end;

  FReadThread := TIpcReaderThread.Create(NodeIpcFd, @OnMessageReceived);
  FWriteStream := TOutputPipeStream.Create(NodeIpcFd);

  FIsAttached := True;
end;

destructor TNodeIpcProtocol.Destroy;
begin
  FreeAndNil(FReadThread);
  FreeAndNil(FWriteStream);
  FreeAndNil(FMessageBufferLock);
  inherited;
end;

procedure TNodeIpcProtocol.Send(Message: Variant);
var
  Buffer: TBytes;
begin
  if not Assigned(FWriteStream) then
    exit;

  Buffer := TEncoding.UTF8.GetBytes(VarToStr(Message) + #10);
  FWriteStream.Write(Buffer[0], Length(Buffer));
end;

end.

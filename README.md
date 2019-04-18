# pascal-nodejs-ipc

Free Pascal library for communicating with a parent Node.js process through
Inter Process Communications (IPC). 

Tested with FPC v3.0.4, v3.1.1, node v11.9.0 on Linux x64

[Example project](example)

## Usage

**parent.js**
```js
const { spawn } = require('child_process');

const proc = spawn('./dist/child', {
  stdio: ['inherit', 'inherit', 'inherit', 'ipc']
});

// Using built-in Node.js IPC
proc.on('message', (msg) => { console.log('parent, new message:', msg) });
proc.send({ source: 'Node.js' });
```

**child.pas**
```pascal
program child;

uses
  {$IFDEF Unix}cthreads,{$ENDIF} nodeipc;

type
  TApplication = class
  private
    FNodeIpc: TNodeIpcProtocol;
    procedure OnMessage(Message: Variant);
  public
    procedure Run;
  end;

procedure TApplication.OnMessage(Message: Variant);
begin
  WriteLn('child, new message: ', Message);
end;

procedure TApplication.Run;
begin
  FNodeIpc := TNodeIpcProtocol.Create;
  FNodeIpc.OnMessage := @OnMessage;
  FNodeIpc.Send('{"source":"Free Pascal"}');

  WriteLn('Press any key to exit');
  ReadLn();
end;

var
  App: TApplication;
begin
  App := TApplication.Create;
  App.Run;
end.

```

## How Node.js built-in IPC module works

- Node.js passes a bi-directional IPC stream to a child process as a file
descriptor.
```js
const proc = spawn('./dist/child', {
  stdio: ['inherit', 'inherit', 'inherit', 'ipc']
});
```

- To send a message trough this stream, each process can send JSON encoded
  strings separated by a new line char.

- To get the exact file descriptor, a child process reads NODE_CHANNEL_FD
  environment variable.

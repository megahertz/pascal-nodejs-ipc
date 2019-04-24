'use strict';

const { spawn } = require('child_process');

const proc = spawn('./dist/child', {
  stdio: ['pipe', 'pipe', 'pipe', 'ipc']
});

proc
  .on('message', (msg) => {
    console.log(`parent: new message from ${msg.process} process`);
  })

  .on('exit', () => {
    console.log('parent: sample process is finished');
  })

  .send({ process: 'Node.js' });
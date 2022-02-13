## erlang node monitor

Visualize registered processes in an erlang node. This program takes a snapshot of the registered processes and its links and visualizes them as a network of nodes.

![UI Preview](https://github.com/howerest/erlang-node-monitor/raw/master/docs/erlang-node-monitor__screenshot.png)

### Start now

1. Build and Start: `make run` (from root folder)
2. Once server is running, open file `priv/ui/build/index.html` with a browser.

### Add to your application

1. Call `erlang_node_monitor_app:start()` where you want to start monitoring your node. When the application starts, a WebSocket server will be initialised in `ws://localhost:5000`, which is going to be used to communicate with the client.
2. Open `file priv/ui/build/index.html` with a browser. When the page is loaded a fresh snapshot is going to be visualized.

#### Features
```
[x] Take a snapshot of processes and links when loading the page
[x] Display .*_sup named processes as red hexagon (supervisors)
[x] Display processes as green dots
[x] Display process names below each node
[x] Display process pid when hovering a processing for 400 ms
[x] Display linked processes
[x] Be able to trace messages within the erlang node over time (as json events towards clients)
```
#### Next features
```
[ ] Register new spawned processes in real time
[ ] Remove destroyed processes in real time
[ ] Filter traced messages by processes filters
[ ] Filter displayed processes by processes filters
```
##### Next features
```
[ ] Render ports and with different icon
[ ] Render port node details
[ ] Draw dotted line between processes that have communicated
[ ] Make dotted line disappear in time after last exchanged message
[ ] Render animation on failed process
```
##### Authors

- David Valin: `davidvalin@howerest.com`

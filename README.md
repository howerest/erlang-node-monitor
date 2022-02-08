## erlang node monitor

Visualize registered processes in an erlang node. This program takes a snapshot of the registered processes and its links and visualizes them as a network of nodes.

![UI Preview](https://github.com/howerest/erlang-node-monitor/raw/master/docs/screenshot.png)

### Start now

1. Build Backend: `make` (from root folder)
2. Start Backend: `make run` (from root folder)
3. Build UI: `cd priv/ui; npm install; npm run build` (from root folder)
3. Once server is running, open file `priv/ui/dist/index.html` with a browser

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
```
#### Next features
```
[ ] Filter by list of process names or process ids
[ ] Register new spawned processes in real time
[ ] Remove destroyed processes in real time
[ ] Draw dotted line between processes that have communicated
[ ] Render ports and with different icon
[ ] Render port node details
```
##### Next features
```
[ ] Make dotted line disappear in time after last exchanged message
[ ] Render animation on failed process
[ ] Be able to trace messages within the erlang node over time (as json)
```
##### Authors

- David Valin: `davidvalin@howerest.com`

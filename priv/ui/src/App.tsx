import React, {useEffect, useState, useRef} from 'react';
import {MainBar, Network, NodeDetails, Messages} from "./components"
import {DataSet as VisDataSet} from "vis-data";
import {IEdge} from "./state/initial_state";
import './App.css';
import { useSelector, useDispatch } from 'react-redux';
import { IAppState } from './state/initial_state';

export interface ISourceNode {
  id: Array<string>;
  name: string;
  links: Array<Array<string>>;
}

export interface ISourceEdge {
  id: Array<string>;
  from: string;
  to: string;
}

function App() {
  const state = useSelector((state:IAppState) => state);
  const ws = React.useRef(null as any);
  const dispatch = useDispatch();

  useEffect(() => {
    ws.current = new WebSocket('ws://localhost:5000/');
    ws.current.onopen = () => {
      ws.current.send(JSON.stringify({ msg: 'greet' }));
    };
    ws.current.onmessage = handleOnMessage;
    ws.current.onerror = (evt:any) => {
      console.log(evt);
    };

    return () => {
      ws.current.close();
      ws.current = null;
    }
  }, []);

  /*
   * Adds a new process node to the DataSet, which
   * updates the Network
   */
  function addNewProcess(node:ISourceNode) {
    try {
      const id = node.id.join('.');
      const isSupervisor = node.name.indexOf("_sup") !== -1;

      state.nodes.add({
        id,
        label:         node.name ? node.name : id,
        title:         `pid: ${id}`,
        shape:         isSupervisor ? "hexagon" : "dot",
        color: {
          color:       isSupervisor ? "white" : "black",
          background:  isSupervisor ? "red" : "green",
        },
        links:         node.links.map((l:Array<string>) => l.join('.'))
      });

      // add an edge for each node link
      for (let i=0; i < node.links.length; i++) {
        const edgesDataSet = state.edges.getDataSet();

        if (edgesDataSet.get({
          filter: (edge:any) =>
            edge.from === node.links[i].join('.') &&
            edge.to === id
          }).length === 0) {

          const newEdge:IEdge = {
            id: edgesDataSet.length+1,
            from: id,
            to: node.links[i].join('.'),
          };
          state.edges.add(newEdge);
        }
      }
    } catch (e) {
      console.log('err: ', e);
    }
  }

  /**
   * Handles a new received message from erlang node websocket server
   */
  function handleOnMessage(evt:any) {
    try {
      const payload = JSON.parse(evt.data);
      switch(payload.type) {
        case "greet_back":
          dispatch({
            type: "SET_ERLANG_NODE_NAME",
            payload: payload.node
          });
          for (let i = 0; i < payload.processes.length; i++) {
            addNewProcess(payload.processes[i]);
          }
          break;
        case "msg_received":
          dispatch({
            type: "ADD_MESSAGE_RECEIVED",
            payload: {
              at: payload.at.join("."),
              datetime: payload.datetime,
              message: payload.msg
            }
          });
          break;
        case "msg_sent":
          dispatch({
            type: "ADD_MESSAGE_SENT",
            payload: {
              from: payload.from.join("."),
              datetime: payload.datetime,
              message: payload.msg
            }
          });
          break;
      }
    } catch(e) {
      console.log('err', e);
    }
  }

  return (
    <div className="App">
      <MainBar name={state.name} />
      <div className="App__content">
        <Network />
        <NodeDetails />
        <Messages />
      </div>
    </div>
  );
}

export default App;

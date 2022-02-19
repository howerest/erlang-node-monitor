import React, {useEffect, useState, useRef} from 'react';
import {MainBar, Network, NodeDetails, Messages} from "./components"
import {DataSet as VisDataSet} from "vis-data";
import {IEdge} from "./state/initial_state";
import {addNewProcess} from "./state/reducers";
import './App.css';
import { useSelector, useDispatch } from 'react-redux';
import { IAppState } from './state/initial_state';
import useWebSocket, { ReadyState } from 'react-use-websocket';

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
  const { sendMessage, readyState } =
    useWebSocket('ws://localhost:5000/', {
      onOpen: () => {
        dispatch({ type: "SET_CONNECTED" });
        sendMessage(JSON.stringify({ msg: 'greet' }));
      },
      onMessage: handleOnMessage,
      onClose: () => {
        dispatch({ type: "SET_DISCONNECTED" });
      },
      onError: (evt:any) => {
        console.log('error: ', evt);
      }
    });

  useEffect(() => {
    // Store filters in localStorage
    localStorage.setItem(
      'enm_f_messages',
      JSON.stringify(state.filters.messages)
    );
    if (readyState === ReadyState.CONNECTING) {
      sendMessage(JSON.stringify({
        msg: 'subscribe',
        data: state.filters.messages.receivedAtAnyOfProcessIds.filter((i:string) => i !== '')
      }));
    }
  }, [state.filters])

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
            addNewProcess(state, payload.processes[i]);
          }
          break;

        case "new_process":
          dispatch({
            type: "NEW_PROCESS",
            payload: {
              parent: payload.parent,
              child: payload.child,
              datetime: payload.datetime
            }
          });
          break;

        case "end_process":
          dispatch({
            type: "END_PROCESS",
            payload: {
              pid: payload.pid,
              terminationReason: payload.termination_reason,
              datetime: payload.datetime
            }
          });
          break;

        case "msg_received":
          dispatch({
            type: "ADD_MESSAGE_RECEIVED",
            payload: {
              at: payload.at,
              datetime: payload.datetime,
              message: payload.msg
            }
          });
          break;
        case "msg_sent":
          dispatch({
            type: "ADD_MESSAGE_SENT",
            payload: {
              from: payload.from,
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

  const connectionStatus = {
    [ReadyState.CONNECTING]:      'Connecting...',
    [ReadyState.OPEN]:            'Connected!',
    [ReadyState.CLOSING]:         'Disconecting...',
    [ReadyState.CLOSED]:          'Disconnected',
    [ReadyState.UNINSTANTIATED]:  '-'
  }[readyState];

  return (
    <div className="App">
      <MainBar />
      <div className="App__content">
        <Network />
        <NodeDetails />
        <Messages />
      </div>
    </div>
  );
}

export default App;

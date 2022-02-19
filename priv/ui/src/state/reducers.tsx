import initialState, { IAppState, IMessageSent, IMessageReceived } from './initial_state';
import {IEdge} from "./initial_state";
import {
  SET_CONNECTED,
  SET_DISCONNECTED,
  SET_ERLANG_NODE_NAME,
  NEW_PROCESS,
  END_PROCESS,
  TOGGLE_MESSAGES,
  SET_NODE_CONTENT,
  SET_TRACED_PROCESSES,
  ADD_MESSAGE_SENT,
  ADD_MESSAGE_RECEIVED
} from './actions';

export interface IAction<T> {
  payload: T
}

export interface ISourceNode {
  id: string;
  name: string;
  links: Array<string>;
}

/**
 * Reduces the state
 * @param state The original state
 * @param action The action to perform in the state
 */
export function appReducer(state:IAppState = initialState, action:any) {
  switch(action.type) {
    case SET_CONNECTED:
      return setConnectedReducer(state);
    case SET_DISCONNECTED:
      return setDisconnectedReducer(state);
    case SET_ERLANG_NODE_NAME:
      return setErlangNodeNameReducer(state, action);
    case NEW_PROCESS:
      return setNewProcessReducer(state, action);
    case END_PROCESS:
      return setEndProcessReducer(state, action);
    case TOGGLE_MESSAGES:
      return setToggleMessagesReducer(state, action);
    case SET_NODE_CONTENT:
      return setNodeContent(state, action);
    case SET_TRACED_PROCESSES:
      return setTracedProcesses(state, action);
    case ADD_MESSAGE_SENT:
      return addMessageSent(state, action);
    case ADD_MESSAGE_RECEIVED:
      return addMessageReceived(state, action);
  }
  return state;
};

// SET_CONNECTED
function setConnectedReducer(state:IAppState): IAppState {
  return {...state, connected: true };
}

// SET_DISCONNECTED
function setDisconnectedReducer(state:IAppState): IAppState {
  return {...state, connected: false };
}

// SET_ERLANG_NODE_NAME
function setErlangNodeNameReducer(state:IAppState, action:IAction<string>): IAppState {
  return {...state, ...{name: action.payload}};
};

// NEW_PROCESS
interface INewProcess {
  parent: string;
  child: string;
  datetime: string;
}

function setNewProcessReducer(state:IAppState, action:IAction<INewProcess>): IAppState {
  const id = action.payload.child;
  state.nodes.add({
    id,
    label:         id,
    title:         `pid: ${id}`,
    shape:         "dot",
    color: {
      color:       "black",
      background:  "green",
    },
    links:         [],
    parent:        action.payload.parent
  });
  return state;
};

// END_PROCESS
interface IEndedProcess {
  pid: string;
  terminationReason: string;
  datetime: string;
}

function setEndProcessReducer(state:IAppState, action:IAction<IEndedProcess>): IAppState {
  state.nodes.remove(action.payload.pid);
  return state;
};

// TOGGLE_MESSAGES
function setToggleMessagesReducer(state:IAppState, action:IAction<string>): IAppState {
  const newValue = !state.config.filters;
  localStorage.setItem("enm_config", newValue ? '1' : '0');
  return {
    ...state,
    ...{ config: {
      ...state.config,
      filters: newValue
    }}
  };
};

// SET_NODE_CONTENT
function setNodeContent(state:IAppState, action:IAction<string>): IAppState {
  return {...state, ...{nodeContent: action.payload}};
};

// SET_TRACED_PROCESSES
function setTracedProcesses(state:IAppState, action:IAction<Array<string>>) : IAppState {
  // TODO: avoid duplicates
  return {
    ...state,
    ...{
      filters: {
        ...state.filters,
        messages: {
          ...state.filters.messages,
          sentFromAnyOfProcessIds: action.payload,
          receivedAtAnyOfProcessIds: action.payload
        }
      }
    }
  };
}

// ADD_MESSAGE_SENT
function addMessageSent(state:IAppState, action:IAction<IMessageSent>): IAppState {
  const newMessages = {...state.messagesSent};
  if (!newMessages[action.payload.from]) {
    newMessages [action.payload.from] = {};
  }
  if (!newMessages[action.payload.from][action.payload.datetime]) {
    newMessages
      [action.payload.from]
      [action.payload.datetime] = [];
  }
  newMessages[action.payload.from][action.payload.datetime] = [
    action.payload.message,
    ...newMessages[action.payload.from][action.payload.datetime].slice(100)
  ];

  return {
    ...state,
    messagesSent: newMessages,
    messagesSentCount: state.messagesSentCount+1,
    lastMessageAtNode: action.payload.from
  }
}

// ADD_MESSAGE_RECEIVED
function addMessageReceived(state:IAppState, action:IAction<IMessageReceived>): IAppState {
  const newMessages = {...state.messagesReceived};
  if (!newMessages[action.payload.at]) {
    newMessages [action.payload.at] = {};
  }
  if (!newMessages[action.payload.at][action.payload.datetime]) {
    newMessages
      [action.payload.at]
      [action.payload.datetime] = [];
  }
  newMessages[action.payload.at][action.payload.datetime] = [
    action.payload.message,
    ...newMessages[action.payload.at][action.payload.datetime].slice(100)
  ];

  return {
    ...state,
    messagesReceived: newMessages,
    messagesReceivedCount: state.messagesReceivedCount+1,
    lastMessageAtNode: action.payload.at
  }
}

/*
 * Adds a new process node to the DataSet, which
 * updates the Network
 */
export function addNewProcess(state:IAppState, node:ISourceNode) {
  try {
    const id = node.id;
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
      links:         node.links.map((l:string) => l),
      parent: ""
    });

    // add an edge for each node link
    for (let i=0; i < node.links.length; i++) {
      const edgesDataSet = state.edges.getDataSet();

      if (edgesDataSet.get({
        filter: (edge:any) =>
          edge.from === node.links[i] &&
          edge.to === id
        }).length === 0) {

        const newEdge:IEdge = {
          id: edgesDataSet.length+1,
          from: id,
          to: node.links[i],
        };
        state.edges.add(newEdge);
      }
    }
  } catch (e) {
    console.log('err: ', e);
  }
}

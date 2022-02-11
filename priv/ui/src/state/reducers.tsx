import {DataSet as VisDataSet} from "vis-data";
import initialState, { IAppState, INode } from './initial_state';
import {
  SET_ERLANG_NODE_NAME,
  SET_NODE_CONTENT,
  ADD_MESSAGE
} from './actions';

interface IAction<T> {
  payload: T
}

/**
 * Reduces the state
 * @param state The original state
 * @param action The action to perform in the state
 */
export function appReducer(state:IAppState = initialState, action:any) {
  switch(action.type) {
    case SET_ERLANG_NODE_NAME:
      return setErlangNodeNameReducer(state, action);
    case SET_NODE_CONTENT:
      return setNodeContent(state, action);
    case ADD_MESSAGE:
      return addMessage(state, action);
  }
  return state;
};

// SET_ERLANG_NODE_NAME
function setErlangNodeNameReducer(state:IAppState, action:IAction<string>): IAppState {
  return {...state, ...{name: action.payload}};
};

// SET_NODE_CONTENT
function setNodeContent(state:IAppState, action:IAction<string>): IAppState {
  return {...state, ...{nodeContent: action.payload}};
};

// ADD_MESSAGE
function addMessage(state:IAppState, action:IAction<INewMessage>): IAppState {
  const process:INode = state.nodes.get(action.payload.sourceProcessId) as INode;
  const newMessages = {...process.messages};
  if (!newMessages[action.payload.sourceProcessId]) {
    newMessages[action.payload.sourceProcessId] = {};
  }
  if (!newMessages[action.payload.sourceProcessId][action.payload.datetime]) {
    newMessages[action.payload.sourceProcessId][action.payload.datetime] = [];
  }
  newMessages[action.payload.sourceProcessId][action.payload.datetime].push({
    fromProcessId: action.payload.sourceProcessId,
    content: action.payload.message
  });
  state.nodes.updateOnly({
    ...process,
    id: action.payload.sourceProcessId,
    messages: newMessages
  } as INode);
  return {
    ...state,
    lastMessageAtNode: action.payload.sourceProcessId
  }
}
interface INewMessage {
  sourceProcessId: string;
  datetime: string;
  message: string;
}

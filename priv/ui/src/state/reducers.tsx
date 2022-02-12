import initialState, { IAppState, IMessageSent, IMessageReceived } from './initial_state';
import {
  SET_ERLANG_NODE_NAME,
  SET_NODE_CONTENT,
  ADD_MESSAGE_SENT,
  ADD_MESSAGE_RECEIVED
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
    case ADD_MESSAGE_SENT:
      return addMessageSent(state, action);
    case ADD_MESSAGE_RECEIVED:
      return addMessageReceived(state, action);
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
  newMessages[action.payload.from][action.payload.datetime].push(
    action.payload.message
  );

  return {
    ...state,
    messagesSent: newMessages,
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
  newMessages[action.payload.at][action.payload.datetime].push(
    action.payload.message
  );

  return {
    ...state,
    messagesReceived: newMessages,
    lastMessageAtNode: action.payload.at
  }
}

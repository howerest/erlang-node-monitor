import {DataSet as VisDataSet} from "vis-data";
import initialState, { IAppState } from './initial_state';
import {
  SET_ERLANG_NODE_NAME,
  SET_NODE_CONTENT
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

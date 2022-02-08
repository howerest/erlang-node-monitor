/**
 * Actions
 */

export const SET_ERLANG_NODE_NAME = 'SET_ERLANG_NODE_NAME';
export const SET_NODE_CONTENT = 'SET_NODE_CONTENT';


export function setErlangNodeName(payload:any) {
  return { type: SET_ERLANG_NODE_NAME, payload };
}
export function setNodeContent(payload:any) {
  return { type: SET_NODE_CONTENT, payload };
}

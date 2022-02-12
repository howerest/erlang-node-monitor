import {DataSet as VisDataSet} from "vis-data";

export interface IMessageSent {
  from: string;
  datetime: string;
  message: string;
}

export interface IMessageReceived {
  at: string;
  datetime: string;
  message: string;
}

export interface IMessagesHistory {
  // from/to process id      // date            // messages
  [key: string]:            {[key: string]:     Array<string>};
}

// NOTE: IMessagesHistory sample
// const sampleMessageHistory:IMessagesHistory = {
//   "0,10,0": {
//     "0.12.0": {
//       "2022-10-05 14:30": [],
//       "2022-10-05 14:31": []
//     }
//   }
// }

export interface INode {
  id: string;
  label: string;
  title: string;
  shape: "hexagon"|"dot";
  links: Array<string>;
  color?: any;
}

export interface IEdge {
  id: number;
  from: string;
  to: string;
}

export interface IAppState {
  name: string;
  nodes: VisDataSet<INode>;
  edges: VisDataSet<IEdge>;
  messagesSent: IMessagesHistory;
  messagesReceived: IMessagesHistory;
  nodeContent: string;
  lastMessageAtNode: null|string;
}

const initialState:IAppState = {
  name: 'NOT_CONNECTED!',
  nodes: new VisDataSet(),
  edges: new VisDataSet(),
  messagesSent: {},
  messagesReceived: {},
  nodeContent: '',
  lastMessageAtNode: null
};

export default initialState;

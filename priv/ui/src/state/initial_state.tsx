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

// {
//   "0,10,0": {
//     "2022-10-05 14:30": [
//       "first message",
//       "second message"
//     ],
//     "2022-10-05 14:31": [
//       "third message"
//     ]
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

interface IMessageFilters {
  sentFromAnyOfProcessIds: null|Array<string>;
  receivedAtAnyOfProcessIds: null|Array<string>;
}

export interface IAppState {
  name: string;
  nodes: VisDataSet<INode>;
  edges: VisDataSet<IEdge>;
  filters: {
    processes: {},
    messages: IMessageFilters
  },
  messagesSent: IMessagesHistory;
  messagesReceived: IMessagesHistory;
  nodeContent: string;
  lastMessageAtNode: null|string;
}

const initialState:IAppState = {
  name: 'NOT_CONNECTED!',
  nodes: new VisDataSet(),
  edges: new VisDataSet(),
  filters: {
    processes: {},
    messages: {
      sentFromAnyOfProcessIds: null,
      receivedAtAnyOfProcessIds: null
    }
  },
  messagesSent: {},
  messagesReceived: {},
  nodeContent: '',
  lastMessageAtNode: null
};

export default initialState;

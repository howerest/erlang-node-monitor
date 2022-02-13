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
  config: {
    filters: boolean;
    messages: boolean;
    selectedNodeInfo: boolean;
  },
  nodes: VisDataSet<INode>;
  edges: VisDataSet<IEdge>;
  filters: {
    processes: {},
    messages: IMessageFilters
  },
  messagesSent: IMessagesHistory;
  messagesSentCount: number;
  messagesReceived: IMessagesHistory;
  messagesReceivedCount: number;
  nodeContent: string;
  lastMessageAtNode: null|string;
}

const initialState:IAppState = {
  name: 'NOT_CONNECTED!',
  config: {
    filters: localStorage.getItem("enm_config") === '1' ? true : false,
    messages: false,
    selectedNodeInfo: true
  },
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
  messagesSentCount: 0,
  messagesReceived: {},
  messagesReceivedCount: 0,
  nodeContent: '',
  lastMessageAtNode: null
};

export default initialState;

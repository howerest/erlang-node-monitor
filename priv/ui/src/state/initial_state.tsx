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

export interface IFilteredProcesses {
  [key: string]: {
    messagesSent: boolean;
    messagesReceived: boolean;
  }
}

export interface INode {
  id: string;
  parent: string;
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
  sentFromAnyOfProcessIds: Array<string>;
  receivedAtAnyOfProcessIds: Array<string>;
}

export interface IAppState {
  connected: boolean;
  name: string;
  config: {
    filters: boolean;
    messages: boolean;
    selectedNodeInfo: boolean;
  },
  nodes: VisDataSet<INode>;
  edges: VisDataSet<IEdge>;
  filters: {
    processes: IFilteredProcesses;
    messages: IMessageFilters;
  },
  messagesSent: IMessagesHistory;
  messagesSentCount: number;
  messagesReceived: IMessagesHistory;
  messagesReceivedCount: number;
  nodeContent: string;
  lastMessageAtNode: null|string;
}

const existentFiltersMessages = localStorage.getItem('enm_f_messages');

const initialState:IAppState = {
  connected: false,
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
    messages: existentFiltersMessages ? JSON.parse(existentFiltersMessages) : {
      sentFromAnyOfProcessIds: [],
      receivedAtAnyOfProcessIds: []
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

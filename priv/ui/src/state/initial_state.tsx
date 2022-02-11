import {DataSet as VisDataSet} from "vis-data";

export interface IMessage {
  fromProcessId: string;
  content: string;
}

export interface IMessagesHistory {
  // source process id      // date            // messages
  [key: string]:            {[key: string]:    Array<IMessage>};
}

export interface INode {
  id: string;
  label: string;
  title: string;
  shape: "hexagon"|"dot";
  links: Array<string>;
  color?: any;
  messages: IMessagesHistory;
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
  nodeContent: string;
  lastMessageAtNode: null|string;
}

const initialState:IAppState = {
  name: 'NOT_CONNECTED!',
  nodes: new VisDataSet(),
  edges: new VisDataSet(),
  nodeContent: '',
  lastMessageAtNode: null
};

export default initialState;

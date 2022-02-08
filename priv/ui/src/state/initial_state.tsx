import {DataSet as VisDataSet} from "vis-data";

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
  nodeContent: string;
}

const initialState:IAppState = {
  name: 'NOT_CONNECTED!',
  nodes: new VisDataSet(),
  edges: new VisDataSet(),
  nodeContent: ''
};

export default initialState;

import react, {useRef, useEffect} from "react"
import {Network as VisNetwork} from "vis-network";
import {opts} from "./options";
import "./index.css";
import { useSelector } from 'react-redux';
import { IAppState } from '../../state/initial_state';

interface IProps {
  handleSelect: Function;
}

export default function(props: IProps) {
  const state = useSelector((state:IAppState) => state);
  const domNode = useRef(null);
  const network = useRef(null);

  useEffect(() => {
    network.current !== null && (network.current as any).redraw();
  }, [state.lastMessageAtNode])

  useEffect(() => {
    if (network.current === null) {
      network.current = new VisNetwork(
        domNode.current as any,
        {
          nodes: state.nodes,
          edges: state.edges
        },
        opts as any
      ) as any;

      (network.current as any).on("click", props.handleSelect);
    }

    return () => {
      (network.current as any).off("click", props.handleSelect);
      network.current = null;
    }
  }, [
      domNode,
      network,
      state.nodes,
      state.edges
  ]);

  return (
    <div className="Network">
      <div ref={domNode}></div>
    </div>
  );
}

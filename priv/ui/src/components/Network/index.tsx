import react, {useRef, useEffect} from "react"
import {Network as VisNetwork} from "vis-network";
import {opts} from "./options";
import "./index.css";
import { useSelector, useDispatch } from 'react-redux';
import { IAppState } from '../../state/initial_state';

export default function(props:{}) {
  const state = useSelector((state:IAppState) => state);
  const domNode = useRef(null);
  const network = useRef(null);
  const dispatch = useDispatch();

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

      (network.current as any).on("click", handleSelect);
    }

    return () => {
      (network.current as any).off("click", handleSelect);
      network.current = null;
    }
  }, [
      domNode,
      network,
      state.nodes,
      state.edges
  ]);

  function handleSelect(params:any) {
    if (params.nodes.length > 0) {
      const nodeData = state.nodes.get(params.nodes[0]);
      dispatch({
        type: "SET_NODE_CONTENT",
        payload: JSON.stringify(nodeData, undefined, 3)
      });
    }
  }

  return (
    <div className="Network">
      <div ref={domNode}></div>
    </div>
  );
}

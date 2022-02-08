import React from "react";
import "./index.css";
import { useSelector } from 'react-redux';
import { IAppState } from '../../state/initial_state';

export default function() {
  const state = useSelector((state:IAppState) => state);

  return (
    <div className="NodeDetails">
      <h4>Selected node:</h4>
      <pre>{state.nodeContent}</pre>
    </div>
  );
}

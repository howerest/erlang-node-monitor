import React from "react";
import "./index.css";
import { useSelector, useDispatch } from 'react-redux';
import { IAppState } from '../../state/initial_state';

export default function({}) {
  const state = useSelector((state:IAppState) => state);
  const dispatch = useDispatch();

  function handleChange(e:any) {
    dispatch({
      type: "SET_TRACED_PROCESSES",
      payload: e.target.value
        .replace(/[^a-z0-9., ]+/gi, "")
        .replace("  ", " ")
        .replace(",,", ",")
        .replace("..", ".")
        .toLowerCase()
        .split(',')
    });
  }

  return (
    <div className="MainBar">
      <div>
        <h2>erlang node monitor</h2>
        <div>{state.name} - <span>{state.connected ? 'Connected' : 'Disconnected'}</span></div>
      </div>
      <div className="MainBar__filters">
        <label>
          Traced processes<br />
          <input
            type="text"
            onChange={handleChange}
            value={state.filters.messages.receivedAtAnyOfProcessIds}
          />
        </label>
      </div>
    </div>
  );
}

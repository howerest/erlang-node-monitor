import React from "react";
import "./index.css";
import { useSelector } from 'react-redux';
import { IAppState } from '../../state/initial_state';

interface IProps {
  name: string;
}

export default function(props: IProps) {
  const state = useSelector((state:IAppState) => state);

  return (
    <div className="MainBar">
      <h2>erlang node monitor</h2>
      <div>{state.name}</div>
    </div>
  );
}

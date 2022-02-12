import React from "react";
import "./index.css";
import { useSelector } from 'react-redux';
import { IAppState, IMessageSent, IMessageReceived } from '../../state/initial_state';

export default function(props:{}) {
  const state = useSelector((state:IAppState) => state);

  function getFilteredMessagesSent() {
    // TODO: filter messages on state.filters.messages
    let filteredMessages:Array<IMessageSent> = [];
    Object.keys(state.messagesSent).map((from) => {
      Object.keys(state.messagesSent[from]).map((datetime) => {
        state.messagesSent[from][datetime].map((message) => {
          filteredMessages = [{
            from,
            datetime,
            message
          }, ...filteredMessages];
        });
      });
    });
    return filteredMessages;
  }

  function getFilteredMessagesReceived() {
    // TODO: filter messages on state.filters.messages
    let filteredMessages:Array<IMessageReceived> = [];
    Object.keys(state.messagesReceived).map((at) => {
      Object.keys(state.messagesReceived[at]).map((datetime) => {
        state.messagesReceived[at][datetime].map((message) => {
          filteredMessages = [{
            at,
            datetime,
            message
          }, ...filteredMessages];
        });
      });
    });
    return filteredMessages;
  }


  return (
    <div className="Messages">
      <ul>
        <li className="Messages__group">
          <div className="Messages__group__name">Sent</div>
          <ul>
            {getFilteredMessagesSent().map((message, i) => (
              <li className="Messages__message" key={i}>
                <div className="Messages__message__description">
                  sent from <span>{message.from}</span> at <span>{message.datetime}</span>:
                </div>
                <pre>
                  {message.message}
                </pre>
              </li>
            ))}
          </ul>
        </li>
        <li className="Messages__group">
          <div className="Messages__group__name">Received</div>
          <ul>
            {getFilteredMessagesReceived().map((message, i) => (
              <li className="Messages__message" key={i}>
                <div className="Messages__message__description">
                  received at <span>{message.at}</span> at <span>{message.datetime}</span>:
                </div>
                <pre>
                  {message.message}
                </pre>
              </li>
            ))}
          </ul>
        </li>
      </ul>
    </div>
  )
}

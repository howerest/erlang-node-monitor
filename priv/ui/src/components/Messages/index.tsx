import React from "react";
import {
  CSSTransition,
  TransitionGroup,
} from 'react-transition-group';
import "./index.css";
import { useSelector, useDispatch } from 'react-redux';
import { IAppState, IMessageSent, IMessageReceived } from '../../state/initial_state';

export default function(props:{}) {
  const state = useSelector((state:IAppState) => state);
  const dispatch = useDispatch();

  function handleToggle(params:any) {
    dispatch({ type: "TOGGLE_MESSAGES" });
  }

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
    <div className={`Messages ${state.config.filters ? 'Messages--shown' : 'Messages-hidden'}`}>
      <div className="Messages__toggle" onClick={handleToggle}>
        {state.config.filters ? 'Collapse' : 'Expand'}
      </div>
      <ul>
        <li className="Messages__group">
          <div className="Messages__group__name">
            {state.messagesSentCount} Messages Sent
          </div>
          <ul>
            <TransitionGroup className="Messages__group__messages-sent">
              {getFilteredMessagesSent().map((message, i) => (
                <CSSTransition
                  key={i}
                  timeout={3000}
                  classNames="Messages__group__messages-sent__message"
                >
                  <li className="Messages__message">
                    <div className="Messages__message__description">
                      sent from <span>{message.from}</span> <span className="Messages__message__description__date">{message.datetime}</span>
                    </div>
                    <pre>
                      {message.message}
                    </pre>
                  </li>
                </CSSTransition>
              ))}
            </TransitionGroup>
          </ul>
        </li>
        <li className="Messages__group">
          <div className="Messages__group__name">
            {state.messagesReceivedCount} Messages Received
          </div>
          <ul>
            <TransitionGroup className="Messages__group__messages-received">
              {getFilteredMessagesReceived().map((message, i) => (
                <CSSTransition
                  key={i}
                  timeout={3000}
                  classNames="Messages__group__messages-received__message"
                >
                  <li className="Messages__message" key={i}>
                    <div className="Messages__message__description">
                      received at <span>{message.at}</span> <span className="Messages__message__description__date">{message.datetime}</span>
                    </div>
                    <pre>
                      {message.message}
                    </pre>
                  </li>
                </CSSTransition>
              ))}
            </TransitionGroup>
          </ul>
        </li>
      </ul>
    </div>
  )
}

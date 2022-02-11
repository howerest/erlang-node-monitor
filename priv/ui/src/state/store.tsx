import {createStore} from 'redux';
import {appReducer} from './reducers';

const store = createStore(
  appReducer,
  (window as any).__REDUX_DEVTOOLS_EXTENSION__ && (window as any).__REDUX_DEVTOOLS_EXTENSION__()
);
export default store;

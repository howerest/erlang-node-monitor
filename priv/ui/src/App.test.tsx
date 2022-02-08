import React from 'react';
import { render, screen } from '@testing-library/react';
import App from './App';

xit('renders MainBar, Network and NodeDetails', () => {
  render(<App />);
  const mainTitleEl = screen.getByText(/erlang node monitor/i);
  expect(mainTitleEl).toBeInTheDocument();
});

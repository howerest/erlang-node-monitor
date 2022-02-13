const opts = {
  layout: {
    hierarchical: {
			enabled: true,
			levelSeparation: 200,
			nodeSpacing: 200
		}
  },
  nodes: {
    font: {
      face: "monospace"
    },
    color: {
      highlight: "black"
    }
  },
  edges: {
    width: 5,
    color: "#333333"
  },
  interaction: {
    tooltipDelay: 400,
    hideEdgesOnDrag: true
  }
};

export {
  opts
}

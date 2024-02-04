function nodeColor(description) {
  switch (description) {
    case "standing":
      return "#248ced";
    case "laying":
      return "#81ff33";
    case "goal":
      return "gold";
    case "start":
      return "#87492d";
    default:
      return "black";
  }
  
}

class GraphVisualization {
  
  constructor(nodes, links, terrain) {
    this.fixed = true;
    this.nodes = nodes;
    this.links = links;
    this.terrain = terrain;

    var dimX = Math.max(...terrain.map(t => t.x)) + 1
    var dimY = Math.max(...terrain.map(t => t.y)) + 1
    var maxDim = Math.max(dimX, dimY)

    this.width = maxDim;
    this.height = maxDim;

    d3.select("#network").html("");

    const svg = d3.select("#network")
      .append("svg")
        .attr("viewBox", [0, 0, this.width, this.height])
        .style("font", "5px sans-serif");

    // Per-type markers, as they don't inherit styles.
    svg.append("defs").append("marker")
        .attr("id", `arrow`)
        .attr("viewBox", "0 -5 10 10")
        .attr("refX", 15)
        .attr("refY", -0.5)
        .attr("markerWidth", 6)
        .attr("markerHeight", 6)
        .attr("orient", "auto")
      .append("path")
        .attr("fill", "#696969")
        .attr("alpha", 0.5)
        .attr("d", "M0,-5L10,0L0,5");

    // grey squares
    const spacing = 0.1;
    this.background = svg.selectAll("foo")
      .data(this.terrain)
      .enter()
      .append("rect")
      .attr("x", d => d.x + spacing)
      .attr("y", d => d.y + spacing)
      .attr("height", d => 1 - spacing)
      .attr("width", d => 1 - spacing)
      .attr("fill", "#cfcfcf")
      .style("opacity", 0.2)

    // links
    const link = svg.append("g")
      .attr("fill", "none")
      .attr("stroke-width", 0.02)
    .selectAll("path")
    .data(links)
    .join("path")
      .attr("stroke", d => "#696969")
      .attr("marker-end", d => `url(#arrow)`)
      // .attr("d", linkArc)
      .style("opacity", d => d.used ? 1.0 : 0.3);
    this.link = link;

    // nodes
    const node = svg.append("g")
      .attr("stroke-linecap", "round")
      .attr("stroke-linejoin", "round")
      .selectAll()
    .data(nodes)
      .join("circle")
      .attr("r", 0.1)
      .attr("fill", d => nodeColor(d.description))
      // .attr("transform", d => `translate(${d.x},${d.y})`)
      .style("opacity", d => d.visited ? 1.0 : 0.3)
    this.node = node;

    this.simulation = this.buildSimulation();
    this.fixedLayout();
  }

  toggleLayout() {
    this.fixed = this.fixed ? false : true;
    if (this.fixed) this.fixedLayout();
    else this.forceLayout();
  }

  fixedLayout() {
    this.simulation.stop();

    this.background.attr("visibility", "visible");

    this.link
      .attr("d", d => {
        const r = Math.hypot(d.target.posX - d.source.posX, d.target.posY - d.source.posY);
        return `
          M${d.source.posX},${d.source.posY}
          A${r},${r} 0 0,1 ${d.target.posX},${d.target.posY}
        `;
      });
    
    this.node
      .attr("cx", d => d.posX)
      .attr("cy", d => d.posY);    
  }

  forceLayout() {    
    this.simulation = this.buildSimulation();

    this.background.attr("visibility", "hidden");

    // initialize simulation with fixed positions
    for (var i = 0; i < this.nodes.length; i++) {
      this.nodes[i].x = this.nodes[i].posX;
      this.nodes[i].y = this.nodes[i].posY;
    }

  }

  buildSimulation() {
    return d3.forceSimulation(this.nodes)
      .force("center",  d3.forceCenter(this.width / 2, this.height / 2))
      .force("link", d3.forceLink(this.links).id(i => i.id).distance(1))
      .force("charge", d3.forceManyBody().strength(0))
      .force("bounds", () => {
        for (var i = 0; i < this.nodes.length; i++) {
          this.nodes[i].x = Math.min(this.width - 0.5, Math.max(0.5, this.nodes[i].x));
          this.nodes[i].y = Math.min(this.height - 0.5, Math.max(0.5, this.nodes[i].y));
        }
      })
      
      .on("tick", () => {
        this.link
          .attr("d", d => {
            const r = Math.hypot(d.target.x - d.source.x, d.target.y - d.source.y);
            return `
              M${d.source.x},${d.source.y}
              A${r},${r} 0 0,1 ${d.target.x},${d.target.y}
            `;
          });

        this.node
          .attr("cx", d => d.x)
          .attr("cy", d => d.y);
      });
  }

  animate(nodes, links) {
    // reflect changes on d3 data
    for (var i = 0; i < nodes.length; i++) 
      this.nodes[i].visited = nodes[i].visited;
    for (var i = 0; i < links.length; i++) 
      this.links[i].used =  links[i].used;
    
    // refresh style
    this.node
      .style("opacity", d => d.visited ? 1.0 : 0.3)
      
    this.link
      .style("opacity", d => d.used ? 1.0 : 0.3);
  }
}
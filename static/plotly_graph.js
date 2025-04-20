// Function to fetch stats and update a graph
export async function fetchStats(div, endPoint) {
  try {
    console.log("endpoint:", endPoint);
    const endpointUrl = "http://localhost:8080/" + endPoint;
    const response = await fetch(endpointUrl);
    const data = await response.json();
    console.log("Fetched data:", data);

    // Render the graph (use Plotly.react for updates)
    Plotly.react(div, data.traces, data.layout);
  } catch (error) {
    console.error("Error fetching statistics:", error);
  }
}

// Function to fetch data and update the graph
export async function fetchAndRender(div, endPoint) {
  try {
    console.log("endpoint:", endPoint);
    const endpointUrl = "http://localhost:8080/data/" + endPoint;
    const response = await fetch(endpointUrl);
    const data = await response.json();

    console.log("Fetched data:", data);

    // Render the graph (use Plotly.react for updates)
    Plotly.react(div, data.traces, data.layout);
  } catch (error) {
    console.error("Error AFTER fetching:", error);
  }
}

async function get_symbols() {
  const endpointUrl = "http://localhost:8080/symbols";
  const response = await fetch(endpointUrl);
  const data = await response.json();
  return data.symbols;
}

export async function fetchAndRenderAll() {
  try {
    const symbols = await get_symbols();
    console.log("fetchandrenderal:", symbols);
    const symbols_array = symbols.split(",");
    const length = symbols.length;
    console.log("symbols before call:", symbols_array);
    const container = document.getElementById("graph");
    const newDiv = document.createElement("div");
    newDiv.style.marginBottom = "20px";
    newDiv.style.width = "1400px";
    newDiv.style.height = "900px";
    newDiv.style.marginLeft = "auto";
    newDiv.style.marginRight = "auto";
    newDiv.style.display = "block";
    newDiv.id = "Statistics";
    const stats = await fetchStats(newDiv, "stats");
    container.appendChild(newDiv);
    for (let i = 0; i < length; i++) {
      const symbol = symbols_array[i];
      if (symbol) {
        console.log("Fetching data for:", symbol);
        const newDiv = document.createElement("div");
        newDiv.style.marginBottom = "20px";
        newDiv.style.width = "1400px";
        newDiv.style.height = "900px";
        newDiv.style.marginLeft = "auto";
        newDiv.style.marginRight = "auto";
        newDiv.style.display = "block";
        newDiv.id = symbol;
        const result = await fetchAndRender(newDiv, symbol);
        container.appendChild(newDiv);
      }
    }
  } catch (error) {
    console.error("Error in fetchAndRenderAll:", error);
  }
}

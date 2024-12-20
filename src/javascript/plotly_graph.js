// Function to fetch data and update the graph
export async function fetchAndRender(div, endPoint) {
  try {
    console.log("endpoint:", endPoint);
    const endpointUrl = "http://localhost:8080/" + endPoint;
    const response = await fetch(endpointUrl);
    const data = await response.json();

    if (!data.data[0] || !data.data[0].x || !data.data[0].y) {
      console.error("Invalid data structure:", data);
      return;
    }

    const text = data.data[0].name;

    console.log("Fetched data:", data);

    const xValues = data.data[0].x;
    const yValues = data.data[0].y;
    const buyValues = data.data[0].buy;
    const sellValues = data.data[0].sell;

    // Filter data based on the "order" field
    const buyX = [];
    const buyY = [];
    const sellX = [];
    const sellY = [];
    for (let i = 0; i < yValues.length; i++) {
      if (buyValues[i] !== null) {
        buyX.push(xValues[i]);
        buyY.push(yValues[i]);
      }
    }
    for (let i = 0; i < yValues.length; i++) {
      if (sellValues[i] !== null) {
        sellX.push(xValues[i]);
        sellY.push(yValues[i]);
      }
    }

    // Process data for Plotly (assuming it returns arrays x and y)
    const trace = {
      x: xValues,
      y: yValues,
      text: text,
      type: "scatter",
      name: endPoint,
    };

    const buyTrace = {
      x: buyX,
      y: buyY,
      type: "scatter",
      mode: "markers", // Only markers for dots
      marker: { color: "green", size: 10 },
      name: "Buy",
    };

    const sellTrace = {
      x: sellX,
      y: sellY,
      type: "scatter",
      mode: "markers", // Only markers for dots
      marker: { color: "red", size: 10 },
      name: "Sell",
    };

    const layout = {
      width: 1000,
      height: 700,
      title: endPoint,
      xaxis: {
        title: "X-axis",
        type: "category",
        tickmode: "linear",
        dtick: 20,
        showticklabels: false,
      },
      yaxis: { title: "Y-axis" },
    };

    // Render the graph (use Plotly.react for updates)
    Plotly.react(div, [trace, buyTrace, sellTrace], layout);
  } catch (error) {
    console.error("Error AFTER fetching:", error);
  }
}

export async function fetchAndRenderAll(symbols) {
  try {
    console.log("fetchandrenderal:", symbols);
    const symbols_array = symbols.split(",");
    const length = symbols.length;
    console.log("symbols before call:", symbols_array);
    const container = document.getElementById("graph");
    for (let i = 0; i < length; i++) {
      const symbol = symbols_array[i];
      if (symbol) {
        console.log("Fetching data for:", symbol);
        const newDiv = document.createElement("div");
        newDiv.style.marginBottom = "20px";
        newDiv.style.width = "1000px";
        newDiv.style.height = "700px";
        newDiv.style.marginLeft = "auto";
        newDiv.style.marginRight = "auto";
        newDiv.style.display = "block"; // Ensure it's treated as a block element
        newDiv.id = symbol;
        const result = await fetchAndRender(newDiv, symbol);
        container.appendChild(newDiv);
      }
    }
  } catch (error) {
    console.error("Error in fetchAndRenderAll:", error);
  }
}

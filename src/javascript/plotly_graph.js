const endpointUrl = "http://localhost:8080/graphs";

// Function to fetch data and update the graph
async function fetchAndRender() {
                try {
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
    const orderValues = data.data[0].order;

    // Filter data based on the "order" field
    const filteredX = [];
    const filteredY = [];
    for (let i = 0; i < orderValues.length; i++) {
      if (orderValues[i] !== null) {
        filteredX.push(xValues[i]);
        filteredY.push(yValues[i]);
      }
    }

    // Process data for Plotly (assuming it returns arrays x and y)
    const trace = {
      x: xValues,
      y: yValues,
      text: text,
      type: "scatter",
    };

    const dotTrace = {
      x: filteredX,
      y: filteredY,
      type: "scatter",
      mode: "markers", // Only markers for dots
      marker: { color: "red", size: 10 },
      name: "Highlighted Dots",
    };

    const layout = {
      title: "Dynamic Plotly Graph",
      xaxis: { title: "X-axis", type: "category", tickmode: "linear", dtick: 20, showticklabels: false,},
      yaxis: { title: "Y-axis" },
    };

    // Render the graph (use Plotly.react for updates)
    Plotly.react("graph", [trace, dotTrace], layout);
  } catch (error) {
    console.error("Error AFTER fetching:", error);
  }
}

// Initial render
fetchAndRender();

// Refresh data every 5 seconds
setInterval(fetchAndRender, 5000);

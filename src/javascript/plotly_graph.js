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

    console.log("Fetched data:", data);

    // Process data for Plotly (assuming it returns arrays x and y)
    const trace = {
      x: data.data[0].x,
      y: data.data[0].y,
      type: "scatter",
    };

    const layout = {
      title: "Dynamic Plotly Graph",
      xaxis: { title: "X-axis", type: "category", tickmode: "linear", dtick: 20,},
      yaxis: { title: "Y-axis" },
    };

    // Render the graph (use Plotly.react for updates)
    Plotly.react("graph", [trace], layout);
  } catch (error) {
    console.error("Error AFTER fetching:", error);
  }
}

// Initial render
fetchAndRender();

// Refresh data every 5 seconds
setInterval(fetchAndRender, 5000);

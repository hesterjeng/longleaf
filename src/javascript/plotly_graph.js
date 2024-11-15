        const endpointUrl = "/your-json-endpoint";

        // Function to fetch data and update the graph
        async function fetchAndRender() {
            try {
                const response = await fetch(endpointUrl);
                const data = await response.json();

                // Process data for Plotly (assuming it returns arrays x and y)
                const trace = {
                    x: data.x,
                    y: data.y,
                    type: 'scatter'
                };

                const layout = {
                    title: 'Dynamic Plotly Graph',
                    xaxis: { title: 'X-axis' },
                    yaxis: { title: 'Y-axis' }
                };

                // Render the graph (use Plotly.react for updates)
                Plotly.react('graph', [trace], layout);
            } catch (error) {
                console.error("Error fetching data:", error);
            }
        }

        // Initial render
        fetchAndRender();

        // Refresh data every 5 seconds
        setInterval(fetchAndRender, 5000);

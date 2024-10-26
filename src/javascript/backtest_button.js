document.addEventListener("DOMContentLoaded", () => {
  const button = document.getElementById("backtest_button");

  button.addEventListener("click", () => {
    fetch('http://localhost:8080/run_dead', { method: 'GET' })
      .then(response => {
        if (!response.ok) {
          throw new Error('Network response was not ok');
        }
        return response.json(); // or .text() for plain text response
      })
      .then(data => {
        console.log(data); // Handle the data received from the server
      })
      .catch(error => {
        console.error('There was a problem with the fetch operation:', error);
      });
  });
});

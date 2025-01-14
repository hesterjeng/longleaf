document.addEventListener("DOMContentLoaded", () => {
  const button = document.getElementById("graceful_exit");

  button.addEventListener("click", () => {
    fetch("http://localhost:8080/exit", { method: "GET" })
      .then((response) => {
        if (!response.ok) {
          throw new Error("Network response was not ok");
        }
        return response.json(); // or .text() for plain text response
      })
      .then((data) => {
        console.log(data); // Handle the data received from the server
        document.querySelector("#graceful_exit").textContent =
          "Exit Commmand Sent";
      })
      .catch((error) => {
        console.error("There was a problem with the fetch operation:", error);
      });
  });
});

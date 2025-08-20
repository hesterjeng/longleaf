// Create heading
const title = document.createElement('h1');
title.textContent = 'Hello, World!';
document.body.appendChild(title);

// Create button
const button = document.createElement('button');
button.textContent = 'Change Text';
document.body.appendChild(button);

// Add click event
button.addEventListener('click', () => {
  title.textContent = 'Text Changed!';
});

#!/bin/bash
# Deploy React frontend to OCaml server's static directory

set -e

echo "🚀 Deploying React frontend to OCaml server..."

# Build React app
echo "📦 Building React app..."
cd react
npm run build

# Copy to OCaml static directory
echo "📂 Copying to OCaml static directory..."
cd ..
rm -rf static/js static/css static/media 2>/dev/null || true
cp -r react/build/* static/

echo "✅ Frontend deployed successfully!"
echo "🌐 Your OCaml server will now serve the React app from /static/"
echo "💡 Access the dashboard at: http://localhost:8080/static/"

# Optional: Start the server
read -p "🔄 Start the OCaml server now? (y/n): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "🚀 Starting OCaml server..."
    dune exec bin/longleaf_server.exe
fi
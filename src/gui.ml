let plotly_graph_html () =
  let plot_data =
    `Assoc
      [
        ( "data",
          `List
            [
              `Assoc
                [
                  ("x", `List [ `Float 1.0; `Float 2.0; `Float 3.0 ]);
                  ("y", `List [ `Float 4.0; `Float 5.0; `Float 6.0 ]);
                  ("mode", `String "lines+markers");
                  ("name", `String "Sample Data");
                ];
            ] );
        ( "layout",
          `Assoc
            [
              ("title", `String "Sample Plotly Graph");
              ("xaxis", `Assoc [ ("title", `String "X Axis") ]);
              ("yaxis", `Assoc [ ("title", `String "Y Axis") ]);
            ] );
      ]
  in

  let json_data = Yojson.Basic.to_string plot_data in
  let plotly_html = Template.render json_data in
  plotly_html

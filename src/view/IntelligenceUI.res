module StringStore = LocalStorage.MakeJsonable(String)

module DiagnosticView = {
  @react.component
  let make = (~d, ~onClick as givenOnClick=?, ~selected=false) => {
    let onClick = givenOnClick->Option.map((givenOnClick, e) => {
      ReactEvent.Mouse.stopPropagation(e)
      givenOnClick(d)
    })
    <div
      ?onClick
      style={ReactDOM.Style.make(
        ~padding="0.1rem 0.5rem",
        ~cursor="pointer",
        ~fontSize="0.95rem",
        ~background={
          if selected {
            "rgba(220,220,220,1)"
          } else {
            "rgba(0,0,0,0)"
          }
        },
        (),
      )}>
      {React.string(Diagnostic.message(d))}
    </div>
  }
}

module Indicator = {
  let error =
    <svg
      xmlns="http://www.w3.org/2000/svg"
      version="1.1"
      width="0.8em"
      height="0.8em"
      style={ReactDOM.Style.make(
        ~position="relative",
        ~top="0.05em",
        ~fill="none",
        ~stroke="currentColor",
        ~strokeLinejoin="round",
        ~strokeLinecap="round",
        (),
      )}
      viewBox="0 0 100 100">
      <path d="M 31,10 69,10 90,31 90,69 69,90 31,90 10,69 10,31 z" strokeWidth="10" />
      <path d="M 35,35 65,65" strokeWidth="12" />
      <path d="M 35,65 65,35" strokeWidth="12" />
    </svg>

  let information =
    <svg
      xmlns="http://www.w3.org/2000/svg"
      version="1.1"
      width="0.96em"
      height="0.8em"
      style={ReactDOM.Style.make(
        ~position="relative",
        ~top="0.05em",
        ~fill="none",
        ~stroke="currentColor",
        ~strokeLinejoin="round",
        ~strokeLinecap="round",
        (),
      )}
      viewBox="0 0 120 100">
      <circle cx="60" cy="45" r="20" strokeWidth="12" />
      <path d="M10,50 Q60,-15 110,50" strokeWidth="10" />
      <path d="M10,50 Q60,115 110,50" strokeWidth="10" />
    </svg>

  let spinner =
    <>
      <style>
        {React.string(`
@keyframes kf-loading-intelligence {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}
`)}
      </style>
      <span
        style={ReactDOM.Style.make(
          ~width="12px",
          ~height="12px",
          ~position="relative",
          ~borderRadius="6px",
          ~display="inline-block",
          (),
        )}>
        <span
          style={ReactDOM.Style.make(
            ~display="inline-block",
            ~width="10px",
            ~height="10px",
            ~borderRadius="5px",
            ~border="1px solid #666",
            ~borderColor="#666 transparent #666  transparent",
            ~animation="kf-loading-intelligence 1.2s linear infinite",
            (),
          )}
        />
      </span>
    </>

  let spacer = (~small) =>
    <span
      style={ReactDOM.Style.make(
        ~display="inline-block",
        ~width={
          if small {
            "0.3em"
          } else {
            "0.7em"
          }
        },
        (),
      )}
    />

  module Filter = {
    @react.component
    let make = (~active, ~onClick, ~children) => {
      let (hovering, setHovering) = React.useState(_ => false)
      let onMouseOver = _ => setHovering(_ => true)
      let onMouseOut = _ => setHovering(_ => false)
      let hoverStyle = ReactDOM.Style.make(
        ~background="#ccc",
        ~borderRadius="1000px",
        ~padding="0.125rem 0.5rem 0.125rem 0.4rem",
        ~marginRight="0.25rem",
        ~cursor="default",
        (),
      )
      let activeStyle = ReactDOM.Style.make(
        ~background="#ddd",
        ~borderRadius="1000px",
        ~padding="0.125rem 0.5rem 0.125rem 0.4rem",
        ~marginRight="0.25rem",
        ~cursor="default",
        (),
      )
      let defaultStyle = ReactDOM.Style.make(
        ~borderRadius="1000px",
        ~padding="0.125rem 0.5rem 0.125rem 0.4rem",
        ~marginRight="0.25rem",
        ~cursor="default",
        (),
      )
      <span
        onClick
        onMouseOver
        onMouseOut
        style={if hovering {
          hoverStyle
        } else if active {
          activeStyle
        } else {
          defaultStyle
        }}>
        {children}
      </span>
    }
  }

  @react.component
  let make = (
    ~errorCount,
    ~errorStatus: [#ready | #loading],
    ~informationCount,
    ~informationStatus: [#ready | #loading],
    ~focusedTabs,
    ~onClickTab,
  ) => {
    let errorString = React.string(
      switch errorCount {
      | 1 => "1 error"
      | n => Int.toString(n) ++ " errors"
      },
    )
    let informationString = React.string(
      switch informationCount {
      | 1 => "1 insight"
      | n => Int.toString(n) ++ " insights"
      },
    )
    <>
      <Filter active={focusedTabs->Array.includes("errors")} onClick={_ => onClickTab("errors")}>
        {if errorStatus == #ready {
          error
        } else {
          spinner
        }}
        {spacer(~small=true)}
        {errorString}
      </Filter>
      <Filter
        active={focusedTabs->Array.includes("information")}
        onClick={_ => onClickTab("information")}>
        {if informationStatus == #ready {
          information
        } else {
          spinner
        }}
        {spacer(~small=true)}
        {informationString}
      </Filter>
    </>
  }
}

@react.component
let make = (~intelligence: array<Diagnostic.t>, ~onClickDiagnostic=?, ~style as givenStyle=?) => {
  let (visible, setVisible) = React.useState(_ =>
    StringStore.get("RST_ERROR_PANEL_VISIBLE")
    ->Or_error.getWithDefault("")
    ->String.split(",")
    ->Array.keep(v => v !== "")
  )

  let (selected, setSelected) = React.useState(_ => None)

  let toggleTab = tab => {
    let newVisible = if visible->Array.includes(tab) {
      visible->Array.keep(v => v !== tab)
    } else {
      Array.concat(visible, [tab])
    }
    StringStore.set("RST_ERROR_PANEL_VISIBLE", newVisible->Js.Array2.joinWith(","))
    setVisible(_ => newVisible)
  }

  let (errors, information) =
    intelligence->Array.partition(d => Diagnostic.kind(d) == Diagnostic.Kind.Error)

  let status = #ready

  let nErrors = Array.length(errors)
  let nInformation = Array.length(information)

  let visible = visible->Array.keep(v =>
    switch v {
    | "errors" => nErrors > 0
    | "information" => nInformation > 0
    | _ => false
    }
  )

  let commonStyle = ReactDOM.Style.make(
    ~background="white",
    ~bottom="0",
    ~position="absolute",
    ~borderTop="1px solid black",
    ~display="flex",
    ~flexDirection="column",
    ~margin="0",
    ~padding="0.25em",
    ~width="100%",
    (),
  )
  let hiddenStyle = commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~height="30px", ()))
  let visibleStyle = commonStyle->ReactDOM.Style.combine(ReactDOM.Style.make(~height="200px", ()))
  let defaultStyle = if visible->Array.length === 0 {
    hiddenStyle
  } else {
    visibleStyle
  }
  let style =
    givenStyle
    ->Option.map(ReactDOM.Style.combine(defaultStyle))
    ->Option.getWithDefault(defaultStyle)

  let section = title =>
    <div
      style={ReactDOM.Style.make(
        ~padding="0.25rem 1.5rem",
        ~color="#333",
        ~background="#eee",
        ~fontSize="0.6rem",
        ~textTransform="uppercase",
        ~letterSpacing="0.05em",
        (),
      )}>
      {React.string(title)}
    </div>

  let onClick = (d, _) => {
    setSelected(_ => Some(Diagnostic.hash(d)))
    onClickDiagnostic->Option.iter(f => f(d))
  }

  <div style onClick={_ => setSelected(_ => None)}>
    <div
      style={ReactDOM.Style.make(
        ~padding="0 0.5rem",
        ~fontSize="0.9rem",
        ~color={
          if visible->Array.length === 0 {
            "#333"
          } else {
            "black"
          }
        },
        (),
      )}>
      <Indicator
        errorCount=nErrors
        errorStatus={status}
        informationCount=nInformation
        informationStatus={status}
        focusedTabs={visible}
        onClickTab={toggleTab}
      />
    </div>
    {if visible->Array.length !== 0 {
      <div
        style={ReactDOM.Style.make(
          ~width="calc(100% - 1rem)",
          ~flexGrow="1",
          ~margin="0.25rem 0.5rem 0.5rem 0.5rem",
          ~borderRadius="2px",
          ~border="1px solid #aaa",
          ~overflowY="scroll",
          (),
        )}>
        {if visible->Array.includes("errors") {
          <>
            {section("Errors")}
            {errors
            ->Array.map(d => {
              <DiagnosticView
                d
                onClick={onClick(d)}
                selected={Some(Diagnostic.hash(d)) === selected}
                key={Diagnostic.hash(d)->Hash.toString}
              />
            })
            ->React.array}
          </>
        } else {
          React.null
        }}
        {if visible->Array.includes("information") {
          <>
            {section("Information")}
            {information
            ->Array.map(d => {
              <DiagnosticView
                d
                onClick={onClick(d)}
                selected={Some(Diagnostic.hash(d)) === selected}
                key={Diagnostic.hash(d)->Hash.toString}
              />
            })
            ->React.array}
          </>
        } else {
          React.null
        }}
      </div>
    } else {
      React.null
    }}
  </div>
}

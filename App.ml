open Revery
open Revery.UI
open Revery.UI.Components
module Row =
  struct
    let%component createElement ~children  () hooks =
    let style =
             let open Style in
               [flexDirection `Row;
               alignItems `Stretch;
               justifyContent `Center;
               flexGrow 1] in
     ((View.createElement ~style ~children ())[@JSX ]), hooks
  end
module Column =
  struct
    let%component createElement ~children  () hooks =
    let style =
             let open Style in
               [flexDirection `Column;
               alignItems `Stretch;
               justifyContent `Center;
               backgroundColor Colors.darkGrey;
               flexGrow 1] in
      ((View.createElement ~style ~children ())[@JSX ]), hooks
  end
module Button =
  struct
    let%component createElement ?fontFamily:(family= "Roboto-Regular.ttf") 
      ~contents:(contents : string)  ~onClick  () hooks =
      let clickableStyle =
             let open Style in
               [position `Relative;
               backgroundColor Colors.lightGrey;
               justifyContent `Center;
               alignItems `Center;
               flexGrow 1;
               width 125;
               margin 10] in
           let viewStyle =
             let open Style in
               [position `Relative;
               justifyContent `Center;
               alignItems `Center] in
           let textStyle =
             let open Style in
               [color Colors.black;
               fontFamily family; fontSize 32.] in
      ((Clickable.createElement ~style:clickableStyle ~onClick
                                ~children:[((View.createElement ~style:viewStyle
                                ~children:[((Text.createElement
                                               ~style:textStyle
                                               ~text:contents ~children:[] ())
                                          [@JSX ])] ())
                           [@JSX ])] ())[@JSX ]), hooks
  end
(*
module Display =
  struct
    let component = React.component "Display"
    let createElement ~display:(display : string)  ~curNum:(curNum : string) 
      ~children:_  () =
      component
        (fun hooks ->
           let viewStyle =
             let open Style in
               [backgroundColor Colors.white;
               height 120;
               flexDirection `Column;
               alignItems `Stretch;
               justifyContent `FlexStart;
               flexGrow 2] in
           let displayStyle =
             let open Style in
               [color Colors.black;
               fontFamily "Roboto-Regular.ttf";
               fontSize 20;
               margin 15] in
           let numStyle =
             let open Style in
               [color Colors.black;
               fontFamily "Roboto-Regular.ttf";
               fontSize 32;
               margin 15] in
           (hooks,
             ((View.createElement ~style:viewStyle
                 ~children:[((Text.createElement ~style:displayStyle
                                ~text:display ~children:[] ())
                           [@JSX ]);
                           ((Text.createElement ~style:numStyle ~text:curNum
                               ~children:[] ())
                           [@JSX ])] ())[@JSX ])))
  end
type operator = [ `Nop  | `Add  | `Sub  | `Mul  | `Div ]
let showFloat float =
  let string = string_of_float float in
  if
    ((String.length string) > 1) &&
      ((string.[(String.length string) - 1]) = '.')
  then String.sub string 0 ((String.length string) - 1)
  else string
type state =
  {
  operator: operator ;
  result: float ;
  display: string ;
  number: string }
type action =
  | BackspaceKeyPressed 
  | ClearKeyPressed of bool 
  | DotKeyPressed 
  | NumberKeyPressed of string 
  | OperationKeyPressed of operator 
  | PlusMinusKeyPressed 
  | ResultKeyPressed 
let eval state newOp =
  let newOpString =
    match newOp with
    | `Nop -> ""
    | `Add -> "+"
    | `Sub -> "-"
    | `Mul -> "\195\151"
    | `Div -> "\195\183" in
  let partitionedDisplay = String.split_on_char '!' state.display in
  let display =
    List.nth partitionedDisplay ((List.length partitionedDisplay) - 1) in
  let (newDisplay, newResult) =
    match state.operator with
    | #operator when state.number = "" ->
        ("Error: Can't evaluate binary operator without input!",
          (state.result))
    | `Nop -> ((state.number ^ newOpString), (float_of_string state.number))
    | `Add ->
        ((display ^ (state.number ^ newOpString)),
          (state.result +. (float_of_string state.number)))
    | `Sub ->
        ((display ^ (state.number ^ newOpString)),
          (state.result -. (float_of_string state.number)))
    | `Mul ->
        ((display ^ (state.number ^ newOpString)),
          (state.result *. (float_of_string state.number)))
    | `Div ->
        if (float_of_string state.number) <> 0.
        then
          ((display ^ (state.number ^ newOpString)),
            (state.result /. (float_of_string state.number)))
        else ("Error: Divide by zero!", (state.result)) in
  (newResult, newDisplay)
let reducer action state =
  match action with
  | BackspaceKeyPressed ->
      (match state.number = "" with
       | true -> state
       | false ->
           {
             state with
             number =
               (String.sub state.number 0 ((String.length state.number) - 1))
           })
  | ((ClearKeyPressed (ac))[@explicit_arity ]) ->
      (match ac with
       | true -> { operator = `Nop; result = 0.; display = ""; number = "" }
       | false -> { state with number = "" })
  | DotKeyPressed ->
      (match String.contains state.number '.' with
       | true -> state
       | false -> { state with number = (state.number ^ ".") })
  | ((NumberKeyPressed (n))[@explicit_arity ]) ->
      { state with number = (state.number ^ n) }
  | ((OperationKeyPressed (o))[@explicit_arity ]) ->
      let (result, display) = eval state o in
      { operator = o; result; display; number = "" }
  | PlusMinusKeyPressed ->
      if (state.number <> "") && (((state.number).[0]) = '-')
      then
        {
          state with
          number =
            (String.sub state.number 1 ((String.length state.number) - 1))
        }
      else { state with number = ("-" ^ state.number) }
  | ResultKeyPressed ->
      let (result, display) = eval state `Nop in
      { operator = `Nop; result; display; number = (showFloat result) }
module KeyboardInput =
  struct
    type state = {
      ref: node option ;
      hasFocus: bool }
    type action =
      | Focused of bool 
      | SetRef of node 
    let reducer action state =
      match action with
      | ((Focused (v))[@explicit_arity ]) -> { state with hasFocus = v }
      | ((SetRef (v))[@explicit_arity ]) ->
          { state with ref = ((Some (v))[@explicit_arity ]) }
    let component = React.component "KeyboardInput"
    let createElement ~children:_  ~dispatch:parentDispatch  () =
      component
        (fun hooks ->
           let (v, dispatch, hooks) =
             Hooks.reducer ~initialState:{ ref = None; hasFocus = false }
               reducer hooks in
           let hooks =
             Hooks.effect Always
               (fun () ->
                  if not v.hasFocus
                  then
                    (match v.ref with
                     | ((Some (v))[@explicit_arity ]) -> Focus.focus v
                     | None -> ());
                  None) hooks in
           let onBlur () = dispatch ((Focused (false))[@explicit_arity ]) in
           let onFocus () = dispatch ((Focused (true))[@explicit_arity ]) in
           let respondToKeys (e : NodeEvents.keyEventParams) =
             match e.key with
             | Key.KEY_BACKSPACE -> parentDispatch BackspaceKeyPressed
             | Key.KEY_C when e.ctrlKey ->
                 parentDispatch ((ClearKeyPressed (true))[@explicit_arity ])
             | Key.KEY_C ->
                 parentDispatch ((ClearKeyPressed (false))[@explicit_arity ])
             | Key.KEY_EQUAL when e.shiftKey ->
                 parentDispatch ((OperationKeyPressed (`Add))
                   [@explicit_arity ])
             | Key.KEY_MINUS when e.ctrlKey ->
                 parentDispatch PlusMinusKeyPressed
             | Key.KEY_MINUS ->
                 parentDispatch ((OperationKeyPressed (`Sub))
                   [@explicit_arity ])
             | Key.KEY_8 when e.shiftKey ->
                 parentDispatch ((OperationKeyPressed (`Mul))
                   [@explicit_arity ])
             | Key.KEY_SLASH ->
                 parentDispatch ((OperationKeyPressed (`Div))
                   [@explicit_arity ])
             | Key.KEY_PERIOD -> parentDispatch DotKeyPressed
             | Key.KEY_EQUAL -> parentDispatch ResultKeyPressed
             | Key.KEY_0 ->
                 parentDispatch ((NumberKeyPressed ("0"))[@explicit_arity ])
             | Key.KEY_1 ->
                 parentDispatch ((NumberKeyPressed ("1"))[@explicit_arity ])
             | Key.KEY_2 ->
                 parentDispatch ((NumberKeyPressed ("2"))[@explicit_arity ])
             | Key.KEY_3 ->
                 parentDispatch ((NumberKeyPressed ("3"))[@explicit_arity ])
             | Key.KEY_4 ->
                 parentDispatch ((NumberKeyPressed ("4"))[@explicit_arity ])
             | Key.KEY_5 ->
                 parentDispatch ((NumberKeyPressed ("5"))[@explicit_arity ])
             | Key.KEY_6 ->
                 parentDispatch ((NumberKeyPressed ("6"))[@explicit_arity ])
             | Key.KEY_7 ->
                 parentDispatch ((NumberKeyPressed ("7"))[@explicit_arity ])
             | Key.KEY_8 ->
                 parentDispatch ((NumberKeyPressed ("8"))[@explicit_arity ])
             | Key.KEY_9 ->
                 parentDispatch ((NumberKeyPressed ("9"))[@explicit_arity ])
             | _ -> () in
           (hooks,
             ((View.createElement
                 ~ref:(fun r -> dispatch ((SetRef (r))[@explicit_arity ]))
                 ~onBlur ~onFocus
                 ~style:(let open Style in
                           [position `Absolute; width 1; height 1])
                 ~onKeyDown:respondToKeys ~children:[] ())[@JSX ])))
  end
module Calculator =
  struct
    let component = React.component "Calculator"
    let createElement ~children:_  () =
      component
        (fun hooks ->
           let ({ display; number;_}, dispatch, hooks) =
             Hooks.reducer
               ~initialState:{
                               operator = `Nop;
                               result = 0.;
                               display = "";
                               number = ""
                             } reducer hooks in
           (hooks,
             ((Column.createElement
                 ~children:[((KeyboardInput.createElement ~dispatch
                                ~children:[] ())
                           [@JSX ]);
                           ((Display.createElement ~display ~curNum:number
                               ~children:[] ())
                           [@JSX ]);
                           ((Row.createElement
                               ~children:[((Button.createElement
                                              ~contents:"AC"
                                              ~onClick:(fun _ ->
                                                          dispatch
                                                            ((ClearKeyPressed
                                                                (true))
                                                            [@explicit_arity
                                                              ]))
                                              ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"C"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((ClearKeyPressed
                                                               (false))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement
                                             ~contents:"\194\177"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           PlusMinusKeyPressed)
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement
                                             ~fontFamily:"FontAwesome5FreeSolid.otf"
                                             ~contents:{||}
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           BackspaceKeyPressed)
                                             ~children:[] ())
                                         [@JSX ])] ())
                           [@JSX ]);
                           ((Row.createElement
                               ~children:[((Button.createElement
                                              ~contents:"7"
                                              ~onClick:(fun _ ->
                                                          dispatch
                                                            ((NumberKeyPressed
                                                                ("7"))
                                                            [@explicit_arity
                                                              ]))
                                              ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"8"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((NumberKeyPressed
                                                               ("8"))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"9"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((NumberKeyPressed
                                                               ("9"))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement
                                             ~contents:"\195\183"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((OperationKeyPressed
                                                               (`Div))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ])] ())
                           [@JSX ]);
                           ((Row.createElement
                               ~children:[((Button.createElement
                                              ~contents:"4"
                                              ~onClick:(fun _ ->
                                                          dispatch
                                                            ((NumberKeyPressed
                                                                ("4"))
                                                            [@explicit_arity
                                                              ]))
                                              ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"5"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((NumberKeyPressed
                                                               ("5"))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"6"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((NumberKeyPressed
                                                               ("6"))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement
                                             ~contents:"\195\151"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((OperationKeyPressed
                                                               (`Mul))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ])] ())
                           [@JSX ]);
                           ((Row.createElement
                               ~children:[((Button.createElement
                                              ~contents:"1"
                                              ~onClick:(fun _ ->
                                                          dispatch
                                                            ((NumberKeyPressed
                                                                ("1"))
                                                            [@explicit_arity
                                                              ]))
                                              ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"2"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((NumberKeyPressed
                                                               ("2"))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"3"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((NumberKeyPressed
                                                               ("3"))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"-"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((OperationKeyPressed
                                                               (`Sub))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ])] ())
                           [@JSX ]);
                           ((Row.createElement
                               ~children:[((Button.createElement
                                              ~contents:"."
                                              ~onClick:(fun _ ->
                                                          dispatch
                                                            DotKeyPressed)
                                              ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"0"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((NumberKeyPressed
                                                               ("0"))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"="
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ResultKeyPressed)
                                             ~children:[] ())
                                         [@JSX ]);
                                         ((Button.createElement ~contents:"+"
                                             ~onClick:(fun _ ->
                                                         dispatch
                                                           ((OperationKeyPressed
                                                               (`Add))
                                                           [@explicit_arity ]))
                                             ~children:[] ())
                                         [@JSX ])] ())
                           [@JSX ])] ())[@JSX ])))
  end
let render _ = ((Calculator.createElement ~children:[] ())[@JSX ])
*)

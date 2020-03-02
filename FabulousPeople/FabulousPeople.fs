namespace FabulousPeople

open System.Diagnostics
open System.Drawing
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.LiveUpdate
open Xamarin.Forms

module App = 

  type AddModel = (string * string)

  type Page =
    | MainPage
    | AddPage

  type Person = 
    { Name : string
      Surname : string }

  type Model = 
    { Page : Page
      People : Person list
      AddPersonModel : AddModel }

  type AddMsg =
    | UpdateName of string
    | UpdateSurname of string
    | AddPerson

  type Msg =
    | ChangePage of Page
    | Add of AddMsg

  let initialModel = 
    { People = List.empty
      Page = MainPage
      AddPersonModel = ("", "") }
  
  let init () = initialModel, Cmd.none

  let addUpdate addMsg model=
    match addMsg with
    | UpdateName s -> 
      let (_, surname) = model.AddPersonModel
      { model with AddPersonModel = (s, surname) }, Cmd.none
    | UpdateSurname s ->
      let (name, _) = model.AddPersonModel
      { model with AddPersonModel = (name, s) }, Cmd.none
    | AddPerson -> 
      let (name, surname) = model.AddPersonModel
      let newPerson = { Name = name
                        Surname = surname }
      {model with People = newPerson::model.People; Page = MainPage}, Cmd.none

  let update (msg:Msg) model =
    match msg with
    | ChangePage p ->
      match p with
      | AddPage -> { model with Page = AddPage; AddPersonModel = ("", "") }, Cmd.none
      | MainPage -> { model with Page = MainPage}, Cmd.none
    | Add addMsg -> addUpdate addMsg model
  
  let view (model : Model) (dispatch : Msg -> unit) =

    let mkButton text command = 
      View.Button(text = text, command = (fun () -> dispatch command))

    let mkEntry placeholder text completed =
      View.Entry(placeholder = placeholder,
                 text = text,
                 clearButtonVisibility = ClearButtonVisibility.WhileEditing,
                 returnType = ReturnType.Next,
                 completed = completed)

    let personView person =
      View.Label(text = sprintf "%s %s" person.Name person.Surname,
                 textColor = textC,
                 backgroundColor = Color.Orange)
    let mainView =
      View.StackLayout(verticalOptions = LayoutOptions.Center ,children = [
        View.StackLayout(children = List.map personView model.People)
        mkButton "Add Person" (ChangePage AddPage)
      ])

    let addView addModel =
      let (curName, curSurname) = addModel
      View.StackLayout(verticalOptions = LayoutOptions.Center, children = [
          mkEntry "Scott" curName (UpdateName >> Add >> dispatch)
          mkEntry "Hanselmann" curSurname (UpdateSurname >> Add >> dispatch)
          mkButton "+" (Add AddPerson)
      ])

    View.ContentPage(
      content = 
        View.StackLayout(padding = Thickness 20.0, verticalOptions = LayoutOptions.Center,
          children = [
            match model.Page with
            | MainPage -> mainView
            | AddPage  -> addView model.AddPersonModel
          ])
    )

  let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


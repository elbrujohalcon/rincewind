# rincewind

Wizarding Guide for your Erlang and Elixir Projects

![](rincewind.jpeg)
_(image by [DarkGosp](https://x.com/Darkgosp))_

## Concept and Scope

Rincewind is an Erlang library that would allow you to easily build the backend for your favorite
[Wizard UI](https://ui-patterns.com/patterns/Wizard).
Rincewind will not deal with the actual UI, providing just a functional API with two sides:

* **Configuration:** Providing the corresponding API endpoints to set up as many wizards as you want, each one
  defined by its own set of rules and procedures to determine how users will move from step to step and what will be the
  result of traversing the whole (or part) of the wizard.
* **Usage:** Providing the endpoints your system will use to manage each user's navigation through any of the
  previously defined wizards, collecting data and figuring out what options to display in each step.

### Limitations

The original goal of this project is to serve as an example for [this training session](https://codebeameurope.com/trainings/everything-you-always-wanted-to-know-about-testing-on-the-beam/).
Therefore, it's not expected to be production-ready by any means. IF there is traction and need, that may change in the
future. Should that happen, this section will be removed. Before that, **USE IT AT YOUR OWN RISK**.

## Usage

### Configuration

To define a wizard with `rincewind`, you have to provide it with the corresponding configuration.
You can do that using the `create_wizard/2` function and provided the corresponding `rincewind:wizard()` definition,
like this:

```erlang
rincewind:create_wizard(WizardName, #{phases := […]})
```

[//]: # (TODO: Complete this section with the actual definition of the phases and the rest of the parameters)

### Consumption

Once you have a user that wants to go through a wizard, you can manage their progress using the following functions:

```erlang
%% Kick off the wizard process and get a reference for it
{ok, WizardProcess} = rincewind:start_runner(WizardName, UserName),
%% Get the options for the current phase
%% Other than the name, the options for the current phase are up to the callback module that implements rincewind_phase
{ok, #{name := FirstPhaseName, …}} = rincewind:current_phase(WizardProcess),
%% Receive input from the user for the current phase (it returns the definition for the next phase, if there is one)
%% The expected values depend on each phase's implementation
{next_phase, #{name := SecondPhaseName, …}} = rincewind:submit(WizardProcess, Values),
%% If the submitted values are deemed invalid, the corresponding validation errors are returned
{invalid, #{errors := …}} = rincewind:submit(WizardProcess, Values),
%% If there are no further phases, the final values chosen for each phase are returned
{done, [#{phase := FirstPhaseName, values := FirstPhaseValues}, …]} = rincewind:submit(WizardProcess, Values),
```

While that's the happy path, Rincewind also provides the means for the user to jump through phases (if that's allowed by
the wizard definition):

```erlang
%% Get the full wizard definition (it can be used to navigate through the phases)
#{name := WizardName, phases := […]} = rincewind:wizard_definition(WizardProcess),
%% Skip the current phase
{next_phase, #{phase := SecondPhaseName, …}} = rincewind:skip_phase(WizardProcess),
%% Move back to a previous phase
{next_phase, #{phase := FirstPhaseName, …}} = rincewind:jump_back(WizardProcess, FirstPhaseName),
%% Get the current values of the wizard (i.e., the values chosen _so far_)
[#{phase := FirstPhaseName, values := FirstPhaseValue, …}] = rincewind:get_values(WizardProcess),
```

[//]: # (TODO: Add more functions if we think we need them)

[//]: # (TODO: If we add a folder with examples, link it here)

[//]: # (TODO: Add implementation details that can be useful for our users, like the fact that each process is an FSM or something)

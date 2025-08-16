/*
 * SERVER RESPONSE OBJECTS
 */

/**
 * A mapping from choice IDs (as integer strings) to choice indices.
 */
type ChoiceMap = { [string]?: number }

/**
 * A series of pairs of choice IDs and choice indices. As such, the length of the array must be even.
 */
type ChoiceArray = number[]

/**
 * Serializes a choice map to a choice array.
 */
declare function serializeChoices(obj: ChoiceMap): ChoiceArray;

/**
 * Deserializes a choice map from a choice array.
 */
declare function deserializeChoices(arr: ChoiceArray): ChoiceMap;

/**
 * A header for a field of a type.
 */
type VariantFieldHeader = {
  /**
   * The human-readable name and type of the field, or just the type of a non-record type.
   */
  label: string,
  
  /**
   * How values of this field should be rendered.
   */
  type: "string" | "text" | "number" | "markup"
}

/**
 * Information about an option selection.
 */
type OptionInfo = {
  /**
   * The label for the option; usually a question or prompt.
   */
  label: string,

  /**
   * The choice ID for the option.
   */
  choice: number,

  /**
   * The labels for the option choices.
   */
  options: string[]
}

/**
 * A record representing a single variant.
 */
type VariantRecord = {
  /**
   * The values of the record's fields.
   * Interpretation of the strings depends on the field type.
   * 
   * @see VariantFieldHeader#type
   */
  fields: string[],

  /**
   * The choices made in the execution trace for this variant.
   */
  choices: ChoiceArray,

  /**
   * Information about the option selections in the execution trace for this variant.
   */
  options: OptionInfo[]
}

/**
 * A group of variants for a particular type.
 */
type VariantGroup = {
  /**
   * The headers for the type, naming the type's fields.
   * For non-record types, there should be only a single header naming that type.
   */
  headers: VariantFieldHeader[],

  /**
   * The records for the variants.
   */
  dataset: VariantRecord[]
}

/**
 * The response from the server for a code evaluation.
 */
type WNResponse = {
  /**
   * The results of the evaluation, grouped by type.
   */
  groups: VariantGroup[],
  
  /**
   * Any status messages (e.g. warnings) emitted during evaluation.
   */
  msg: string
}

/*
 * CLIENT STATES
 */

/**
 * Common fields for states that carry GF code.
 */
interface StateWithCode {
  /**
   * The QID of the focused Wikidata entity.
   */
  qid: string;

  /**
   * The target language for linearization.
   */
  lang: string;

  /**
   * The GF code snippet to evaluate.
   */
  code: string;
}

/**
 * Common fields for states that carry information about choices.
 */
interface StateWithChoices extends StateWithCode {
  /**
   * A choice map for non-option choices (i.e. a trace).
   */
  choices: ChoiceMap;

  /**
   * A choice map for options.
   */
  opts: ChoiceMap;
}

/**
 * The initial client state.
 */
type WNStateInitial = { state: "initial" }

/**
 * The client is waiting for a response from the server.
 */
type WNStateWaiting = StateWithChoices & { state: "waiting" }

/**
 * The client encountered an error while querying the server.
 */
type WNStateInvalid = StateWithChoices & {
  state: "invalid",
  
  /**
   * An error message.
   */
  error: string
}

/**
 * The client received a response from the server and is ready to display it.
 */
type WNStateValid = StateWithCode & {
  state: "valid",

  /**
   * The results received from the server.
   */
  groups: VariantGroup[]
}

/**
 * The client has fixed a trace and is ready for option selection.
 */
type WNStateInteractive = StateWithChoices & {
  state: "interactive",

  /**
   * The headers for the fixed trace's group.
   */
  headers: VariantFieldHeader[],
  
  /**
   * The record for the fixed trace.
   */
  record: VariantRecord
}

/**
 * A state for the WordNet client.
 */
type WNState = WNStateInitial | WNStateWaiting | WNStateInvalid | WNStateValid | WNStateInteractive

/**
 * Emitted when the state of the WordNet client changes.
 */
declare class WNStateChangeEvent extends Event {
  /**
   * The new state.
   */
  newState: WNState;
  
  constructor(newState: WNState);
}

/**
 * Emitted when the WordNet client consumes a result from the server.
 */
declare class WNResultEvent extends Event {
  /**
   * The received result.
   */
  result: WNResponse;
  
  constructor(result: WNResponse);
}

/**
 * A state machine for evaluating code on the WordNet server.
 */
declare class WNClient extends EventTarget {
  /**
   * Constructs a new state machine in the initial state.
   */
  constructor();

  /**
   * Updates the state, emitting a {@link WNStateChangeEvent} in the process.
   * 
   * @param state The new state.
   */
  setState(state: WNState);

  /**
   * Sets the target program for evaluation, sending a request to the server if necessary.
   * 
   * @param qid The QID of the focused Wikidata entity.
   * @param lang The target language for linearization.
   * @param code The GF code snippet to evaluate.
   */
  setProgram(qid: string, lang: string, code: string);

  /**
   * Fixes a program trace to allow for option selection.
   * The trace is specified by a variant, which is given in the form of a record.
   * 
   * @param headers The field headers for the record's group.
   * @param record The variant record for the interaction point.
   */
  setInteractionPoint(headers: VariantFieldHeader[], record: VariantRecord);

  /**
   * Sets an option for the current program, sending a request to the server if necessary.
   * This should only be used after an interaction point has been set.
   * 
   * @param choice The choice ID for the option.
   * @param index The new choice index to set for the option.
   * @see WNClient#setInteractionPoint
   */
  setOption(choice: number, index: number);

  /**
   * Sends a request to the server to evaluate a snippet of code.
   * 
   * @param qid The QID of the focused Wikidata entity.
   * @param lang The target language for linearization.
   * @param code The GF code snippet to evaluate.
   * @param choices The initial choices to use in evaluation (i.e. an execution trace prefix).
   * @param opts The options to set in evaluation.
   */
  revalidate(qid: string, lang: string, code: string, choices: ChoiceMap, opts: ChoiceMap);

  /**
   * Loads a result received from the server.
   * 
   * @param result The result to load.
   * @param qid The QID of the focused Wikidata entity.
   * @param lang The target language for linearization.
   * @param code The GF code snippet that was evaluated.
   */
  loadResult(result: WNResponse, qid: string, lang: string, code: string);
}

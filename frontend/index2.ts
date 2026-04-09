
const SESSION_KEY_STORED_STATE = 'stored_state_2';

type StoreState = {
    collections: string[]
    selectedCollection: string | null;
    query: string;
    maxResults: number;
    results: QueryResults;
};

type QueryResults = {
    num_results: number;
    results: QueryResult[];
};

type QueryResult = {
    uri: string;
    score: number;
    term_count: number;
    metadata: Record<string, string>;
};

const defaultState: StoreState = {
    collections: [],
    selectedCollection: null,
    query: "",
    maxResults: 10,
    results: { num_results: 0, results: [] }
};

const fakeState: StoreState = {
    collections: ["one", "two"],
    selectedCollection: "two",
    query: "some query",
    maxResults: 3,
    results: {
        num_results: 2, results: [{
            uri: "http://foo.bar",
            score: 20.4,
            term_count: 2,
            metadata: {}
        }, {
            uri: "http://var.foo",
            score: 13.4,
            term_count: 1,
            metadata: {}
        }]
    }
};

function restoreStoredState(): StoreState | null {
    const strState = sessionStorage.getItem(SESSION_KEY_STORED_STATE);
    return strState
        ? JSON.parse(strState)
        : null;
}

function storeState(state: StoreState): void {
    sessionStorage.setItem(SESSION_KEY_STORED_STATE, JSON.stringify(state));
}

const freshState = async (): Promise<StoreState> => {
    const state = defaultState;
    state.collections = await fireCollections();
    return state;
}

const renderCollections = async (state: StoreState, collectionSwitchHandler: EventListener) => {

    const collections = document.getElementById("collections")!;
    collections.innerHTML = '';

    const legend = document.createElement("legend");
    legend.innerText = "Search collection:";
    collections.append(legend);

    for (const collectionName of state.collections) {

        const radioName = "radio" + collectionName;

        const radio = document.createElement("input");
        radio.setAttribute("id", radioName);
        radio.setAttribute("type", "radio");
        radio.setAttribute("name", "collectionName");
        radio.setAttribute("value", collectionName)

        if (state.selectedCollection === collectionName) {
            radio.setAttribute("checked", "checked");
        }
        radio.addEventListener("change", collectionSwitchHandler);
        collections.append(radio);

        const label = document.createElement("label");
        label.setAttribute("for", radioName);
        label.innerText = collectionName;
        collections.append(label);
    }
}

function renderResults(state: StoreState): HTMLOListElement {
    const resultList = document.createElement("ol");
    if (state.selectedCollection) {
        for (let i = 0; i < state.results.results.length; i++) {
            const resultItem = renderResult(state.selectedCollection, state.results.results[i]!);
            resultList.appendChild(resultItem);
        }
    }
    return resultList;
}

function renderResult(collectionName: string, result: QueryResult): HTMLLIElement {

    const createTitle = () => {
        const title = document.createElement("div");
        title.innerText = result.metadata?.title ?? "Untitled";
        title.classList.add("result_title");
        resultItem.append(title);
        return title;
    }

    const createDesc = () => {
        const desc = document.createElement("p");
        desc.innerText = result.metadata?.description ?? "No description";
        desc.classList.add("result_desc");
        resultItem.append(desc);
        return desc;
    }

    const createTermCount = () => {
        const termCount = document.createElement("div");
        termCount.innerText = result.term_count.toString();
        resultItem.append(termCount);
        return termCount;
    }

    const createScore = () => {
        const score = document.createElement("div");
        score.innerText = result.score.toString();
        resultItem.append(score);
        return score;
    }

    const createUriLink = () => {
        const uri = document.createElement("div");
        const a = document.createElement('a');
        a.href = result.uri;
        a.textContent = result.uri;
        uri.innerHTML = '';
        uri.appendChild(a);
        return uri;
    }

    const createCacheLink = () => {
        const cached = document.createElement("div");
        const safeTarget = encodeURIComponent(result.uri);
        const link = `/cached/${encodeURIComponent(collectionName)}?url=${safeTarget}`;
        const a = document.createElement("a");
        a.href = link;
        a.textContent = "cached";
        cached.appendChild(a);
        return cached;
    }

    const resultItem = document.createElement("li");
    resultItem.classList.add("result");
    resultItem.append(
        createTitle(),
        createDesc(),
        createTermCount(),
        createScore(),
        createUriLink(),
        createCacheLink());

    return resultItem;
}

function render(
    state: StoreState,
    collectionSwitchHandler: EventListener,
    queryChangeHandler: EventListener) {

    renderCollections(state, collectionSwitchHandler);

    // TODO - recreate this? otherwise it's just readded
    const search = document.querySelector<HTMLInputElement>("#search")!
    search.value = state.query;
    search.addEventListener("input", queryChangeHandler);

    document.querySelector<HTMLInputElement>("#max_results")!
        .value = (state.maxResults ?? 10).toString();

    document
        .querySelector<HTMLElement>('#result_wrapper')!
        .replaceChildren(renderResults(state));
}

const changeCollection = async (state: StoreState, collection: string) => {
    state.selectedCollection = collection;
    await newQuery(state);
}

const changeResults = async (state: StoreState, results: QueryResults) => {
    state.results = results;
    document
        .querySelector<HTMLElement>('#result_wrapper')!
        .replaceChildren(renderResults(state));
}

const newQuery = async (state: StoreState) => {

    if (!state.selectedCollection || state.query.trim().length == 0 || state.maxResults < 1) {
        console.log("early abort" + state.selectedCollection + " " + state.query.trim() + " " + state.maxResults);
        return;
    }

    const queryResults = await fireSearch(state.selectedCollection, state.query, state.maxResults);

    changeResults(state, queryResults)
}

const reqHeaders: RequestInit = {
    cache: 'no-cache',
    headers: { 'Content-Type': 'application/json' }
}

const fireCollections = async (): Promise<string[]> =>
    await fetch("/collection", reqHeaders)
        .then(resp => resp.json())

const fireSearch =
    async (collectionName: string, query: string, maxResults: number): Promise<QueryResults> => {
        console.log("Firinsg search"); // Why not?
        const params = new URLSearchParams();
        params.append('q', query);
        params.append('n', maxResults.toString());
        const url = `/query/${encodeURIComponent(collectionName)}?${params.toString()}`;
        return await fetch(url, reqHeaders).then(resp => resp.json());
    }

const getState = async (): Promise<StoreState> => {
    var state = restoreStoredState();
    if (!state) {
        state = await freshState();
        storeState(state);
    }
    return state;
}

const init = async () => {

    const state = await getState();

    const collectionSwitchHandler = async (e: Event) =>
        await changeCollection(state, (e.target as HTMLInputElement).value);

    const queryChangeHandler = async (e: Event) => {
        console.log("Query change handler");
        await newQuery(state);
    }

    render(state, collectionSwitchHandler, queryChangeHandler);
}

init();

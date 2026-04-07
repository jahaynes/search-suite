
const SESSION_KEY_STORED_STATE = 'stored_state';

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
    selectedCollection: "one",
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

const restoreStoredState = (): StoreState | null => {
    const strStoredState = sessionStorage.getItem(SESSION_KEY_STORED_STATE);
    if (strStoredState) {
        return JSON.parse(strStoredState);
    }
    return null;
}

const renderCollections = async (state: StoreState) => {

    const collections = document.getElementById("collections")!;
    collections.innerHTML = '';

    const legend = document.createElement("legend");
    legend.innerText = "Search collection:";
    collections.append(legend);

    for (const collectionName of state.collections) {

        if (!state.selectedCollection) {
            state.selectedCollection = collectionName;
        }

        const radioName = "radio" + collectionName;

        const radio = document.createElement("input");
        radio.setAttribute("id", radioName);
        radio.setAttribute("type", "radio");
        radio.setAttribute("name", "collectionName");
        radio.setAttribute("value", collectionName)

        if (state.selectedCollection === collectionName) {
            radio.setAttribute("checked", "checked");
        }

        const label = document.createElement("label");
        label.setAttribute("for", radioName);
        label.innerText = collectionName;

        /*radio.addEventListener("change", async (e: Event) => {
            const target = e.target as HTMLInputElement;
            state.selectedCollection = target.value;
            await fireSearch(state);
        });*/

        collections.append(radio);
        collections.append(label);
    }
}

function renderQuery(state: StoreState) {
    const query = document.querySelector<HTMLInputElement>("#search")!;
    query.value = state.query;

    const maxResults = document.querySelector<HTMLInputElement>("#max_results")!;
    maxResults.value = (state.maxResults ?? 10).toString();
}

function renderResults(state: StoreState) {

    const resultList = document.querySelector<HTMLElement>("#results")!;
    resultList.innerHTML = '';

    if (state.selectedCollection) {
        console.log("making results");
        for (let i = 0; i < state.results.results.length; i++) {
            const resultItem = renderResult(state.selectedCollection, state.results.results[i]!);
            resultList.appendChild(resultItem);
            console.log("Appending: " + JSON.stringify(resultItem));
        }
    }
}

function renderResult(collectionName: string, result: QueryResult) {

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

function render(storedState: StoreState) {

    renderCollections(storedState);

    renderQuery(storedState);

    renderResults(storedState);
}

const init = async () => {

    // Restore any saved state
    const state = restoreStoredState() ?? fakeState;

    // Re-render any saved state prior to the event-listeners being attached
    render(state);

    // Attach event listeners

    // Done?
}

init();


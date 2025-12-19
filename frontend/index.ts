
type QueryUIState = {
    selectedCollection: string | null;
    query: string | null;
    maxresults: number;
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

const reqHeaders: RequestInit = {
    cache: 'no-cache',
    headers: { 'Content-Type': 'application/json' }
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

const fireSearch = async (state: QueryUIState) => {

    const collectionName: string | null = state.selectedCollection;
    const query: string | null = state.query;
    const maxResults: number = state.maxresults;

    if (!collectionName || !query || query.trim().length == 0 || maxResults < 1) {
        return;
    }

    const params = new URLSearchParams();
    params.append('q', query);
    params.append('n', maxResults.toString());
    const url = `/query/${encodeURIComponent(collectionName)}?${params.toString()}`;

    await fetch(url, reqHeaders)
        .then(resp => resp.json())
        .then((data: QueryResults) => {
            const resultList = document.querySelector<HTMLElement>("#results")!;
            resultList.innerHTML = '';
            for (let i = 0; i < data.results.length; i++) {
                const resultItem = renderResult(collectionName, data.results[i]!);
                resultList.appendChild(resultItem);
            }
        });
}

const displayCollections = async (state: QueryUIState, collectionNames: string[]) => {

    const collections = document.getElementById("collections")!;
    collections.innerHTML = '';

    const legend = document.createElement("legend");
    legend.innerText = "Search collection:";
    collections.append(legend);

    for (const collectionName of collectionNames) {

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

        radio.addEventListener("change", async (e: Event) => {
            const target = e.target as HTMLInputElement;
            state.selectedCollection = target.value;
            await fireSearch(state);
        });

        collections.append(radio);
        collections.append(label);
    }
}

const onLoad = async (ev: Event) => {

    const params = new URLSearchParams(window.location.search);

    const p_maxresults = params.get('n');
    const state: QueryUIState =
    {
        selectedCollection: params.get('c'),
        query: params.get('q'),
        maxresults: p_maxresults ? parseInt(p_maxresults) : 10
    }

    const searchInput = document.getElementById("search")! as HTMLInputElement;
    searchInput.value = state.query ?? "";
    searchInput
        .addEventListener("input", (e: Event) => {
            const target = e.target as HTMLInputElement;
            state.query = target.value;
            fireSearch(state);
        });

    const maxResultsInput = document.getElementById("max_results")! as HTMLInputElement;
    maxResultsInput.value = state.maxresults.toString();
    maxResultsInput.addEventListener("input", (e: Event) => {
        const target = e.target as HTMLInputElement;
        state.maxresults = parseInt(target.value) || 10;
        fireSearch(state);
    });

    await fetch("/collection", reqHeaders)
        .then(resp => resp.json())
        .then(cns => displayCollections(state, cns));

    await fireSearch(state);
}

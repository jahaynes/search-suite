type Either<L, R> = { Left: L } | { Right: R };

type QueryUIState = {
    selectedCollection: string | null;
};

type UnscoredResults = {
    num_unscored: number;
    unscored_results: UnscoredResult[];
};

type UnscoredResult = {
    ur_doc_id: number;
    ur_uri: string;
};

const reqHeaders: RequestInit = {
    cache: 'no-cache',
    headers: { 'Content-Type': 'application/json' }
}

function renderResult(collectionName: string, result: UnscoredResult) {

    const createUriLink = () => {
        const uri = document.createElement("div");
        const a = document.createElement('a');
        a.href = result.ur_uri;
        a.textContent = result.ur_uri;
        uri.innerHTML = '';
        uri.appendChild(a);
        return uri;
    }

    const createCacheLink = () => {
        const cached = document.createElement("div");
        const safeTarget = encodeURIComponent(result.ur_uri);
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
        createUriLink(),
        createCacheLink());

    return resultItem;
}

const fireStructuredSearch = async (state: QueryUIState) => {

    const collectionName: string | null =
        state.selectedCollection;

    const query: string =
        document.querySelector<HTMLInputElement>('#structured_search')?.value ?? "";

    if (!collectionName || query.trim().length == 0) {
        return;
    }

    const url = `/structured-query/${encodeURIComponent(collectionName)}`;

    await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'text/plain;charset=utf-8' },
        body: query
    })
    .then(resp => resp.json())
    .then((data: Either<string, UnscoredResults>) => {
        const resultList = document.querySelector<HTMLElement>("#results")!;
        resultList.innerHTML = '';

        if ('Right' in data) {
            for (let i = 0; i < data.Right.num_unscored; i++) {
                const resultItem = renderResult(collectionName, data.Right.unscored_results[i]!);
                resultList.appendChild(resultItem);
            }
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

        const radioName = "radio" + collectionName;

        const radio = document.createElement("input");
        radio.setAttribute("id", radioName);
        radio.setAttribute("type", "radio");
        radio.setAttribute("name", "collectionName");
        radio.setAttribute("value", collectionName)

        const label = document.createElement("label");
        label.setAttribute("for", radioName);
        label.innerText = collectionName;

        radio.addEventListener("change", async (e: Event) => {
            const target = e.target as HTMLInputElement;
            state.selectedCollection = target.value;
            await fireStructuredSearch(state);
        });

        collections.append(radio);
        collections.append(label);
    }
}

const onLoad = async (ev: Event) => {

    const state =
        { selectedCollection: null };

    document
        .getElementById("structured_search")!
        .addEventListener("input",
            () => fireStructuredSearch(state));

    const url = "/collection"
    await fetch(url, reqHeaders)
        .then(resp => resp.json())
        .then(cns => displayCollections(state, cns));
}

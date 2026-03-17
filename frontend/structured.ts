
import type { Either } from './common.js';
import { gatherMapErrors } from './common.js';

type QueryUIState = {
    selectedCollections: Set<string>;
};

type UnscoredResults = {
    num_unscored: number;
    unscored_results: UnscoredResult[];
};

type UnscoredResult = {
    ur_doc_id: number;
    ur_uri: string;
    ur_metadata: Record<string, string>;
};

enum Mode {
    File,
    Uri
}

type Debounce = {
    currentSearchId: number
};

const reqHeaders: RequestInit = {
    cache: 'no-cache',
    headers: { 'Content-Type': 'application/json' }
}

function renderResult(collectionName: string, result: UnscoredResult) {

    const createTitle = (mode: Mode) => {
        const title = document.createElement("div");
        const a = document.createElement('a');
        switch (mode) {
            case Mode.File:
                const safeTarget = encodeURIComponent(result.ur_uri);
                a.href = `/cached/${encodeURIComponent(collectionName)}?url=${safeTarget}`;
                break;
            case Mode.Uri:
                a.href = result.ur_uri;
                break;
        }
        a.textContent = result.ur_metadata['title'] ?? 'Untitled';  // TODO
        title.appendChild(a);
        return title;
    }

    const renderFileResult = () => {

        const resultItem = document.createElement("li");
        resultItem.classList.add("result");

        const title = createTitle(Mode.File);

        const filePath = document.createElement("small");
        // Nasty way to strip off file://hostname
        filePath.textContent = result.ur_uri.slice(result.ur_uri.lastIndexOf('//') + 1);

        resultItem.appendChild(title);
        resultItem.appendChild(filePath);

        return resultItem;
    }

    const renderUriResult = () => {
        const resultItem = document.createElement("li");
        resultItem.classList.add("result");
        resultItem.appendChild(createTitle(Mode.Uri));
        return resultItem;
    }

    if (result.ur_uri.startsWith("file://")) {
        return renderFileResult();
    } else {
        return renderUriResult();
    }
}

const fireStructuredSearch = async (state: QueryUIState) => {

    const fireStructuredSearchCollection = async (collectionName: string, query: string): Promise<Either<string, UnscoredResults>> => {
        const url = `/structured-query/${encodeURIComponent(collectionName)}`;
        return await fetch(url, {
            method: 'POST',
            headers: { 'Content-Type': 'text/plain;charset=utf-8' },
            body: query
        }).then(resp => resp.json());
    }

    const query: string =
        document.querySelector<HTMLInputElement>('#structured_search')?.value ?? "";

    if (state.selectedCollections.size == 0) {
        return { Left: ["No collections selected"] };
    }

    // Fetch the search results
    const collectionsResults = new Map<string, Either<string, UnscoredResults>>(
        await Promise.all([...state.selectedCollections]
            .map(async name => [name
                , await fireStructuredSearchCollection(name, query)
            ] as const)));

    return gatherMapErrors(collectionsResults);
}

const getUIState = (): QueryUIState => {
    const selectedCollections =
        new Set(Array.from(
            document.querySelectorAll<HTMLInputElement>('.collection-selector'))
            .filter(cb => cb.checked)
            .map(cb => cb.value));
    return { selectedCollections: selectedCollections };
}

const displayCollections = async (debounce: Debounce, collectionNames: string[]) => {

    const collections = document.getElementById("collections")!;
    collections.innerHTML = '';

    const legend = document.createElement("legend");
    legend.textContent = "Include collections:";
    collections.append(legend);

    for (const collectionName of collectionNames) {

        const collectionDiv = document.createElement("div");

        const checkboxName = "checkbox_" + collectionName;

        const checkbox = document.createElement("input");
        checkbox.setAttribute("id", checkboxName);
        checkbox.setAttribute("type", "checkbox");
        checkbox.setAttribute("name", "collectionName");
        checkbox.setAttribute("value", collectionName)
        checkbox.classList.add("collection-selector");

        const label = document.createElement("label");
        label.setAttribute("for", checkboxName);
        label.textContent = collectionName;

        checkbox.addEventListener("change", async (_: Event) => await runSearch(debounce));
        collectionDiv.append(checkbox);
        collectionDiv.append(label);
        collections.append(collectionDiv);
    }
}

const runSearch = async (debounce: Debounce) => {

    // Prepare debouncing
    const resultDebounce = document.getElementById('result_debounce') as HTMLDivElement;
    resultDebounce.innerHTML = '';

    const newSearchId = Math.random();
    debounce.currentSearchId = newSearchId;

    // Clear UI
    const resultColumns = document.createElement('div');
    resultColumns.id = 'result-columns';
    resultColumns.classList.add('grid');

    // Determine the selected collections
    const state = getUIState();

    // Fire search
    const collectionResponses = await fireStructuredSearch(state);

    // Populate UI
    // TODO ('Left')
    if ('Right' in collectionResponses) {
        for (const [collectionName, results] of collectionResponses.Right.entries()) {
            const columnDiv = document.createElement("div");
            const columnHead = document.createElement("p");
            columnHead.textContent = collectionName;
            columnDiv.appendChild(columnHead);
            const resultColumn = document.createElement("ol");
            resultColumn.id = 'result_columns_' + collectionName;
            for (const result of results.unscored_results) {
                resultColumn.appendChild(renderResult(collectionName, result));
            }
            columnDiv.appendChild(resultColumn);
            resultColumns.appendChild(columnDiv);
        }

        if (newSearchId === debounce.currentSearchId) {
            resultDebounce.appendChild(resultColumns);
        }
    }
}

const init = async () => {

    const debounce = { currentSearchId: Math.random() }

    document
        .getElementById("structured_search")!
        .addEventListener("input", async () => await runSearch(debounce));

    await fetch("/collection", reqHeaders)
        .then(resp => resp.json())
        .then(cns => displayCollections(debounce, cns));

    await runSearch(debounce);
}

init();

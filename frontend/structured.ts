import type { Either } from './common.js';
import { gatherMapErrors } from './common.js';

type QueryUIState = {
    selectedCollections: Set<string>;
    query: string;
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

const SESSION_KEY_QUERY = 'structured_query';
const SESSION_KEY_SELECTED_COLLECTIONS = 'structured_selected_collections';
const SESSION_KEY_RESULTS = 'structured_results';

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
        a.textContent = result.ur_metadata['title'] ?? 'Untitled';
        title.appendChild(a);
        return title;
    }

    const renderFileResult = () => {
        const resultItem = document.createElement("li");
        resultItem.classList.add("result");
        const title = createTitle(Mode.File);
        const filePath = document.createElement("small");
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

    if (state.selectedCollections.size == 0) {
        return { Left: ["No collections selected"] };
    }

    const collectionsResults = new Map<string, Either<string, UnscoredResults>>(
        await Promise.all([...state.selectedCollections]
            .map(async name => [name
                , await fireStructuredSearchCollection(name, state.query)
            ] as const)));

    return gatherMapErrors(collectionsResults);
}

const getUIState = (): QueryUIState => {
    const queryInput = document.querySelector<HTMLTextAreaElement>('#structured_search');
    const selectedCollections =
        new Set(Array.from(
            document.querySelectorAll<HTMLInputElement>('.collection-selector'))
            .filter(cb => cb.checked)
            .map(cb => cb.value));
    return {
        selectedCollections: selectedCollections,
        query: queryInput?.value ?? ""
    };
}

const saveUIState = () => {
    const state = getUIState();
    sessionStorage.setItem(SESSION_KEY_QUERY, state.query);
    sessionStorage.setItem(SESSION_KEY_SELECTED_COLLECTIONS, JSON.stringify([...state.selectedCollections]));
    console.log('[StructuredSearch] Saved state:', { query: state.query, collections: [...state.selectedCollections] });
}

const saveResults = (results: Map<string, UnscoredResults>) => {
    const serializable: [string, UnscoredResults][] = [];
    results.forEach((value, key) => {
        serializable.push([key, value]);
    });
    sessionStorage.setItem(SESSION_KEY_RESULTS, JSON.stringify(serializable));
    console.log('[StructuredSearch] Saved results for', results.size, 'collections');
}

const restoreResults = (): Map<string, UnscoredResults> | null => {
    const saved = sessionStorage.getItem(SESSION_KEY_RESULTS);
    console.log('[StructuredSearch] restoreResults, saved data exists:', saved !== null);
    if (saved) {
        try {
            const parsed: [string, UnscoredResults][] = JSON.parse(saved);
            console.log('[StructuredSearch] Parsed results:', parsed.length, 'entries');
            return new Map(parsed);
        } catch (e) {
            console.error('[StructuredSearch] Failed to parse results:', e);
            return null;
        }
    }
    return null;
}

const restoreUIState = () => {
    const query = sessionStorage.getItem(SESSION_KEY_QUERY);
    const collectionsStr = sessionStorage.getItem(SESSION_KEY_SELECTED_COLLECTIONS);
    let collections: string[] | null = null;
    if (collectionsStr) {
        try {
            collections = JSON.parse(collectionsStr);
        } catch {
            // ignore
        }
    }
    console.log('[StructuredSearch] restoreUIState:', { query, collections });
    return { query, selectedCollections: collections };
}

let debounce: Debounce | null = null;
let collectionsInitialized = false;

const displayCollections = (collectionNames: string[], restoredSelections: string[] | null) => {
    if (collectionsInitialized) {
        console.log('[StructuredSearch] displayCollections skipped - already initialized');
        return;
    }
    collectionsInitialized = true;
    console.log('[StructuredSearch] displayCollections, initializing with', collectionNames.length, 'collections');

    const collections = document.getElementById("collections")!;
    collections.innerHTML = '';

    const legend = document.createElement("legend");
    legend.textContent = "Include collections:";
    collections.appendChild(legend);

    for (const collectionName of collectionNames) {
        const collectionDiv = document.createElement("div");
        const checkboxName = "checkbox_" + collectionName;

        const checkbox = document.createElement("input");
        checkbox.setAttribute("id", checkboxName);
        checkbox.setAttribute("type", "checkbox");
        checkbox.setAttribute("name", "collectionName");
        checkbox.setAttribute("value", collectionName)
        checkbox.classList.add("collection-selector");

        if (restoredSelections && restoredSelections.includes(collectionName)) {
            checkbox.checked = true;
            console.log('[StructuredSearch] Restored selection:', collectionName);
        }

        const label = document.createElement("label");
        label.setAttribute("for", checkboxName);
        label.textContent = collectionName;

        checkbox.addEventListener("change", () => {
            saveUIState();
            runSearch();
        });
        collectionDiv.appendChild(checkbox);
        collectionDiv.appendChild(label);
        collections.appendChild(collectionDiv);
    }
}

const displayResults = (collectionResponses: Map<string, UnscoredResults>) => {
    console.log('[StructuredSearch] displayResults called with', collectionResponses.size, 'collections');
    
    const resultDebounce = document.getElementById('result_debounce') as HTMLDivElement;
    resultDebounce.innerHTML = '';

    const resultColumns = document.createElement('div');
    resultColumns.id = 'result-columns';
    resultColumns.classList.add('grid');

    let totalResults = 0;
    for (const [collectionName, results] of collectionResponses.entries()) {
        totalResults += results.unscored_results.length;
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
    
    console.log('[StructuredSearch] Displaying', totalResults, 'total results');
    resultDebounce.appendChild(resultColumns);
}

const runSearch = async () => {
    if (!debounce) {
        console.log('[StructuredSearch] runSearch skipped - debounce not initialized');
        return;
    }

    const newSearchId = Math.random();
    debounce.currentSearchId = newSearchId;

    const state = getUIState();
    console.log('[StructuredSearch] runSearch, query:', state.query, 'collections:', [...state.selectedCollections]);
    
    const collectionResponses = await fireStructuredSearch(state);

    if (newSearchId !== debounce.currentSearchId) {
        console.log('[StructuredSearch] runSearch superseded');
        return;
    }

    if ('Right' in collectionResponses) {
        const resultsMap = collectionResponses.Right;
        saveResults(resultsMap);
        displayResults(resultsMap);
    }
}

const init = (restoredState: { query: string | null; selectedCollections: string[] | null; results: Map<string, UnscoredResults> | null }) => {
    console.log('[StructuredSearch] init called with:', {
        query: restoredState.query ? restoredState.query.substring(0, 50) + '...' : null,
        collections: restoredState.selectedCollections,
        hasResults: restoredState.results !== null
    });
    
    debounce = { currentSearchId: Math.random() };

    const searchInput = document.getElementById("structured_search") as HTMLTextAreaElement;

    if (restoredState.query) {
        searchInput.value = restoredState.query;
        console.log('[StructuredSearch] Restored query to input');
    }

    searchInput.addEventListener("input", () => {
        saveUIState();
        runSearch();
    });

    fetch("/collection", reqHeaders)
        .then(resp => resp.json())
        .then((cns: string[]) => {
            displayCollections(cns, restoredState.selectedCollections);
            
            if (restoredState.results) {
                console.log('[StructuredSearch] Showing cached results');
                displayResults(restoredState.results);
            } else {
                console.log('[StructuredSearch] No cached results, running search');
                runSearch();
            }
        });
}

console.log('[StructuredSearch] Script loaded, checking sessionStorage...');

const savedState = restoreUIState();
const savedResults = restoreResults();

init({
    query: savedState.query,
    selectedCollections: savedState.selectedCollections,
    results: savedResults
});

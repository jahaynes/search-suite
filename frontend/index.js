
const reqHeaders = {
    cache: 'no-cache',
    headers: { 'Content-Type': 'application/json' }
}

function renderResult(collectionName, result) {

    const resultItem = document.createElement("li");
    resultItem.classList.add("result");

    const title = document.createElement("div");

    const metadata = result.metadata;
    if (metadata == null || metadata.title == null) {
        title.innerText = "Untitled";
    } else {
        title.innerText = metadata.title;
    }
    title.classList.add("result_title");
    resultItem.append(title);

    const desc = document.createElement("p");
    if (metadata == null || metadata.description == null) {
        desc.innerText = "No description";
    } else {
        desc.innerText = metadata.description;
    }
    desc.classList.add("result_desc");
    resultItem.append(desc);

    const termCount = document.createElement("div");
    termCount.innerText = result.term_count;
    resultItem.append(termCount);

    const score = document.createElement("div");
    score.innerText = result.score;
    resultItem.append(score);

    const uri = document.createElement("div");
    // TODO safer:
    uri.innerHTML = "<a href='" + result.uri + "'>" + result.uri + "</a>";
    resultItem.append(uri);

    const cached = document.createElement("div");
    // TODO escape?
    const link = "/cached/" + collectionName + "?url=" + result.uri;
    cached.innerHTML = "<a href='" + link + "'>cached</a>";
    resultItem.append(cached);

    return resultItem;
}

const fireSearch = async (state) => {

    const query =
        document.getElementById("search").value;

    const strMaxResults =
        parseInt(document.getElementById("max_results").value);

    const maxResults =
        strMaxResults == NaN ? 8 : strMaxResults;

    const collectionName =
        state.selectedCollection;

    if (!collectionName || query.trim().length == 0 || maxResults < 1) {
        return;
    }

    // TODO Escape here
    const url = "/query/" + collectionName + "?q=" + query + "&n=" + maxResults

    await fetch(url, reqHeaders)
        .then(resp => resp.json())
        .then(data => {
            const resultList = document.getElementById("results");
            resultList.innerHTML = '';
            for (i = 0; i < data.results.length; i++) {
                const resultItem = renderResult(collectionName, data.results[i]);
                resultList.appendChild(resultItem);
            }
        });
}

const displayCollections = async (state, collectionNames) => {

    const collections = document.getElementById("collections");
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

        radio.addEventListener("change", async (e) => {
            state.selectedCollection = e.target.value;
            await fireSearch(state);
        });

        collections.append(radio);
        collections.append(label);
    }
}

const onLoad = async () => {

    const state =
        {};

    document
        .getElementById("search")
        .addEventListener("input",
            () => fireSearch(state));

    document
        .getElementById("max_results")
        .addEventListener("input",
            () => fireSearch(state));

    const url = "/collection"
    await fetch(url, reqHeaders)
        .then(resp => resp.json())
        .then(cns => displayCollections(state, cns));
}


const reqHeaders = {
    cache: 'no-cache',
    headers: { 'Content-Type': 'application/json' }
}

function renderResult(collectionName, result) {

    const resultItem = document.createElement("li");
    resultItem.classList.add("result");

    const title = document.createElement("div");
    title.innerText = result.metadata.title == null ? "Untitled" : result.metadata.title;
    title.classList.add("result_title");
    resultItem.append(title);

    const desc = document.createElement("p");
    desc.innerText = result.metadata.description;
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

    const collectionName =
        state.selectedCollection;

    if (!collectionName) {
        return;
    }

    // TODO Escape here
    const url = "/query/" + collectionName + "?q=" + query + "&n=8"

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

    const search =
        document.getElementById("search");

    search.addEventListener("input",
        () => fireSearch(state));

    const url = "/collection"
    await fetch(url, reqHeaders)
        .then(resp => resp.json())
        .then(cns => displayCollections(state, cns));
}

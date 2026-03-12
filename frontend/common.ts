export type Either<L, R> = { Left: L } | { Right: R };

export function gatherMapErrors<K, V>(dict: Map<K, Either<string, V>>): Either<string[], Map<K, V>> {

    var lefts = [];
    var rights = new Map<K, V>();

    for (const [collection, results] of dict.entries()) {
        if ('Left' in results) {
            lefts.push(results.Left);
        }
        if ('Right' in results) {
            rights.set(collection, results.Right);
        }
    }

    return lefts.length > 0
        ? { Left: lefts }
        : { Right: rights };
}
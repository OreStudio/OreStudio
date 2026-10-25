/**
 * Completing a list request from the protocol it is sent to.
 *
 * A service decodes a list request strictly: every member the struct declares
 * must be present, `as_of` included even though it is optional, and a filter
 * must name every member of its filter struct. A payload that leaves one out is
 * refused as bad_request, and the screen that sent it shows an error instead of
 * rows. The same payload failing silently is worse: a reply parsed against the
 * wrong field yields an empty list and the screen looks merely empty.
 *
 * A caller in TypeScript states what it means -- a filter member or two, the
 * page, the order -- and this fills the rest from the generated table, so the
 * knowledge lives in one place that the generator keeps true.
 */
import { LIST_REQUESTS } from './generated/list_requests.js';

/**
 * The payload to send for a list request.
 *
 * An absent member is stated as null rather than left out, and a filter the
 * caller did not state is sent as null rather than omitted. A request that the
 * table does not describe is returned untouched: the table is generated from
 * the components, so a missing entry means a hand-written subject rather than a
 * reason to guess.
 */
export function completeListRequest(
    subject: string,
    body: Readonly<Record<string, unknown>>,
): Record<string, unknown> {
    const shape = LIST_REQUESTS[subject];
    if (shape === undefined) {
        return { ...body };
    }
    const completed: Record<string, unknown> = { ...body };
    if (shape.asOf && !('as_of' in completed)) {
        completed['as_of'] = null;
    }
    if (shape.filter.length > 0) {
        if (!('filter' in completed)) {
            completed['filter'] = null;
        } else if (isRecord(completed['filter'])) {
            const filter = { ...completed['filter'] };
            for (const member of shape.filter) {
                if (!(member in filter)) {
                    filter[member] = null;
                }
            }
            completed['filter'] = filter;
        }
    }
    return completed;
}

/** Whether a value is an object that states members, rather than null or a scalar. */
function isRecord(value: unknown): value is Readonly<Record<string, unknown>> {
    return typeof value === 'object' && value !== null && !Array.isArray(value);
}

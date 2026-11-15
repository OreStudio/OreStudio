// SPDX-License-Identifier: GPL-3.0-or-later
// Administrative test-data setup only. Never registered as an agent/MCP tool.
import { isDeepStrictEqual } from 'node:util';
import { fxDataset } from '../evals/fx-e2e-dataset.mjs';
const intent = {
    reason_code: 'system.new_record',
    commentary:
        'Create isolated agent-e2e-fx-curve-v1 synthetic validation data; do not overwrite existing records.',
};
const change = (write) => ({ write, precondition: { kind: 'must_not_exist', version: null } });
const field = (row, key) =>
    key === 'observation_datetime'
        ? String(row[key])
              .replace(' ', 'T')
              .replace(/(?:\.0+)?Z$/, 'Z')
        : row[key];
function matches(row, expected) {
    return (
        row &&
        Object.keys(expected).every((k) => isDeepStrictEqual(field(row, k), field(expected, k)))
    );
}
export async function loadFxDataset({
    tenantId,
    partyId,
    systemRequest,
    tenantRequest,
    apply = false,
    dataset = fxDataset(tenantId, partyId),
}) {
    if (dataset.tenantId !== tenantId || dataset.partyId !== partyId)
        throw Error('Dataset owner does not match the selected test identity.');
    const resources = [
        {
            kind: 'provider',
            request: systemRequest,
            get: 'marketdata.v1.providers.get',
            put: 'marketdata.v1.providers.put',
            key: { code: dataset.provider.code },
            property: 'provider',
            write: dataset.provider,
        },
        ...dataset.series.map((write) => ({
            kind: 'series',
            request: tenantRequest,
            get: 'marketdata.v1.market_series.get',
            put: 'marketdata.v1.market_series.put',
            key: { id: write.id },
            property: 'market_series',
            write,
        })),
        ...dataset.observations.map((write) => ({
            kind: 'observation',
            request: tenantRequest,
            get: 'marketdata.v1.market_observations.get',
            key: { id: write.id },
            property: 'market_observation',
            write,
        })),
    ];
    const checked = async (item) => {
        const r = await item.request(item.get, { key: item.key });
        if (r.result?.outcome === 'missing') return false;
        if (r.result?.outcome !== 'ok')
            throw Error(`${item.kind} read failed: ${r.result?.code ?? 'invalid_response'}`);
        const row = r[item.property];
        if (!matches(row, item.write) || (item.kind !== 'provider' && row.tenant_id !== tenantId))
            throw Error(
                `Existing ${item.kind} conflicts with the isolated dataset; nothing will be overwritten.`,
            );
        return true;
    };
    // Complete preflight before any writes, including all pre-existing observations.
    const existing = [];
    for (const item of resources) existing.push(await checked(item));
    const missing = resources.filter((_, i) => !existing[i]);
    if (!apply)
        return {
            version: dataset.version,
            mode: 'plan',
            missing: missing.map((r) => ({ kind: r.kind, key: r.key })),
            existing: resources.length - missing.length,
        };
    const perform = async (subject, payload, items, request) => {
        try {
            const r = await request(subject, payload);
            if (r.result?.outcome !== 'ok')
                throw Error(
                    `${subject} refused: ${r.result?.code ?? 'invalid_response'} ${r.result?.message ?? ''}`,
                );
        } catch (error) {
            // Reconcile uncertain outcomes by reading exact keys; never blind-retry writes.
            for (const item of items) if (!(await checked(item))) throw error;
        }
        for (const item of items)
            if (!(await checked(item))) throw Error(`Written ${item.kind} could not be read back.`);
    };
    for (const item of missing.filter((r) => r.kind !== 'observation'))
        await perform(item.put, { change: change(item.write), intent }, [item], item.request);
    const observations = missing.filter((r) => r.kind === 'observation');
    if (observations.length)
        await perform(
            'marketdata.v1.market_observations.put_many',
            { changes: observations.map((r) => change(r.write)), intent },
            observations,
            tenantRequest,
        );
    return {
        version: dataset.version,
        mode: 'applied',
        created: missing.length,
        reused: resources.length - missing.length,
        provider: dataset.provider.code,
        series: dataset.series.map((s) => ({ id: s.id, uri: s.oresmd_uri })),
        observations: dataset.observations.length,
    };
}

// SPDX-License-Identifier: GPL-3.0-or-later
import { fixtureBackend, id } from './fixtures.mjs';

export function bookingDraftFixtureBackend(scenario) {
    const backend = fixtureBackend(scenario);
    const counterparties = [
        { id: id(60), tenant_id: id(1), short_code: 'FIX-A', full_name: 'Fixture Bank', status: 'active' },
        { id: id(61), tenant_id: id(1), short_code: 'FIX-B', full_name: 'Fixture Bank', status: 'active' },
    ];
    return {
        ...backend,
        async request(op, args, session, signal) {
            signal?.throwIfAborted();
            if (op === 'counterparty') {
                const row = counterparties.find(item => item.id === args.key.id || item.short_code === args.key.short_code);
                return row ? { result: { outcome: 'ok' }, counterparty: row } : { result: { outcome: 'missing' } };
            }
            if (op === 'counterparties')
                return { result: { outcome: 'ok' }, total: counterparties.length, counterparties };
            return backend.request(op, args, session, signal);
        },
    };
}

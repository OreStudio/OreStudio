/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import type { AuthenticatedCaller } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import {
    readStoreBounds,
    readStoreSamples,
    saveStoreBound,
    type StoreBound,
} from './housekeeping.js';

/**
 * The housekeeping reads and the one write: the subject and body each sends, how
 * a store that nothing bounds reaches a screen, and what the write composes from
 * the store rather than from its caller.
 *
 * The bounds read is the database's own answer, so the fixtures state its rows as
 * the wire states them, including the absent values a nullable column sends as nil.
 */

const OK = { outcome: 'ok', code: '', message: '' };

function wireStore(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        code: 'marketdata.delivery_counts',
        display_name: 'Market data delivery counts',
        description: 'How many items each feed delivered.',
        owner_component: 'marketdata',
        target_kind: 'table',
        target: 'public.ores_marketdata_delivery_counts_tbl',
        time_column: 'period_start',
        measure_kind: 'relation_rows',
        bound_kind: 'policy',
        bound: {
            action: 'delete_rows',
            retention_days: 30,
            cadence_hours: 24,
            policy_code: 'delivery_counts_30d',
            policy_enabled: true,
            policy_version: 7,
            policy_description: 'A delivery counter is read for a month',
            mechanism: 'sweep',
            bounded_now: true,
        },
        last_run: {
            outcome: 'applied',
            ran_at: '2026-11-27 03:00:00.000000Z',
            removed: 2,
            detail: '',
        },
        size: {
            size_bytes: 32768,
            measure_count: 27,
            sampled_at: '2026-11-27 03:15:00.000000Z',
        },
        ...overrides,
    };
}

function callerAnswering(answers: Record<string, unknown>) {
    const calls: { subject: string; body: unknown }[] = [];
    const caller: AuthenticatedCaller = {
        async callAuthenticated(subject, body, schema) {
            calls.push({ subject, body });
            return schema.parse(answers[subject]);
        },
    };
    return { caller, calls };
}

describe('readStoreBounds', () => {
    it('asks the bounds read for every store and answers the rows it was given', async () => {
        const { caller, calls } = callerAnswering({
            'scheduler.v1.store_bounds.list': {
                success: true,
                message: '',
                stores: [wireStore()],
            },
        });

        const stores = await readStoreBounds(caller);

        expect(calls).toEqual([{ subject: 'scheduler.v1.store_bounds.list', body: {} }]);
        expect(stores).toHaveLength(1);
        expect(stores[0]?.code).toBe('marketdata.delivery_counts');
        expect(stores[0]?.bound.policy_description).toBe('A delivery counter is read for a month');
    });

    it('reads an absent size as no measurement rather than as zero bytes measured', async () => {
        const { caller } = callerAnswering({
            'scheduler.v1.store_bounds.list': {
                success: true,
                stores: [
                    wireStore({
                        code: 'compute.job_inputs',
                        size: { size_bytes: 0, measure_count: 0, sampled_at: null },
                    }),
                ],
            },
        });

        const stores = await readStoreBounds(caller);

        expect(stores[0]?.size.sampled_at).toBeNull();
        expect(stores[0]?.size.size_bytes).toBe(0);
    });

    it('refuses a read the server refused rather than answering an empty catalogue', async () => {
        const { caller } = callerAnswering({
            'scheduler.v1.store_bounds.list': {
                success: false,
                message: 'Your role cannot read the scheduler.',
                stores: [],
            },
        });

        await expect(readStoreBounds(caller)).rejects.toBeInstanceOf(OperationFailedError);
    });
});

describe('readStoreSamples', () => {
    it('names the store and the horizon, and answers the window it covered', async () => {
        const { caller, calls } = callerAnswering({
            'scheduler.v1.store_samples.list': {
                success: true,
                message: '',
                from_at: '2026-11-27 00:00:00.000000Z',
                to_at: '2026-11-27 04:00:00.000000Z',
                samples: [
                    {
                        sampled_at: '2026-11-27 02:15:00.000000Z',
                        size_bytes: 31000,
                        measure_count: 24,
                    },
                    {
                        sampled_at: '2026-11-27 03:15:00.000000Z',
                        size_bytes: 32768,
                        measure_count: 27,
                    },
                ],
            },
        });

        const series = await readStoreSamples(caller, 'marketdata.delivery_counts', '24h');

        expect(calls[0]?.body).toEqual({ store_code: 'marketdata.delivery_counts', range: '24h' });
        expect(series.samples.map((sample) => sample.size_bytes)).toEqual([31000, 32768]);
        expect(series.fromAt).toBe('2026-11-27 00:00:00.000000Z');
        expect(series.toAt).toBe('2026-11-27 04:00:00.000000Z');
    });
});

describe('saveStoreBound', () => {
    function wired(): StoreBound {
        return {
            code: 'marketdata.delivery_counts',
            display_name: 'Market data delivery counts',
            description: 'How many items each feed delivered.',
            owner_component: 'marketdata',
            target_kind: 'table',
            target: 'public.ores_marketdata_delivery_counts_tbl',
            time_column: 'period_start',
            measure_kind: 'relation_rows',
            bound_kind: 'policy',
            bound: {
                action: 'delete_rows',
                retention_days: 30,
                cadence_hours: 24,
                policy_code: 'delivery_counts_30d',
                policy_enabled: true,
                policy_version: 7,
                policy_description: 'A delivery counter is read for a month',
                mechanism: 'sweep',
                bounded_now: true,
            },
            last_run: {
                outcome: 'applied',
                ran_at: '2026-11-27 03:00:00.000000Z',
                removed: 2,
                detail: '',
            },
            size: {
                size_bytes: 32768,
                measure_count: 27,
                sampled_at: '2026-11-27 03:15:00.000000Z',
            },
        };
    }

    it('sends the store’s own target and action beside the three numbers a person decided', async () => {
        const { caller, calls } = callerAnswering({
            'scheduler.v1.retention_policies.put': { result: OK },
        });

        const outcome = await saveStoreBound(
            caller,
            wired(),
            { enabled: false, retentionDays: 90, cadenceHours: 12 },
            { reasonCode: 'common.correction', commentary: 'the audit asked for a quarter' },
        );

        expect(outcome).toEqual({ done: true });
        expect(calls[0]?.body).toEqual({
            change: {
                write: {
                    code: 'delivery_counts_30d',
                    target_kind: 'table',
                    target: 'public.ores_marketdata_delivery_counts_tbl',
                    store_code: 'marketdata.delivery_counts',
                    action: 'delete_rows',
                    retention_days: 90,
                    cadence_hours: 12,
                    enabled: false,
                    description: 'A delivery counter is read for a month',
                },
                precondition: { kind: 'must_match_version', version: 7 },
            },
            intent: {
                reason_code: 'common.correction',
                commentary: 'the audit asked for a quarter',
            },
        });
    });

    it('claims nothing exists when no policy bounds the store, and names it by the store', async () => {
        const unbounded = wired();
        const change = {
            ...unbounded,
            bound: {
                ...unbounded.bound,
                policy_code: null,
                policy_enabled: false,
                policy_version: 0,
                mechanism: 'none',
                bounded_now: false,
            },
        };
        const { caller, calls } = callerAnswering({
            'scheduler.v1.retention_policies.put': { result: OK },
        });

        await saveStoreBound(
            caller,
            change,
            { enabled: true, retentionDays: 30, cadenceHours: 24 },
            { reasonCode: 'common.correction', commentary: '' },
        );

        const body = calls[0]?.body as {
            change: { write: { code: string }; precondition: unknown };
        };
        expect(body.change.write.code).toBe('marketdata.delivery_counts');
        expect(body.change.precondition).toEqual({ kind: 'must_not_exist', version: null });
    });

    it('reports a refusal as an outcome rather than as a success', async () => {
        const { caller } = callerAnswering({
            'scheduler.v1.retention_policies.put': {
                result: { outcome: 'conflict', code: '', message: 'The policy moved on.' },
            },
        });

        const outcome = await saveStoreBound(
            caller,
            wired(),
            { enabled: true, retentionDays: 30, cadenceHours: 24 },
            { reasonCode: 'common.correction', commentary: '' },
        );

        expect(outcome).toEqual({
            done: false,
            outcome: 'conflict',
            message: 'The policy moved on.',
        });
    });

    it('refuses to write a bound to a store the catalogue states no action for', async () => {
        const unbounded = wired();
        const noAction = {
            ...unbounded,
            bound_kind: 'none',
            bound: { ...unbounded.bound, action: null, policy_code: null, policy_version: 0 },
        };
        const { caller, calls } = callerAnswering({});

        await expect(
            saveStoreBound(
                caller,
                noAction,
                { enabled: true, retentionDays: 30, cadenceHours: 24 },
                { reasonCode: 'common.correction', commentary: '' },
            ),
        ).rejects.toBeInstanceOf(OperationFailedError);
        expect(calls).toEqual([]);
    });
});

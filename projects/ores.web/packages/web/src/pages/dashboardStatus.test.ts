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
 */

import { describe, expect, it } from 'vitest';
import type { BusView, LoginInfo } from '@ores/wire-protocol/browser';
import { sparklinePath } from '../ui/Sparkline.js';
import {
    accountsWithFailedSignIns,
    installationVerdict,
    lockedAccounts,
    panelsNeedingAttention,
    passwordResetsDue,
    throughputSeries,
} from './dashboardStatus.js';

/**
 * The dashboard's arithmetic: the rate of the bus from its counter, the line
 * drawn through it, and how many panels want the person.
 */

function sample(at: string, inMsgs: number): BusView['samples'][number] {
    return {
        sampled_at: at,
        in_msgs: inMsgs,
        out_msgs: 0,
        in_bytes: 0,
        out_bytes: 0,
        connections: 1,
        mem_bytes: 0,
        slow_consumers: 0,
    };
}

function bus(samplesNewestFirst: BusView['samples']): BusView {
    return { sampled_at: null, samples: samplesNewestFirst, streams: [] };
}

describe('the throughput of the bus', () => {
    it('is the difference of the counter over the seconds between two samples, oldest first', () => {
        const series = throughputSeries(
            bus([
                sample('2026-10-10 12:02:00Z', 1_300),
                sample('2026-10-10 12:01:00Z', 1_000),
                sample('2026-10-10 12:00:00Z', 400),
            ]),
        );

        expect(series).toEqual([10, 5]);
    });

    it('leaves out a pair that goes backwards, which is the server having restarted', () => {
        const series = throughputSeries(
            bus([
                sample('2026-10-10 12:02:00Z', 120),
                sample('2026-10-10 12:01:00Z', 5_000),
                sample('2026-10-10 12:00:00Z', 4_000),
            ]),
        );

        expect(series).toEqual([1000 / 60]);
    });

    it('leaves out a pair with no time between them', () => {
        const series = throughputSeries(
            bus([sample('2026-10-10 12:00:00Z', 20), sample('2026-10-10 12:00:00Z', 10)]),
        );

        expect(series).toEqual([]);
    });

    it('has no rate from one sample', () => {
        expect(throughputSeries(bus([sample('2026-10-10 12:00:00Z', 20)]))).toEqual([]);
    });
});

describe('the line drawn through a series', () => {
    it('draws nothing for fewer than two points', () => {
        expect(sparklinePath([])).toBe('');
        expect(sparklinePath([3])).toBe('');
    });

    it('puts the lowest point at the foot and the highest at the top', () => {
        const path = sparklinePath([0, 10]);

        expect(path).toBe('M 0.0 37.0 L 300.0 3.0');
    });

    it('draws a flat series through the middle', () => {
        expect(sparklinePath([4, 4, 4])).toBe('M 0.0 20.0 L 150.0 20.0 L 300.0 20.0');
    });
});

describe('the panels that want the person', () => {
    it('counts only those that need them, not those with nothing to say', () => {
        expect(panelsNeedingAttention(['ok', 'attention', 'quiet', 'attention'])).toBe(2);
        expect(panelsNeedingAttention(['ok', 'pending'])).toBe(0);
    });
});

describe('the verdict on the whole installation', () => {
    it('says everything is fine when every panel is fine or has nothing in it', () => {
        expect(installationVerdict(['ok', 'ok', 'quiet', 'quiet'])).toEqual({ kind: 'ok' });
    });

    it('says nothing while a panel is still being read', () => {
        expect(installationVerdict(['ok', 'ok', 'pending', 'ok'])).toEqual({ kind: 'pending' });
    });

    it('says what needs the person at once, even while another panel is being read', () => {
        expect(installationVerdict(['attention', 'pending', 'quiet', 'ok'])).toEqual({
            kind: 'attention',
            count: 1,
        });
    });

    it('is not held back by a panel that has nothing to report', () => {
        expect(installationVerdict(['attention', 'ok', 'quiet', 'quiet'])).toEqual({
            kind: 'attention',
            count: 1,
        });
    });
});

function login(overrides: Partial<LoginInfo> = {}): LoginInfo {
    return {
        tenantId: 't',
        accountId: 'a',
        lastIp: '',
        lastAttemptIp: '',
        failedLogins: 0,
        locked: false,
        lastLogin: '',
        online: false,
        passwordResetRequired: false,
        ...overrides,
    };
}

describe('the sign-in records of a tenant', () => {
    const rows = [
        login({ locked: true, failedLogins: 5 }),
        login({ failedLogins: 1 }),
        login({ passwordResetRequired: true }),
        login(),
    ];

    it('counts the accounts that are locked', () => {
        expect(lockedAccounts(rows)).toBe(1);
    });

    it('counts the accounts with failed sign-ins, locked or not', () => {
        expect(accountsWithFailedSignIns(rows)).toBe(2);
    });

    it('counts the accounts that must set a new password', () => {
        expect(passwordResetsDue(rows)).toBe(1);
    });

    it('counts nothing in an empty tenant', () => {
        expect(lockedAccounts([])).toBe(0);
        expect(accountsWithFailedSignIns([])).toBe(0);
        expect(passwordResetsDue([])).toBe(0);
    });
});

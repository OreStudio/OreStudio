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

import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
    HEARTBEAT_INTERVAL_MS,
    WEB_SERVICE_NAME,
    releaseVersion,
    startHeartbeat,
    type HeartbeatSink,
} from './heartbeat.js';

type Beat = Parameters<HeartbeatSink['publishServiceHeartbeat']>[0];

function recordingSink(): { sink: HeartbeatSink; beats: Beat[]; failWith: { error?: Error } } {
    const beats: Beat[] = [];
    const failWith: { error?: Error } = {};
    return {
        beats,
        failWith,
        sink: {
            publishServiceHeartbeat: (message) => {
                if (failWith.error !== undefined) {
                    throw failWith.error;
                }
                beats.push(message);
            },
        },
    };
}

function recordingLog(): {
    log: { warn: ReturnType<typeof vi.fn>; info: ReturnType<typeof vi.fn> };
} {
    return { log: { warn: vi.fn(), info: vi.fn() } };
}

describe('the web service heartbeat', () => {
    beforeEach(() => {
        vi.useFakeTimers();
    });
    afterEach(() => {
        vi.useRealTimers();
    });

    it('names the service the registry expects and sends one beat at once', () => {
        const { sink, beats } = recordingSink();

        const stop = startHeartbeat({
            sink,
            version: '0.0.27',
            instanceId: 'instance-1',
            ...recordingLog(),
        });

        expect(beats).toEqual([
            {
                service_name: 'ores.web.service',
                instance_id: 'instance-1',
                host_id: '',
                version: '0.0.27',
            },
        ]);
        expect(WEB_SERVICE_NAME).toBe('ores.web.service');
        stop();
    });

    it('beats on the interval with one instance id for the life of the process', () => {
        const { sink, beats } = recordingSink();
        const stop = startHeartbeat({ sink, version: '0.0.27', ...recordingLog() });

        vi.advanceTimersByTime(HEARTBEAT_INTERVAL_MS * 3);

        expect(beats).toHaveLength(4);
        expect(new Set(beats.map((beat) => beat.instance_id)).size).toBe(1);
        stop();
    });

    it('stops beating when stopped', () => {
        const { sink, beats } = recordingSink();
        const stop = startHeartbeat({ sink, version: '0.0.27', ...recordingLog() });

        stop();
        vi.advanceTimersByTime(HEARTBEAT_INTERVAL_MS * 5);

        expect(beats).toHaveLength(1);
    });

    it('logs a failure once when it begins and once when it ends, and keeps beating', () => {
        const { sink, beats, failWith } = recordingSink();
        const { log } = recordingLog();
        failWith.error = new Error('not connected');

        const stop = startHeartbeat({ sink, version: '0.0.27', log });
        vi.advanceTimersByTime(HEARTBEAT_INTERVAL_MS * 4);

        expect(log.warn).toHaveBeenCalledTimes(1);
        expect(beats).toHaveLength(0);

        delete failWith.error;
        vi.advanceTimersByTime(HEARTBEAT_INTERVAL_MS * 2);

        expect(log.info).toHaveBeenCalledTimes(1);
        expect(beats).toHaveLength(2);
        stop();
    });
});

describe('the release the heartbeat reports', () => {
    it('reads the version the project declares, without a leading v', () => {
        const root = mkdtempSync(join(tmpdir(), 'release-'));
        writeFileSync(
            join(root, 'CMakeLists.txt'),
            'cmake_minimum_required(VERSION 3.20)\nproject(OreStudio VERSION 0.0.27 LANGUAGES CXX)\n',
        );

        expect(releaseVersion(root)).toBe('0.0.27');
    });

    it('finds the declaration above the directory the service runs from', () => {
        const root = mkdtempSync(join(tmpdir(), 'release-'));
        writeFileSync(
            join(root, 'CMakeLists.txt'),
            'project(OreStudio VERSION 1.2.3 LANGUAGES CXX)\n',
        );
        const nested = join(root, 'projects', 'ores.web');
        mkdirSync(nested, { recursive: true });
        writeFileSync(join(nested, 'CMakeLists.txt'), 'add_subdirectory(packages)\n');

        expect(releaseVersion(nested)).toBe('1.2.3');
    });

    it('says unknown when the tree declares none', () => {
        expect(releaseVersion(mkdtempSync(join(tmpdir(), 'release-')))).toBe('unknown');
    });
});

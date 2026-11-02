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

import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { crc32, inflateRawSync } from 'node:zlib';
import { describe, expect, it } from 'vitest';
import { zipOf } from './zip.js';

/**
 * The zip writer. What is checked is what an unzip tool relies on: the end
 * record finds the central directory, each directory entry finds its file, and
 * each file inflates to the bytes given, under its name, with its checksum.
 */

function unzip(archive: Buffer): Map<string, Buffer> {
    const end = archive.length - 22;
    expect(archive.readUInt32LE(end)).toBe(0x06054b50);
    const count = archive.readUInt16LE(end + 10);
    let at = archive.readUInt32LE(end + 16);
    const files = new Map<string, Buffer>();
    for (let i = 0; i < count; i++) {
        expect(archive.readUInt32LE(at)).toBe(0x02014b50);
        const checksum = archive.readUInt32LE(at + 16);
        const packed = archive.readUInt32LE(at + 20);
        const size = archive.readUInt32LE(at + 24);
        const nameLength = archive.readUInt16LE(at + 28);
        const local = archive.readUInt32LE(at + 42);
        const name = archive.subarray(at + 46, at + 46 + nameLength).toString('utf8');
        expect(archive.readUInt32LE(local)).toBe(0x04034b50);
        const start = local + 30 + archive.readUInt16LE(local + 26);
        const content = inflateRawSync(archive.subarray(start, start + packed));
        expect(content.length).toBe(size);
        expect(crc32(content)).toBe(checksum);
        files.set(name, content);
        at += 46 + nameLength;
    }
    return files;
}

describe('zipOf', () => {
    it('writes every file under its name, to be read back byte for byte', () => {
        const schema = Buffer.from(
            '<xs:schema><xs:include schemaLocation="ore_types.xsd"/></xs:schema>',
        );
        const types = Buffer.from('<xs:schema>'.padEnd(5000, 'x') + '</xs:schema>');
        const files = unzip(
            zipOf([
                { name: 'input.xsd', content: schema },
                { name: 'ore_types.xsd', content: types },
            ]),
        );

        expect([...files.keys()]).toEqual(['input.xsd', 'ore_types.xsd']);
        expect(files.get('input.xsd')?.equals(schema)).toBe(true);
        expect(files.get('ore_types.xsd')?.equals(types)).toBe(true);
    });

    it.skipIf(spawnSync('unzip', ['-v']).status !== 0)(
        'writes an archive that the unzip tool tests clean and extracts byte for byte',
        () => {
            const types = Buffer.from('<xs:schema>'.padEnd(5000, 'x') + '</xs:schema>');
            const directory = mkdtempSync(join(tmpdir(), 'zip-test-'));
            try {
                const archive = join(directory, 'schema.zip');
                writeFileSync(archive, zipOf([{ name: 'ore_types.xsd', content: types }]));

                expect(spawnSync('unzip', ['-tq', archive]).status).toBe(0);
                expect(spawnSync('unzip', ['-q', archive, '-d', directory]).status).toBe(0);
                expect(readFileSync(join(directory, 'ore_types.xsd')).equals(types)).toBe(true);
            } finally {
                rmSync(directory, { recursive: true, force: true });
            }
        },
    );

    it('writes an archive of no files that a reader opens empty', () => {
        expect(unzip(zipOf([])).size).toBe(0);
    });
});

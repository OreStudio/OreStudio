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

import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterAll, beforeAll, describe, expect, it } from 'vitest';
import { ConfigurationError, loadSiteConfiguration } from './site-config.js';

/**
 * Which environment a site serves is decided once, at startup, and getting it
 * wrong points a browser at the wrong broker. The precedence is therefore
 * asserted directly rather than inferred from a successful start.
 */

let directory: string;
let configPath: string;

beforeAll(() => {
  directory = mkdtempSync(join(tmpdir(), 'ores-site-config-'));
  configPath = join(directory, 'environments.json');
  writeFileSync(
    configPath,
    JSON.stringify({
      active: 'bright_faraday',
      environments: [
        {
          id: 'eager_maxwell',
          displayName: 'Eager Maxwell',
          host: 'localhost',
          port: 21405,
          subjectPrefix: 'ores.dev.eager_maxwell',
        },
        {
          id: 'bright_faraday',
          displayName: 'Bright Faraday',
          host: 'localhost',
          port: 20605,
          subjectPrefix: 'ores.dev.bright_faraday',
        },
      ],
      tls: { ca: 'ca', cert: 'cert', key: 'key' },
      developerTools: false,
      developerAccounts: [],
    }),
  );
});

afterAll(() => {
  rmSync(directory, { recursive: true, force: true });
});

function load(
  environment: NodeJS.ProcessEnv,
  environmentId?: string,
): ReturnType<typeof loadSiteConfiguration> {
  return loadSiteConfiguration({
    environment: { ORES_WEB_SITE_CONFIG: configPath, ...environment },
    ...(environmentId === undefined ? {} : { environmentId }),
  });
}

describe('loadSiteConfiguration environment precedence', () => {
  it('serves the checkout environment when nothing else names one', () => {
    // The checkout's name uses dashes and the file's ids use underscores.
    const site = load({ ORES_ENV_NAME: 'eager-maxwell' });
    expect(site.environment.id).toBe('eager_maxwell');
  });

  it('prefers ORES_WEB_ENV to the checkout name', () => {
    const site = load({ ORES_ENV_NAME: 'eager-maxwell', ORES_WEB_ENV: 'bright_faraday' });
    expect(site.environment.id).toBe('bright_faraday');
  });

  it('prefers --env to both variables', () => {
    const site = load(
      { ORES_ENV_NAME: 'bright_faraday', ORES_WEB_ENV: 'bright_faraday' },
      'eager_maxwell',
    );
    expect(site.environment.id).toBe('eager_maxwell');
  });

  it('falls back to the file active field when nothing is named', () => {
    const site = load({});
    expect(site.environment.id).toBe('bright_faraday');
  });

  it('rejects a checkout name the file does not define', () => {
    expect(() => load({ ORES_ENV_NAME: 'missing_environment' })).toThrow(ConfigurationError);
  });
});

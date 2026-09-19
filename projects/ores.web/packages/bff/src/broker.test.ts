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
import { siteConfigurationSchema } from '@ores/contracts';
import { resolveBroker } from './broker.js';

/**
 * The process environment wins over the site configuration, and the fallback
 * has to keep working, so both branches are asserted with the same site.
 */

const site = siteConfigurationSchema.parse({
  active: 'eager_maxwell',
  environments: [
    {
      id: 'eager_maxwell',
      displayName: 'Eager Maxwell',
      host: 'localhost',
      port: 21405,
      subjectPrefix: 'ores.dev.eager_maxwell',
      tls: { cert: '/site/cert', key: '/site/key' },
    },
  ],
  tls: { ca: '/site/ca', cert: '/site/default-cert', key: '/site/default-key' },
  developerTools: false,
  developerAccounts: [],
});

const environment = site.environments[0]!;

describe('resolveBroker', () => {
  it('takes the broker from the process environment when it names one', () => {
    const broker = resolveBroker(site, environment, {
      ORES_NATS_URL: 'nats://broker.example:4222',
      ORES_NATS_SUBJECT_PREFIX: 'ores.override',
      ORES_NATS_TLS_CA: '/env/ca',
      ORES_NATS_TLS_CERT: '/env/cert',
      ORES_NATS_TLS_KEY: '/env/key',
    });

    expect(broker).toEqual({
      server: 'nats://broker.example:4222',
      subjectPrefix: 'ores.override',
      tls: { ca: '/env/ca', cert: '/env/cert', key: '/env/key' },
    });
  });

  it('keeps the environment subject prefix when the process names none', () => {
    const broker = resolveBroker(site, environment, {
      ORES_NATS_URL: 'nats://broker.example:4222',
    });

    expect(broker.subjectPrefix).toBe('ores.dev.eager_maxwell');
    expect(broker.tls).toEqual({ ca: '', cert: '', key: '' });
  });

  it('falls back to the site configuration when the process names no broker', () => {
    const broker = resolveBroker(site, environment, {});

    expect(broker).toEqual({
      server: 'nats://localhost:21405',
      subjectPrefix: 'ores.dev.eager_maxwell',
      tls: { ca: '/site/ca', cert: '/site/cert', key: '/site/key' },
    });
  });

  it('treats a blank broker URL as absent', () => {
    const broker = resolveBroker(site, environment, { ORES_NATS_URL: '   ' });

    expect(broker.server).toBe('nats://localhost:21405');
  });
});

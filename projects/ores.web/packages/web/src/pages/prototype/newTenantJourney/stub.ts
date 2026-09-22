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

import { useEffect, useRef, useState } from 'react';
import acmeLogo from './acme-logo.png';

/**
 * PROTOTYPE ONLY. In-memory stand-ins for the seed profile contract: the
 * registered profiles, their parameter schemas, the workflow steps each one
 * runs, and a simulated workflow instance. Nothing here talks to a server.
 */

export interface ProfileParam {
  readonly name: string;
  readonly label: string;
  readonly type: 'text' | 'choice';
  readonly choices?: readonly string[];
  readonly default: string;
  readonly required: boolean;
  readonly hint?: string;
}

export interface SeedProfile {
  readonly code: string;
  readonly name: string;
  readonly summary: string;
  readonly bullets: readonly string[];
  readonly audience: string;
  readonly params: readonly ProfileParam[];
  readonly steps: readonly string[];
  /** Tenant details the profile fills in; the person can still change them. */
  readonly defaults?: Partial<Omit<TenantDetails, 'params'>>;
  readonly logo?: string;
  /** In first run, the tenant admin's password starts as the super admin's. */
  readonly inheritsAdminPassword?: boolean;
  /** The parties the tenant ends up with, for the first sign-in's default party. */
  readonly parties: readonly string[];
}

export const PROFILES: readonly SeedProfile[] = [
  {
    code: 'empty_operational',
    name: 'Operational',
    summary: 'Production-ready setup',
    bullets: ['Standard reference data and counterparties', 'Your legal entities, from their LEI', 'No test data'],
    audience: 'For real use',
    parties: ['The legal entity of the root LEI'],
    params: [
      {
        name: 'root_lei',
        label: 'Root LEI',
        type: 'text',
        default: '',
        required: true,
        hint: 'The LEI of the top legal entity. Its GLEIF hierarchy becomes the tenant\'s parties.',
      },
      {
        name: 'counterparty_size',
        label: 'Counterparty set',
        type: 'choice',
        choices: ['small', 'large'],
        default: 'small',
        required: true,
        hint: 'small is about 13k GLEIF counterparties; large is about 500k.',
      },
    ],
    steps: [
      'Create tenant and admin',
      'Publish base reference data',
      'Import counterparties',
      'Import parties from root LEI',
      'Provision parties (activate, onboard, essentials)',
      'Complete provisioning',
    ],
  },
  {
    code: 'acme_demo',
    name: 'ACME demo',
    summary: 'Pre-configured sandbox',
    bullets: ['4 legal entities, books and desks', '45 staff to sign in as', 'Live synthetic market data'],
    audience: 'For demos and testing',
    logo: acmeLogo,
    inheritsAdminPassword: true,
    parties: ['Acme Corporation Plc', 'ACME Corporation UK plc', 'ACME Corporation US Inc', 'ACME Corporation HK Ltd'],
    defaults: {
      code: 'acme_corporation',
      name: 'Acme Corporation',
      hostname: 'acme_corporation.localhost',
      adminUsername: 'tenant_admin',
      adminEmail: 'tenant_admin@acme.example.com',
    },
    params: [],
    steps: [
      'Create tenant and admin',
      'Publish base reference data',
      'Import counterparties',
      'Import Acme LEI hierarchy',
      'Provision Acme Corporation Plc',
      'Provision ACME UK, US and HK',
      'Load staff and photos',
      'Start market data feeds',
      'Complete provisioning',
    ],
  },
];

export interface TenantDetails {
  code: string;
  name: string;
  hostname: string;
  adminUsername: string;
  adminEmail: string;
  adminPassword: string;
  params: Record<string, string>;
}

export function emptyDetails(profile: SeedProfile): TenantDetails {
  return {
    code: '',
    name: '',
    hostname: '',
    adminUsername: 'tenant_admin',
    adminEmail: '',
    adminPassword: '',
    ...profile.defaults,
    params: Object.fromEntries(profile.params.map((p) => [p.name, p.default])),
  };
}

export type StepState = 'pending' | 'running' | 'done' | 'failed';

export interface RunState {
  readonly steps: readonly { readonly label: string; readonly state: StepState }[];
  readonly status: 'running' | 'failed' | 'done';
}

/**
 * A simulated workflow instance. Steps advance on a timer; when `failAt` is
 * set, that step fails once, and `retry` resumes from it -- the resume
 * semantics the contract chose over compensation.
 */
export function useSimulatedRun(
  profile: SeedProfile | undefined,
  started: boolean,
  failAt: number | undefined,
): RunState & { readonly retry: () => void } {
  const [cursor, setCursor] = useState(0);
  const [failed, setFailed] = useState(false);
  const failedOnce = useRef(false);

  useEffect(() => {
    setCursor(0);
    setFailed(false);
    failedOnce.current = false;
  }, [profile?.code, started]);

  const total = profile?.steps.length ?? 0;

  useEffect(() => {
    if (!started || failed || cursor >= total) return;
    const timer = setTimeout(() => {
      if (failAt === cursor && !failedOnce.current) {
        failedOnce.current = true;
        setFailed(true);
        return;
      }
      setCursor((c) => c + 1);
    }, 900);
    return () => clearTimeout(timer);
  }, [started, failed, cursor, total, failAt]);

  const steps = (profile?.steps ?? []).map((label, i) => ({
    label,
    state: (i < cursor ? 'done' : i === cursor ? (failed ? 'failed' : started ? 'running' : 'pending') : 'pending') as StepState,
  }));

  return {
    steps,
    status: failed ? 'failed' : cursor >= total && started ? 'done' : 'running',
    retry: () => setFailed(false),
  };
}

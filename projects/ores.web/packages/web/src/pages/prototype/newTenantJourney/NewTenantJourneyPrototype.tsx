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

import { useCallback, useState, type ReactNode } from 'react';
import { Button, Field, Input, Notice, PageHeader, Select } from '../../../ui/Primitives.js';
import { NewPasswordField } from '../../../ui/PasswordField.js';
import {
  JourneyPage,
  newTenantSteps,
  useAdvanceWhenProvisioned,
  useNewTenant,
  type JourneyStep,
  type NewTenant,
} from './journey.js';

/**
 * PROTOTYPE ONLY -- throwaway. The two journeys built on the chosen stepper
 * layout (variant A; the rejected variants are on the
 * prototype/new-tenant-journey branch). All data is stubbed; nothing is sent
 * to the server.
 */

const PROVISIONING = 3;
const HANDOFF = 4;

/** New tenant: the shared tenant steps, then back to the Tenants page. */
export function NewTenantJourneyPrototype(): ReactNode {
  const t = useNewTenant();
  const [at, setAt] = useState(0);
  useAdvanceWhenProvisioned(t, at === PROVISIONING, useCallback(() => setAt(HANDOFF), []));

  const steps = newTenantSteps(t, {
    onContinue: () => alert('PROTOTYPE: signs out, signs in as the tenant admin, opens their first sign-in.'),
    onElsewhere: () => alert('PROTOTYPE: back to the Tenants page; the new tenant is listed as active.'),
  });
  return (
    <>
      <PageHeader title="New tenant" />
      <JourneyPage steps={steps} at={at} onMove={setAt} />
    </>
  );
}

/**
 * First run: the only page while the system is in bootstrap mode. Create the
 * system administrator, then the new tenant steps inline, then the tenant
 * administrator's first sign-in -- one flat list of steps.
 */
export function FirstRunJourneyPrototype(): ReactNode {
  const t = useNewTenant();
  const [at, setAt] = useState(0);
  const [admin, setAdmin] = useState({ username: 'admin', email: '', password: '', ok: false });
  const [handedOff, setHandedOff] = useState(false);
  const [signIn, setSignIn] = useState({ password: '', ok: false, party: '' });
  useAdvanceWhenProvisioned(t, at === PROVISIONING + 1, useCallback(() => setAt(HANDOFF + 1), []));

  const tenantSteps = newTenantSteps(t, {
    onContinue: () => setAt(HANDOFF + 2),
    onElsewhere: () => {
      setHandedOff(true);
      setAt(HANDOFF + 3);
    },
  });

  const steps: readonly JourneyStep[] = [
    {
      id: 'system-admin',
      title: 'Create the administrator',
      lead: 'ORE Studio is not set up yet. Create the administrator who sets it up and creates tenants.',
      body: (
        <div className="grid gap-4 sm:grid-cols-2">
          <Field label="Username">
            <Input value={admin.username} autoComplete="username" onChange={(e) => setAdmin({ ...admin, username: e.target.value })} />
          </Field>
          <Field label="Email">
            <Input value={admin.email} type="email" onChange={(e) => setAdmin({ ...admin, email: e.target.value })} />
          </Field>
          <div className="sm:col-span-2">
            <NewPasswordField value={admin.password} onChange={(password, ok) => setAdmin({ ...admin, password, ok })} />
          </div>
        </div>
      ),
      next: { label: 'Create administrator', enabled: admin.ok && admin.username.length >= 3 },
      final: true,
    },
    ...tenantSteps,
    firstSignIn(t, signIn, setSignIn),
    ready(t, handedOff),
  ];

  return (
    <>
      <PageHeader title="Set up ORE Studio" />
      {at > 0 && (
        <p className="-mt-4 mb-6 text-xs text-ink-faint">
          Signed in as <span className="font-mono">{at > HANDOFF + 1 && !handedOff ? tenantAdmin(t) : admin.username}</span>
        </p>
      )}
      <JourneyPage steps={steps} at={at} onMove={setAt} />
    </>
  );
}

function tenantAdmin(t: NewTenant): string {
  return `${t.details?.adminUsername ?? 'tenant_admin'}@${t.details?.code ?? 'tenant'}`;
}

function firstSignIn(
  t: NewTenant,
  signIn: { password: string; ok: boolean; party: string },
  setSignIn: (value: { password: string; ok: boolean; party: string }) => void,
): JourneyStep {
  const parties = t.profile?.parties ?? [];
  return {
    id: 'first-sign-in',
    title: 'First sign-in',
    lead: 'Set a password only you know, then choose where you start.',
    body: (
      <div className="space-y-5">
        <NewPasswordField
          label="New password"
          value={signIn.password}
          onChange={(password, ok) => setSignIn({ ...signIn, password, ok })}
        />
        {parties.length > 1 && (
          <Field label="Start in" hint="You work in more than one party. You can switch at any time.">
            <Select value={signIn.party || parties[0]} onChange={(e) => setSignIn({ ...signIn, party: e.target.value })}>
              {parties.map((p) => (
                <option key={p}>{p}</option>
              ))}
            </Select>
          </Field>
        )}
      </div>
    ),
    next: { label: 'Finish', enabled: signIn.ok },
    final: true,
  };
}

function ready(t: NewTenant, handedOff: boolean): JourneyStep {
  return {
    id: 'ready',
    title: 'Ready',
    lead: 'ORE Studio is set up.',
    body: (
      <div className="space-y-4">
        {handedOff ? (
          <Notice tone="success">
            Give <span className="font-mono">{tenantAdmin(t)}</span> their username. They set their own password at
            first sign-in.
          </Notice>
        ) : (
          <Notice tone="success">You are signed in to {t.details?.name ?? 'the new tenant'}.</Notice>
        )}
        <div className="flex flex-wrap gap-2">
          <Button variant="primary">Go to {handedOff ? 'Tenants' : 'home'}</Button>
          <Button>Create another tenant</Button>
        </div>
      </div>
    ),
  };
}

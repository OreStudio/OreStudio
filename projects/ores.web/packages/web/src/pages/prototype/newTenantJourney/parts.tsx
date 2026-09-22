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

import type { ReactNode } from 'react';
import { Button, Field, Input, Select, cx } from '../../../ui/Primitives.js';
import { NewPasswordField } from '../../../ui/PasswordField.js';
import type { RunState, SeedProfile, StepState, TenantDetails } from './stub.js';

/**
 * PROTOTYPE ONLY. The words that tell a person where they are. Adapted from
 * the ores.qt wizards' welcome pages and subtitles: say why, list the steps
 * up front, say what happens after.
 */
export const JOURNEY_STEPS = [
  {
    title: 'Choose a starting point',
    lead: 'Choose a starting point for the new tenant.',
  },
  {
    title: 'Describe the tenant',
    lead: 'Name the tenant and create its administrator.',
  },
  {
    title: 'Review',
    lead: 'Nothing is created until you confirm.',
  },
  {
    title: 'Provisioning',
    lead: 'This runs on the server. You can leave this page and come back.',
  },
  {
    title: 'Hand off',
    lead: 'The tenant is ready. Its administrator signs in next.',
  },
] as const;

export function StepLead({ index }: { readonly index: number }): ReactNode {
  return <p className="mb-5 text-sm text-ink-muted">{JOURNEY_STEPS[index]?.lead}</p>;
}

/**
 * PROTOTYPE ONLY. The journey's steps, written once and placed differently by
 * each variant -- the "library of steps" the catalogue decided on.
 */

export function ProfileChoice({
  profiles,
  selected,
  onSelect,
  layout = 'grid',
}: {
  readonly profiles: readonly SeedProfile[];
  readonly selected: string | undefined;
  readonly onSelect: (code: string) => void;
  readonly layout?: 'grid' | 'list';
}): ReactNode {
  return (
    <div role="radiogroup" className={cx('gap-3', layout === 'grid' ? 'grid sm:grid-cols-2' : 'flex flex-col')}>
      {profiles.map((p) => (
        <button
          key={p.code}
          type="button"
          role="radio"
          aria-checked={selected === p.code}
          onClick={() => onSelect(p.code)}
          className={cx(
            'card p-4 text-left transition-colors',
            selected === p.code ? 'border-accent ring-3 ring-accent/20' : 'hover:border-line-strong',
          )}
        >
          {p.logo !== undefined && <img src={p.logo} alt="" className="mb-3 h-12 w-auto rounded-md bg-white p-1.5" />}
          <div className="flex items-baseline justify-between gap-2">
            <span className="font-semibold">{p.name}</span>
            <span className="text-xs text-ink-faint">{p.audience}</span>
          </div>
          <p className="mt-1 text-sm text-ink-muted">{p.summary}</p>
          <ul className="mt-3 space-y-1 text-sm">
            {p.bullets.map((b) => (
              <li key={b} className="flex gap-2">
                <span aria-hidden className="text-ink-faint">•</span>
                {b}
              </li>
            ))}
          </ul>
          <p className="mt-3 text-xs text-ink-faint">
            {p.params.length} {p.params.length === 1 ? 'setting' : 'settings'} · {p.steps.length} steps
          </p>
        </button>
      ))}
    </div>
  );
}

export function DetailsForm({
  profile,
  details,
  onChange,
}: {
  readonly profile: SeedProfile;
  readonly details: TenantDetails;
  readonly onChange: (details: TenantDetails) => void;
}): ReactNode {
  const set = (key: keyof Omit<TenantDetails, 'params'>, value: string): void =>
    onChange({ ...details, [key]: value });
  const setParam = (name: string, value: string): void =>
    onChange({ ...details, params: { ...details.params, [name]: value } });

  return (
    <div className="space-y-6">
      {profile.defaults !== undefined && (
        <p className="text-sm text-ink-muted">Filled in for {profile.name}. Change anything you need, then set the administrator's password.</p>
      )}
      <fieldset className="grid gap-4 sm:grid-cols-2">
        <legend className="mb-2 text-sm font-semibold">Tenant</legend>
        <Field label="Name">
          <Input value={details.name} onChange={(e) => set('name', e.target.value)} placeholder="Northwind Capital" />
        </Field>
        <Field label="Code" hint="Short and unique. Used in usernames: admin@code.">
          <Input value={details.code} onChange={(e) => set('code', e.target.value)} placeholder="northwind" />
        </Field>
        <Field label="Hostname" className="sm:col-span-2">
          <Input value={details.hostname} onChange={(e) => set('hostname', e.target.value)} placeholder="northwind.example.com" />
        </Field>
      </fieldset>

      {profile.params.length > 0 && (
        <fieldset className="grid gap-4 sm:grid-cols-2">
          <legend className="mb-2 text-sm font-semibold">{profile.name} settings</legend>
          {profile.params.map((p) => (
            <Field key={p.name} label={p.label} {...(p.hint !== undefined && { hint: p.hint })}>
              {p.type === 'choice' ? (
                <Select value={details.params[p.name]} onChange={(e) => setParam(p.name, e.target.value)}>
                  {p.choices?.map((c) => (
                    <option key={c}>{c}</option>
                  ))}
                </Select>
              ) : (
                <Input value={details.params[p.name]} onChange={(e) => setParam(p.name, e.target.value)} />
              )}
            </Field>
          ))}
        </fieldset>
      )}

      <fieldset className="grid gap-4 sm:grid-cols-2">
        <legend className="mb-2 text-sm font-semibold">Tenant administrator</legend>
        <Field label="Username">
          <Input value={details.adminUsername} onChange={(e) => set('adminUsername', e.target.value)} />
        </Field>
        <Field label="Email">
          <Input value={details.adminEmail} onChange={(e) => set('adminEmail', e.target.value)} />
        </Field>
        <div className="sm:col-span-2">
          <NewPasswordField
            label="Initial password"
            hint="They must change it at first sign-in."
            value={details.adminPassword}
            onChange={(password) => set('adminPassword', password)}
          />
        </div>
      </fieldset>
    </div>
  );
}

const MARK: Record<StepState, string> = { pending: '○', running: '◐', done: '●', failed: '✕' };
const TONE: Record<StepState, string> = {
  pending: 'text-ink-faint',
  running: 'text-accent-bright',
  done: 'text-up',
  failed: 'text-down',
};

export function ProgressList({
  run,
  onRetry,
  onDiscard,
  compact = false,
}: {
  readonly run: RunState;
  readonly onRetry: () => void;
  readonly onDiscard?: () => void;
  readonly compact?: boolean;
}): ReactNode {
  return (
    <div>
      <ol className={cx(compact ? 'space-y-1' : 'space-y-2')}>
        {run.steps.map((s) => (
          <li key={s.label} className={cx('flex items-center gap-3 text-sm', TONE[s.state])}>
            <span aria-hidden className={cx('w-4 text-center', s.state === 'running' && 'animate-pulse')}>
              {MARK[s.state]}
            </span>
            <span className={s.state === 'pending' ? 'text-ink-faint' : 'text-ink'}>{s.label}</span>
            {s.state === 'failed' && <span className="text-xs">timed out</span>}
          </li>
        ))}
      </ol>
      {run.status === 'failed' && (
        <div className="mt-4 flex flex-wrap items-center gap-2">
          <Button variant="primary" size="sm" onClick={onRetry}>
            Retry from failed step
          </Button>
          {onDiscard !== undefined && (
            <Button variant="danger" size="sm" onClick={onDiscard}>
              Discard tenant
            </Button>
          )}
          <span className="text-xs text-ink-faint">Completed steps are kept. Retrying is safe.</span>
        </div>
      )}
    </div>
  );
}

export function Handoff({
  details,
  onRestart,
}: {
  readonly details: TenantDetails;
  readonly onRestart: () => void;
}): ReactNode {
  const user = `${details.adminUsername}@${details.code || 'tenant'}`;
  return (
    <div className="space-y-4">
      <p className="text-sm text-ink-muted">
        The tenant is ready. Its administrator is <span className="font-mono text-ink">{user}</span>.
      </p>
      <div className="grid gap-3 sm:grid-cols-2">
        <button type="button" className="card p-4 text-left hover:border-accent" onClick={() => alert('PROTOTYPE: signs out, signs in as ' + user + ', opens first sign-in (new password).')}>
          <span className="font-semibold">Continue as tenant admin</span>
          <p className="mt-1 text-sm text-ink-muted">Sign in as {user} now and finish their first sign-in.</p>
        </button>
        <button type="button" className="card p-4 text-left hover:border-line-strong" onClick={() => alert('PROTOTYPE: signs out; shows ' + user + ' to pass on.')}>
          <span className="font-semibold">Hand off to someone else</span>
          <p className="mt-1 text-sm text-ink-muted">Give them the username. They set their own password at first sign-in.</p>
        </button>
      </div>
      <Button variant="ghost" size="sm" onClick={onRestart}>
        Start over (prototype)
      </Button>
    </div>
  );
}

/** PROTOTYPE ONLY. Lets the reviewer see the failure path on demand. */
export function FailToggle({
  value,
  onChange,
}: {
  readonly value: boolean;
  readonly onChange: (value: boolean) => void;
}): ReactNode {
  return (
    <label className="flex items-center gap-2 text-xs text-fuchsia-400">
      <input type="checkbox" checked={value} onChange={(e) => onChange(e.target.checked)} />
      Prototype: make step 3 fail once
    </label>
  );
}

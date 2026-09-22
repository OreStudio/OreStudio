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

import { useState, type FormEvent, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useSession } from '../session/SessionProvider.js';
import { ApiFailure } from '../api/transport.js';
import { PasswordInput } from '../ui/PasswordField.js';
import { Button, Field, Input, Notice, Tag } from '../ui/Primitives.js';
import type { PartySummary } from '@ores/wire-protocol/browser';

/**
 * Signing in.
 *
 * An ordinary web login. No heading that repeats the button, no environment
 * notice, and no field for anything the deployment already knows. The
 * environment is in the footer, where it belongs.
 */
export function SignInPage(): ReactNode {
  const { signIn, chooseParty } = useSession();

  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [pendingParties, setPendingParties] = useState<readonly PartySummary[] | null>(null);

  async function handleSubmit(event: FormEvent<HTMLFormElement>): Promise<void> {
    event.preventDefault();
    setBusy(true);
    setError(null);
    try {
      const result = await signIn({ username: username.trim(), password });
      if (result.outcome === 'party-required') {
        setPendingParties(result.parties);
      }
    } catch (cause) {
      setError(cause instanceof ApiFailure ? cause.message : 'Something went wrong.');
    } finally {
      setBusy(false);
    }
  }

  if (pendingParties !== null) {
    return (
      <div className="mx-auto max-w-[420px] py-16">
        <h2 className="text-xl font-semibold tracking-tight">Choose a party</h2>
        <p className="mt-2 mb-6 text-sm text-ink-muted">
          This account works in more than one party.
        </p>
        {error !== null && <Notice tone="error">{error}</Notice>}
        <ul className="space-y-2">
          {pendingParties.map((party) => (
            <li key={party.id}>
              <button
                type="button"
                disabled={busy}
                className="flex w-full items-center justify-between rounded-[var(--radius-card)] border border-line bg-bg-secondary px-4 py-3 text-left text-sm hover:border-line-strong disabled:opacity-50"
                onClick={() => {
                  setBusy(true);
                  setError(null);
                  chooseParty(party.id, pendingParties)
                    .catch((cause: unknown) =>
                      setError(cause instanceof ApiFailure ? cause.message : 'That did not work.'),
                    )
                    .finally(() => setBusy(false));
                }}
              >
                <span>{party.name.length > 0 ? party.name : party.id}</span>
                {party.partyCategory.length > 0 && <Tag>{party.partyCategory}</Tag>}
              </button>
            </li>
          ))}
        </ul>
      </div>
    );
  }

  return (
    <div className="mx-auto max-w-[380px] py-16">
      {error !== null && <Notice tone="error">{error}</Notice>}

      <form onSubmit={(event) => void handleSubmit(event)} className="space-y-5">
        <Field label="Username">
          <Input
            name="username"
            value={username}
            autoComplete="username"
            autoFocus
            required
            onChange={(event) => {
              setUsername(event.target.value);
              setError(null);
            }}
          />
        </Field>

        <Field label="Password">
          <PasswordInput
            name="password"
            value={password}
            autoComplete="current-password"
            required
            onChange={(event) => {
              setPassword(event.target.value);
              setError(null);
            }}
          />
        </Field>

        <Button
          type="submit"
          variant="primary"
          size="lg"
          className="w-full"
          disabled={busy || username.trim().length === 0 || password.length === 0}
          pending={busy}
          pendingLabel="Signing in..."
        >
          Sign in
        </Button>
      </form>

      <p className="mt-6 text-center text-sm text-ink-muted">
        No account?{' '}
        <Link to="/signup" className="text-accent hover:text-accent-bright">
          Sign up
        </Link>
      </p>
    </div>
  );
}

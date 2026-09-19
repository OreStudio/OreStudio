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

import { useMemo, useState, type ReactNode } from 'react';
import type { Account } from '@ores/wire-protocol/browser';
import { useAccounts } from '../api/queries.js';
import { Button, Detail, Field, Input, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';

/**
 * The accounts screen.
 *
 * The C++ list handler accepts `offset` and `limit` and then ignores them, so
 * this asks for one large page and filters in the browser. The moment the server
 * applies pagination this becomes a keyed query again; the seam is
 * `listRequest`.
 */

const PAGE_SIZE = 500;
const ACCOUNT_TYPES = ['user', 'service', 'algorithm', 'llm'] as const;

export function AccountsPage(): ReactNode {
  const [search, setSearch] = useState('');
  const [accountType, setAccountType] = useState('');
  const [selectedId, setSelectedId] = useState<string | null>(null);

  const listRequest = useMemo(() => ({ offset: 0, limit: PAGE_SIZE }), []);
  const { data, isPending, isError, error, isFetching } = useAccounts(listRequest);

  const rows = useMemo(() => {
    const all = data?.accounts ?? [];
    const needle = search.trim().toLowerCase();
    return all.filter((account) => {
      if (accountType !== '' && account.accountType !== accountType) {
        return false;
      }
      if (needle.length === 0) {
        return true;
      }
      return (
        account.username.toLowerCase().includes(needle) ||
        account.fullName.toLowerCase().includes(needle) ||
        account.email.toLowerCase().includes(needle)
      );
    });
  }, [data, search, accountType]);

  const selected = rows.find((account) => account.id === selectedId) ?? null;

  return (
    <section className="mx-auto max-w-6xl">
      <PageHeader
        title="Accounts"
        description="Identities that can sign in or act as a service, scoped to this tenant."
      />

      {isError && (
        <Notice tone="error">
          {error instanceof Error ? error.message : 'Could not load accounts.'}
        </Notice>
      )}

      <div className="mb-4 flex flex-wrap items-end gap-3">
        <Field label="Search" className="w-64">
          <Input
            type="search"
            placeholder="Username, name, or email"
            value={search}
            onChange={(event) => setSearch(event.target.value)}
          />
        </Field>
        <Field label="Type" className="w-40">
          <Select value={accountType} onChange={(event) => setAccountType(event.target.value)}>
            <option value="">All types</option>
            {ACCOUNT_TYPES.map((type) => (
              <option key={type} value={type}>
                {type}
              </option>
            ))}
          </Select>
        </Field>
        <p className="ml-auto pb-2 text-xs text-ink-faint">
          {isPending ? 'Loading...' : `${rows.length} of ${data?.totalCount ?? 0}`}
          {isFetching && !isPending ? ' · refreshing' : ''}
        </p>
      </div>

      <div className="card overflow-hidden">
        <table className="w-full text-sm">
          <thead>
            <tr className="border-b border-line text-left text-[11px] uppercase tracking-wider text-ink-faint">
              <th className="px-4 py-2 font-medium">Username</th>
              <th className="px-4 py-2 font-medium">Full name</th>
              <th className="px-4 py-2 font-medium">Email</th>
              <th className="px-4 py-2 font-medium">Type</th>
              <th className="px-4 py-2 font-medium">Recorded</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((account) => (
              <tr
                key={account.id}
                onClick={() => setSelectedId(account.id === selectedId ? null : account.id)}
                className={cx(
                  'cursor-pointer border-b border-line-subtle last:border-0',
                  account.id === selectedId ? 'bg-accent/10' : 'hover:bg-surface-hover',
                )}
              >
                <td className="px-4 py-2 font-mono text-xs">{account.username}</td>
                <td className="px-4 py-2">
                  {account.fullName.length > 0 ? (
                    account.fullName
                  ) : (
                    <span className="text-ink-faint">not recorded</span>
                  )}
                </td>
                <td className="px-4 py-2 text-ink-muted">{account.email}</td>
                <td className="px-4 py-2">
                  <Tag tone={account.accountType === 'user' ? 'neutral' : 'accent'}>
                    {account.accountType}
                  </Tag>
                </td>
                <td className="px-4 py-2 font-mono text-xs text-ink-muted">{account.recordedAt}</td>
              </tr>
            ))}
          </tbody>
        </table>
        {!isPending && rows.length === 0 && (
          <p className="px-4 py-10 text-center text-sm text-ink-faint">
            No accounts match the current filter.
          </p>
        )}
      </div>

      {selected !== null && <AccountDetail account={selected} onClose={() => setSelectedId(null)} />}
    </section>
  );
}

function AccountDetail({
  account,
  onClose,
}: {
  readonly account: Account;
  readonly onClose: () => void;
}): ReactNode {
  return (
    <aside className="card mt-5 p-5" aria-label={`Details for ${account.username}`}>
      <div className="mb-4 flex items-start justify-between">
        <h2 className="text-sm font-semibold">{account.username}</h2>
        <Button size="sm" variant="ghost" onClick={onClose}>
          Close
        </Button>
      </div>
      <dl className="grid gap-4 sm:grid-cols-3 lg:grid-cols-4">
        <Detail label="Account id" value={account.id} mono />
        <Detail label="Tenant id" value={account.tenantId} mono />
        <Detail label="Version" value={String(account.version)} />
        <Detail label="Account type" value={account.accountType} />
        <Detail label="Full name" value={account.fullName || 'not recorded'} />
        <Detail label="Email" value={account.email} />
        <Detail label="Job title" value={account.jobTitle || 'not recorded'} />
        <Detail label="Default party" value={account.defaultPartyId ?? 'not set'} mono />
        <Detail label="Reports to" value={account.reportsToAccountId ?? 'nobody'} mono />
        <Detail label="Recorded at" value={account.recordedAt} mono />
        <Detail label="Last change by" value={account.modifiedBy || 'unknown'} />
        <Detail label="Change reason" value={account.changeReasonCode || 'none'} />
      </dl>
      {account.changeCommentary.length > 0 && (
        <p className="mt-4 border-t border-line pt-4 text-sm text-ink-muted">
          {account.changeCommentary}
        </p>
      )}
    </aside>
  );
}

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

import { keepPreviousData, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { api } from './client.js';
import type { AccountList } from '@ores/wire-protocol/browser';

/**
 * Server state for the accounts screen.
 *
 * Reads are keyed by the page request so paging back to a visited page is
 * instant, and `keepPreviousData` holds the current rows on screen while the
 * next page loads instead of flashing an empty table.
 */

export const accountsKey = (input: { readonly offset: number; readonly limit: number }) =>
  ['accounts', input] as const;

export function useAccounts(input: { readonly offset: number; readonly limit: number }) {
  return useQuery<AccountList>({
    queryKey: accountsKey(input),
    queryFn: () => api.accounts(input),
    placeholderData: keepPreviousData,
  });
}

export function useLockAccount() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (input: { readonly accountId: string; readonly locked: boolean }) =>
      api.setAccountLocked(input.accountId, input.locked),
    onSuccess: () => queryClient.invalidateQueries({ queryKey: ['accounts'] }),
  });
}

export function useDeleteAccount() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (accountId: string) => api.deleteAccount(accountId),
    onSuccess: () => queryClient.invalidateQueries({ queryKey: ['accounts'] }),
  });
}

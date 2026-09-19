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

import { useQuery, type UseQueryResult } from '@tanstack/react-query';
import { z } from 'zod';
import { request } from './transport.js';

/**
 * The reasons a write may carry.
 *
 * Fetched from the server rather than declared in the interface, because the set
 * is data: it differs per deployment, and one reason means "changed nothing
 * material" while the rest mean the opposite. A hardcoded list would be wrong on
 * the first deployment that added a reason.
 */
const reasonSchema = z.object({
  code: z.string(),
  description: z.string(),
  categoryCode: z.string(),
  appliesToNew: z.boolean(),
  appliesToAmend: z.boolean(),
  appliesToDelete: z.boolean(),
  requiresCommentary: z.boolean(),
  displayOrder: z.int(),
});

export type ChangeReason = z.infer<typeof reasonSchema>;

const reasonListSchema = z.object({ reasons: z.array(reasonSchema) });

export function useChangeReasons(): UseQueryResult<readonly ChangeReason[]> {
  return useQuery({
    queryKey: ['change-reasons'],
    queryFn: async () => {
      const body = await request('/api/change-reasons', { method: 'GET' });
      return reasonListSchema.parse(body).reasons;
    },
    // The set changes when somebody edits it, which is not a per-page concern.
    staleTime: 5 * 60 * 1000,
  });
}

/** Which operation a write is doing, which decides which reasons apply. */
export type WriteOperation = 'create' | 'amend' | 'delete';

/**
 * The reason that means "touched, but nothing changed".
 *
 * It is the one reason whose meaning depends on whether the form actually
 * differs from the record, and getting it wrong records a material change as a
 * touch or the reverse. Named here because both the dialog and its caller need
 * to agree on which one it is.
 */
export const NON_MATERIAL_REASON = 'common.non_material_update';

/**
 * The reasons offered for an operation, given whether anything changed.
 *
 * A create offers every reason the server marks as applying to a new record. An
 * amend or a delete offers the non-material reason only when nothing changed,
 * and every other reason only when something did. That asymmetry is the rule
 * that keeps the audit trail honest: a person cannot file a material change as a
 * touch, or a touch as a material change.
 */
export function reasonsFor(
  reasons: readonly ChangeReason[],
  operation: WriteOperation,
  hasChanges: boolean,
): readonly ChangeReason[] {
  const applies =
    operation === 'create'
      ? (r: ChangeReason) => r.appliesToNew
      : operation === 'amend'
        ? (r: ChangeReason) => r.appliesToAmend
        : (r: ChangeReason) => r.appliesToDelete;

  const applicable = reasons.filter(applies).sort(byDisplayOrder);

  /*
   * The diff decides which reasons apply to an amendment, and only to an
   * amendment.
   *
   * "Nothing material changed" is a statement about an edit, so it is not a
   * reason to delete anything, and a create is unaffected for the same reason:
   * nothing has changed yet. Applying the rule to a delete once offered *no*
   * reasons at all and the dialog rendered empty.
   */
  if (operation !== 'amend') return applicable;

  const isNonMaterial = (reason: ChangeReason): boolean =>
    reason.code === NON_MATERIAL_REASON;
  const matching = applicable.filter((reason) =>
    hasChanges ? !isNonMaterial(reason) : isNonMaterial(reason),
  );

  /*
   * A filter that matches nothing offers everything rather than nothing.
   *
   * The reasons are data, and this deployment has no non-material reason at all:
   * the code the rule is built on is absent. Filtering to it therefore left an
   * amendment with an empty dialog and no way to save — the same failure the
   * delete path had, arriving by a different route. An empty selector is never
   * the right answer; the reason set the server offers is.
   */
  return matching.length > 0 ? matching : applicable;
}

/**
 * By display order, which is the field that means it.
 *
 * The order is deliberate: the reasons people reach for most sit first, and the
 * catch-alls carry a sentinel order of 1000 so they sink to the bottom without
 * the author having to number everything between them.
 */
function byDisplayOrder(a: ChangeReason, b: ChangeReason): number {
  return a.displayOrder - b.displayOrder;
}

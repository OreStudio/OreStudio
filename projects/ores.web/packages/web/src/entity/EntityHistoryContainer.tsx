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

import { useState, type ReactNode } from 'react';
import { useNavigate, useParams } from 'react-router';
import { ChangeReasonDialog, type ChangeReasonResult } from './ChangeReasonDialog.js';
import { EntityHistoryPage, type HistoryVersion } from './EntityHistoryPage.js';
import { entityRecordPath } from './entityPaths.js';
import { useEntityHistory, useSaveEntity, type EntityRow } from './useEntity.js';
import { useChangeReasons } from '../api/changeReasons.js';
import { useTranslation } from '../i18n/Provider.js';
import { Notice } from '../ui/Primitives.js';
import { usePageCrumbLabel } from '../components/PageCrumb.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * The history of any one record.
 *
 * The versions come from the service already ordered, and the screen is the
 * shared one. What is here is the projection from the declaration's field names
 * to the shape a history is rendered from, which is the same projection for
 * every entity because the wire record already uses those names.
 */
export function EntityHistoryContainer({
  descriptor,
}: {
  readonly descriptor: EntityDescriptor;
}): ReactNode {
  const params = useParams();
  const key = params[descriptor.keyParam];
  const { t } = useTranslation();
  const navigate = useNavigate();

  const query = useEntityHistory(descriptor, key);
  const reasons = useChangeReasons();
  const save = useSaveEntity(descriptor);

  /*
   * Reverting writes the record again from an earlier version.
   *
   * The values come from the version being restored and the version number from
   * the record as it stands, because that number is the optimistic lock: sending
   * the old one would be asking to overwrite a record that has moved on since.
   * Nothing is erased -- the result is a new version, which is what makes a
   * revert safe to do and safe to undo.
   */
  const [reverting, setReverting] = useState<EntityRow | undefined>(undefined);
  const [failure, setFailure] = useState<string | undefined>(undefined);

  function revert(result: ChangeReasonResult): void {
    const target = reverting;
    const current = query.data?.[0];
    if (target === undefined || current === undefined) return;
    setFailure(undefined);
    /*
     * A version states its field values as the history rendered them, one
     * entry per field, so the record a revert sends is built from those rather
     * than from the version row -- which carries the provenance, not the
     * entity. Only the members a write record states are taken.
     */
    const rendered = new Map<string, unknown>(
      ((target['fields'] ?? []) as readonly { name: string; value: unknown }[])
        .map((field) => [field.name, field.value]),
    );
    const data: Record<string, unknown> = {};
    for (const name of descriptor.writeFields) {
      if (rendered.has(name)) data[name] = rendered.get(name);
    }
    save.mutate(
      {
        data,
        // The current version, because that number is the optimistic lock:
        // sending the reverted version would ask to overwrite a record that
        // has moved on since.
        version: Number(current['version'] ?? 0),
        intent: {
          reason_code: result.reasonCode,
          commentary: result.commentary,
        },
      },
      {
        onSuccess: () => {
          setReverting(undefined);
          void query.refetch();
        },
        onError: (error: unknown) => {
          setFailure(error instanceof Error ? error.message : t('feedback.saveFailed'));
          setReverting(undefined);
        },
      },
    );
  }

  const versions: readonly HistoryVersion[] = (query.data ?? []).map((row) => ({
    version: Number(row['version'] ?? 0),
    modifiedBy: String(row['modified_by'] ?? ''),
    performedBy: String(row['performed_by'] ?? ''),
    recordedAt: String(row['recorded_at'] ?? ''),
    changeReasonCode: String(row['change_reason_code'] ?? ''),
    changeCommentary: String(row['change_commentary'] ?? ''),
    wire: row,
    values: row,
  }));

  // The name the record goes by, taken from its most recent version, so the
  // breadcrumb says the name rather than the key.
  const newest = query.data?.[0];
  const name =
    String(newest?.[descriptor.meta.displayField] ?? '') || String(key ?? '');
  usePageCrumbLabel(name);

  if (failure !== undefined) {
    return (
      <div className="mx-auto max-w-[680px] px-5 py-10">
        <Notice tone="error">{failure}</Notice>
      </div>
    );
  }

  return (
    <>
      <EntityHistoryPage
        meta={descriptor.meta}
        title={name}
        versions={versions}
        loading={query.isPending}
        failed={query.isError}
        onRetry={() => void query.refetch()}
        // Opening a version is reading it, which is a route rather than a mode, so
        // the back button means something.
        onOpenVersion={() => navigate(entityRecordPath(descriptor, String(key ?? '')))}
        recordName={name}
        {...(descriptor.capabilities.edit
          ? {
              onRevert: (version: HistoryVersion) => {
                if (version.wire !== undefined) setReverting(version.wire as EntityRow);
              },
            }
          : {})}
      />

      {reverting !== undefined && (
        <ChangeReasonDialog
          operation="amend"
          // The values change, so the reasons are the ones for a real change.
          hasChanges
          reasons={reasons.data ?? []}
          pending={save.isPending}
          onConfirm={revert}
          onCancel={() => setReverting(undefined)}
        />
      )}
    </>
  );
}

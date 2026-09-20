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
import { useNavigate } from 'react-router';
import { ChangeReasonDialog, type ChangeReasonResult } from './ChangeReasonDialog.js';
import { ConfirmDialog } from './ConfirmDialog.js';
import { EntityListPage } from './EntityListPage.js';
import { entityBasePath, entityRecordPath } from './entityPaths.js';
import { useDeleteEntity, useEntityList, type EntityRow } from './useEntity.js';
import { useChangeReasons } from '../api/changeReasons.js';
import { useTranslation } from '../i18n/Provider.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * The list of any entity, driven by its declaration.
 *
 * This is what an entity used to write for itself. The screen is the shared one
 * and the data is the shared hooks; what is here is the wiring between them, and
 * it reads entirely from the descriptor. Paging, searching, the create action,
 * the row actions and the two-step delete are the same for every entity, so they
 * are written once.
 *
 * A capability the entity does not declare produces no action. The list does not
 * ask whether it should offer Edit: the declaration has already answered.
 */
export function EntityListContainer({
  descriptor,
}: {
  readonly descriptor: EntityDescriptor;
}): ReactNode {
  const { t } = useTranslation();
  const navigate = useNavigate();

  // Small enough that a screenful is readable and a person can find a record
  // without scrolling through a hundred of them.
  const [page, setPage] = useState(1);
  const [pageSize, setPageSize] = useState(25);

  const query = useEntityList(descriptor, { page, pageSize });
  const reasons = useChangeReasons();
  const remove = useDeleteEntity(descriptor);

  /*
   * Deleting asks twice, and the two questions are asked separately.
   *
   * First whether, because the record's name is what makes the question
   * answerable. Then why, because the reason is a different question, and folding
   * it into the confirmation is how a reason gets chosen without being read.
   */
  const [target, setTarget] = useState<EntityRow | undefined>(undefined);
  const [stage, setStage] = useState<'confirm' | 'reason' | undefined>(undefined);
  const [failure, setFailure] = useState<string | undefined>(undefined);

  const base = entityBasePath(descriptor);
  /** The record's identity, which is the field the declaration freezes. */
  const keyOf = (row: EntityRow): string => String(row[descriptor.meta.keyField] ?? '');

  function confirmDelete(result: ChangeReasonResult): void {
    const row = target;
    if (row === undefined) return;
    setFailure(undefined);
    remove.mutate(
      { key: keyOf(row), reason: result.reasonCode, commentary: result.commentary },
      {
        onSuccess: () => {
          setTarget(undefined);
          setStage(undefined);
        },
        onError: (error: unknown) => {
          // The server's own words, because it knew something we did not.
          setFailure(error instanceof Error ? error.message : t('feedback.deleteFailed'));
          setStage(undefined);
        },
      },
    );
  }

  return (
    <>
      <EntityListPage
        meta={descriptor.meta}
        title={t(`${descriptor.entity}.title`)}
        description={t(`${descriptor.entity}.description`)}
        rows={query.data?.rows ?? []}
        totalCount={query.data?.totalCount ?? 0}
        query={query}
        page={page}
        pageSize={pageSize}
        // Changing the page size resets to the first page, because the page you
        // were on may not exist at the new size.
        onPageChange={setPage}
        onPageSizeChange={(size) => {
          setPageSize(size);
          setPage(1);
        }}
        onReload={() => void query.refetch()}
        onOpen={(row) => navigate(entityRecordPath(descriptor, keyOf(row)))}
        {...(descriptor.capabilities.create
          ? { onCreate: () => navigate(`${base}/new`) }
          : {})}
        {...(descriptor.capabilities.edit
          ? {
              onEdit: (row: EntityRow) =>
                navigate(`${entityRecordPath(descriptor, keyOf(row))}/edit`),
            }
          : {})}
        {...(descriptor.capabilities.history
          ? {
              onHistory: (row: EntityRow) =>
                navigate(`${entityRecordPath(descriptor, keyOf(row))}/history`),
            }
          : {})}
        {...(descriptor.capabilities.remove
          ? {
              onDelete: (row: EntityRow) => {
                setFailure(undefined);
                setTarget(row);
                setStage('confirm');
              },
            }
          : {})}
        searchFields={descriptor.searchFields}
        searchPlaceholderKey={`${descriptor.entity}.searchPlaceholder`}
        collectionName={t(`${descriptor.entity}.title`)}
        watchedAs={{ component: descriptor.component, entity: descriptor.entity }}
        {...(failure === undefined ? {} : { failureMessage: failure })}
      />

      {target !== undefined && stage === 'confirm' && (
        <ConfirmDialog
          title={t('confirmation.deleteTitle', { singular: t(`${descriptor.entity}.singular`) })}
          body={t('confirmation.deleteBody', {
            singular: t(`${descriptor.entity}.singular`),
            name: keyOf(target),
          })}
          confirmLabel={t('entity.delete')}
          pending={false}
          onCancel={() => {
            setTarget(undefined);
            setStage(undefined);
          }}
          onConfirm={() => setStage('reason')}
        />
      )}

      {target !== undefined && stage === 'reason' && (
        <ChangeReasonDialog
          operation="delete"
          // Nothing about the record changed, so the delete reasons apply.
          hasChanges={false}
          reasons={reasons.data ?? []}
          pending={remove.isPending}
          onConfirm={confirmDelete}
          onCancel={() => setStage(undefined)}
        />
      )}
    </>
  );
}

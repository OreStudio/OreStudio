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
import {
  ChangeReasonDialog,
  type ChangeReasonResult,
} from './ChangeReasonDialog.js';
import { ConfirmDialog } from './ConfirmDialog.js';
import { EntityListPage } from './EntityListPage.js';
import {
  entityBasePath,
  entityRecordPath,
  recordKeyFromValues,
  recordLabel,
} from './entityPaths.js';
import { useDeleteEntity, useEntityList, type EntityRow } from './useEntity.js';
import { useChangeReasons } from '../api/changeReasons.js';
import { useTranslation } from '../i18n/Provider.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * The list of any entity, driven by its declaration.
 *
 * The screen is the shared one and the data is the shared hooks; what is here is
 * the wiring between them, and it reads entirely from the descriptor. Paging,
 * searching, the create action, the row actions and the two-step delete are the
 * same for every entity, so they are written once.
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
  /*
   * The point in time the page is read as of, empty for the present. It is
   * state of the screen rather than a query parameter of the route, because
   * looking at an earlier page and looking at last quarter are two questions
   * and only one of them is worth a link.
   */
  const [asOf, setAsOf] = useState('');

  const query = useEntityList(descriptor, { page, pageSize, asOf });
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


  function confirmDelete(result: ChangeReasonResult): void {
    const row = target;
    if (row === undefined) return;
    setFailure(undefined);
    const version = Number(row['version']);
    remove.mutate(
      {
        key: recordKeyFromValues(descriptor, row),
        intent: {
          reason_code: result.reasonCode,
          commentary: result.commentary,
        },
        version: Number.isFinite(version) ? version : undefined,
      },
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
        keyFields={descriptor.keyFields}
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
        onOpen={(row) => navigate(entityRecordPath(descriptor, row))}
        {...(descriptor.capabilities.create
          ? { onCreate: () => navigate(`${base}/new`) }
          : {})}
        {...(descriptor.capabilities.edit
          ? {
              onEdit: (row: EntityRow) =>
                navigate(`${entityRecordPath(descriptor, row)}/edit`),
            }
          : {})}
        {...(descriptor.capabilities.history
          ? {
              onHistory: (row: EntityRow) =>
                navigate(`${entityRecordPath(descriptor, row)}/history`),
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
        {...(descriptor.capabilities.asOf
          ? {
              asOf,
              // A different instant is a different page: the one the person
              // was on may not exist as of the window they just chose.
              onAsOfChange: (value: string) => {
                setAsOf(value);
                setPage(1);
              },
            }
          : {})}
        collectionName={t(`${descriptor.entity}.title`)}
        watchedAs={{ component: descriptor.component, entity: descriptor.entity }}
        {...(failure === undefined ? {} : { failureMessage: failure })}
      />

      {target !== undefined && stage === 'confirm' && (
        <ConfirmDialog
          title={t('confirmation.deleteTitle', { singular: t(`${descriptor.entity}.singular`) })}
          body={t('confirmation.deleteBody', {
            singular: t(`${descriptor.entity}.singular`),
            name: recordLabel(descriptor, target),
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

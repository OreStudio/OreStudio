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
import { EntityListPage } from '../../../../entity/EntityListPage.js';
import { ConfirmDialog } from '../../../../entity/ConfirmDialog.js';
import {
  ChangeReasonDialog,
  type ChangeReasonResult,
} from '../../../../entity/ChangeReasonDialog.js';
import { useCountries, useDeleteCountry, type CountryRow } from '../../../../api/countries.js';
import { useChangeReasons } from '../../../../api/changeReasons.js';
import { countryMeta } from '../../../../generated/refdata/ui/country_ui.js';
import { useTranslation } from '../../../../i18n/Provider.js';

/**
 * The country list.
 *
 * The whole screen is the shared list plus a declaration: there is no table, no
 * paging control, no search box and no state machine here. What an entity adds is
 * the wiring of its own actions, and that is all that is below.
 */
export function CountryListPage(): ReactNode {
  const { t } = useTranslation();
  const navigate = useNavigate();

  // Small enough that a screenful is readable and a person can find a record
  // without scrolling through a hundred of them.
  const [page, setPage] = useState(1);
  const [pageSize, setPageSize] = useState(25);

  const query = useCountries({ page, pageSize });
  const reasons = useChangeReasons();
  const remove = useDeleteCountry();

  /*
   * Deleting asks twice, and the two questions are asked separately.
   *
   * First whether, because the record's name is what makes the question
   * answerable. Then why, because the reason is a different question, and folding
   * it into the confirmation is how a reason gets chosen without being read.
   */
  const [target, setTarget] = useState<CountryRow | undefined>(undefined);
  const [stage, setStage] = useState<'confirm' | 'reason' | undefined>(undefined);
  const [failure, setFailure] = useState<string | undefined>(undefined);

  function confirmDelete(result: ChangeReasonResult): void {
    const row = target;
    if (row === undefined) return;
    void result;
    setFailure(undefined);
    remove.mutate(String(row['alpha2_code'] ?? ''), {
      onSuccess: () => {
        setTarget(undefined);
        setStage(undefined);
      },
      onError: (error: unknown) => {
        // The server's own words, because it knew something we did not.
        setFailure(error instanceof Error ? error.message : t('feedback.deleteFailed'));
        setStage(undefined);
      },
    });
  }

  return (
    <>
      <EntityListPage
        meta={countryMeta}
        title={t('country.title')}
        description={t('country.description')}
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
        onOpen={(row) => navigate(`/refdata/country/${String(row['alpha2_code'] ?? '')}`)}
        onCreate={() => navigate('/refdata/country/new')}
        onEdit={(row) => navigate(`/refdata/country/${String(row['alpha2_code'] ?? '')}/edit`)}
        onHistory={(row) =>
          navigate(`/refdata/country/${String(row['alpha2_code'] ?? '')}/history`)
        }
        onDelete={(row) => {
          setFailure(undefined);
          setTarget(row);
          setStage('confirm');
        }}
        searchFields={['alpha2_code', 'alpha3_code', 'numeric_code', 'name', 'official_name']}
        searchPlaceholderKey="country.searchPlaceholder"
        collectionName={t('country.title')}
        watchedAs={{ component: 'refdata', entity: 'country' }}
        {...(failure === undefined ? {} : { failureMessage: failure })}
      />

      {target !== undefined && stage === 'confirm' && (
        <ConfirmDialog
          title={t('confirmation.deleteTitle', { singular: t('country.singular') })}
          body={t('confirmation.deleteBody', {
            singular: t('country.singular'),
            name: String(target['alpha2_code'] ?? ''),
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

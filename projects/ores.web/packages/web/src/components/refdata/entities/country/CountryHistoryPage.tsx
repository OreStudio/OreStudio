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
import {
  ChangeReasonDialog,
  type ChangeReasonResult,
} from '../../../../entity/ChangeReasonDialog.js';
import { useSaveCountry } from '../../../../api/countries.js';
import { useChangeReasons } from '../../../../api/changeReasons.js';
import { Notice } from '../../../../ui/Primitives.js';
import { EntityHistoryPage, type HistoryVersion } from '../../../../entity/EntityHistoryPage.js';
import { countryMeta } from '../../../../generated/refdata/ui/country_ui.js';
import { useCountryHistory } from '../../../../api/countries.js';
import { useTranslation } from '../../../../i18n/Provider.js';
import type { WireCountry } from '@ores/wire-protocol/browser';
import { usePageCrumbLabel } from '../../../PageCrumb.js';

/**
 * The history of one country.
 *
 * The versions come from the service already ordered, and the screen is the
 * shared one. What is here is only the mapping from this entity's records to the
 * shape a history is rendered from.
 */
export function CountryHistoryPage(): ReactNode {
  const { id } = useParams<{ id: string }>();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const query = useCountryHistory(id);
  const reasons = useChangeReasons();
  const save = useSaveCountry();

  /*
   * Reverting writes the record again from an earlier version.
   *
   * The values come from the version being restored and the version number from
   * the record as it stands, because that number is the optimistic lock: sending
   * the old one would be asking to overwrite a record that has moved on since.
   * Nothing is erased — the result is a new version, which is what makes a
   * revert safe to do and safe to undo.
   */
  const [reverting, setReverting] = useState<WireCountry | undefined>(undefined);
  const [failure, setFailure] = useState<string | undefined>(undefined);

  function revert(result: ChangeReasonResult): void {
    const target = reverting;
    const current = query.data?.versions[0];
    if (target === undefined || current === undefined) return;
    setFailure(undefined);
    save.mutate(
      {
        data: { ...target, version: current.version },
        reasonCode: result.reasonCode,
        commentary: result.commentary,
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

  // The name the record goes by, taken from its most recent version, so the
  // breadcrumb says Argentina rather than AR.
  const newest = query.data?.versions[0];
  usePageCrumbLabel(newest?.name);

  const versions: readonly HistoryVersion[] = (query.data?.versions ?? []).map((version) => ({
    version: version.version,
    modifiedBy: version.modifiedBy,
    performedBy: version.performedBy,
    recordedAt: version.recordedAt,
    changeReasonCode: version.changeReasonCode,
    changeCommentary: version.changeCommentary,
    wire: version.wire,
    values: {
      alpha2_code: version.alpha2Code,
      alpha3_code: version.alpha3Code,
      numeric_code: version.numericCode,
      name: version.name,
      official_name: version.officialName,
      version: version.version,
      modified_by: version.modifiedBy,
      recorded_at: version.recordedAt,
    },
  }));

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
      meta={countryMeta}
      /*
       * The record's own name, as the detail screen uses.
       *
       * `country.singular` is deliberately lower case: it exists to sit inside a
       * sentence, as in "Delete country?". Using it as a heading is how the title
       * read "country AD" — a fragment of a sentence and an identifier, in the
       * one place a person looks to see what they are looking at.
       */
      title={newest?.name ?? String(id ?? '')}
      versions={versions}
      loading={query.isPending}
      failed={query.isError}
      onRetry={() => void query.refetch()}
      // Opening a version is reading it, which is a route rather than a mode, so
      // the back button means something.
      onOpenVersion={() => navigate(`/refdata/country/${String(id ?? '')}`)}
      recordName={newest?.name ?? String(id ?? '')}
      onRevert={(version) => {
        if (version.wire !== undefined) setReverting(version.wire as WireCountry);
      }}
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

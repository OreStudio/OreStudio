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
import type { FeedStart } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import { Notice } from '../../ui/Primitives.js';
import { formatDateTime } from '../../ui/Time.js';

/**
 * Where a run's starting value came from, in one sentence: the fixing it took,
 * with the value as the publisher wrote it, the publisher's date and the time it
 * arrived; or that no fixing set the level and the run kept its configured one.
 * The item itself is drawn as the oresmd URI it is, so a reader can open the
 * series the value came from.
 */
export function FeedStartNotice({ start }: { readonly start: FeedStart }): ReactNode {
    const { t, language } = useTranslation();
    const received =
        start.received_at === null || start.received_at === undefined
            ? t('marketdata.fxFeeds.start.receivedUnknown')
            : formatDateTime(start.received_at, language);
    return (
        <Notice tone="info">
            <p className="font-medium">{t('marketdata.fxFeeds.start.title')}</p>
            <p>
                {start.from_fixing
                    ? t('marketdata.fxFeeds.start.fromFixing', {
                          provider: start.provider_code,
                          value: start.value,
                          date:
                              start.fixing_date === null || start.fixing_date === undefined
                                  ? t('marketdata.fxFeeds.start.dateUnknown')
                                  : start.fixing_date,
                          received,
                      })
                    : t('marketdata.fxFeeds.start.fixedLevel')}
            </p>
            {start.from_fixing && start.item !== '' && <OresmdUrl uri={start.item} />}
            {start.from_fixing && start.inverted && <p>{t('marketdata.fxFeeds.start.inverted')}</p>}
            {start.from_fixing && start.used > 0 && (
                <p>
                    {t('marketdata.fxFeeds.start.window', {
                        used: String(start.used),
                        days: String(start.window_days),
                    })}
                </p>
            )}
            {start.note !== '' && <p>{start.note}</p>}
        </Notice>
    );
}

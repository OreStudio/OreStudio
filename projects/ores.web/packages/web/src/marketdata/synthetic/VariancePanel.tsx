/* -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 */

import { type ReactNode } from 'react';
import { Link } from 'react-router';
import { type FeedStart, type RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { show } from '../../refdata/records.js';
import { calibrationFeedPath } from '../../shell/paths.js';
import { Fact } from '../../ui/Primitives.js';
import { FeedStartVariance } from './feedStart.js';

/**
 * One feed's daily variance: whether it is constant or follows a GJR-GARCH, the reviewed
 * calibration the feed is pointed at, and the shape the running feed carries from it.
 *
 * The tab only reads. A feed's shape comes from a calibration of its pair that a person
 * approved on the Calibration screen, never from values typed on the feed, so there is
 * nothing here to edit; the link opens that calibration, its evidence and its review.
 */
export function VarianceTab({ feed }: { readonly feed: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const model = show(feed['variance_model']) === 'gjr_garch' ? 'gjr_garch' : 'constant';
    const calibration = show(feed['variance_calibration_id']);
    // The feed screen reads the running feed's start with the record.
    const running = (feed as { readonly start?: FeedStart | null }).start?.variance;
    const assetClass = show(feed['asset_class_code']) || 'fx';
    return (
        <section className="space-y-4" aria-label={t('marketdata.variance.title')}>
            <p className="text-sm text-ink-muted">{t('marketdata.variance.lead')}</p>
            <dl className="grid gap-4 sm:grid-cols-2">
                <Fact
                    label={t('marketdata.variance.model')}
                    value={t(`marketdata.variance.model.${model}`)}
                />
                <Fact
                    label={t('marketdata.variance.calibration')}
                    value={
                        model === 'gjr_garch' && calibration !== ''
                            ? calibration
                            : t('marketdata.variance.calibrationNone')
                    }
                />
            </dl>
            {model === 'gjr_garch' && (
                <div className="space-y-2">
                    <h3 className="text-sm font-medium text-ink">
                        {t('marketdata.variance.running')}
                    </h3>
                    {running !== null && running !== undefined ? (
                        <div className="text-sm text-ink-muted">
                            <FeedStartVariance variance={running} />
                        </div>
                    ) : (
                        <p className="text-sm text-ink-muted">
                            {t('marketdata.variance.notRunning')}
                        </p>
                    )}
                    <Link
                        className="text-sm text-accent hover:underline"
                        to={`${calibrationFeedPath(assetClass, show(feed['id']))}?tab=variance`}
                    >
                        {t('marketdata.variance.openCalibration')}
                    </Link>
                </div>
            )}
            <p className="text-sm text-ink-muted">{t('marketdata.variance.calibrationLead')}</p>
        </section>
    );
}

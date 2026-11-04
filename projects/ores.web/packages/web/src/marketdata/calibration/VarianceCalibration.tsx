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

import { useMutation } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { api, type VarianceCalibration } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import { Button, Fact, Field, Notice, Tag, Textarea } from '../../ui/Primitives.js';

/** The decimals a GARCH number is read to: enough to tell one fit from another. */
export const garchText = (value: number | null | undefined): string =>
    value === null || value === undefined ? '' : value.toFixed(4);

/**
 * A shape's parameters as a table of name and value. Its names keep their case, so the
 * Greek letter each one carries reads as written; a fact's caption would capitalise it.
 */
export function ParameterTable({
    rows,
}: {
    readonly rows: readonly { readonly label: string; readonly value: string }[];
}): ReactNode {
    return (
        <table className="w-full text-sm" data-parameter-table>
            <tbody>
                {rows.map((row) => (
                    <tr key={row.label} className="border-b border-line-subtle last:border-b-0">
                        <th
                            scope="row"
                            className="py-1.5 pr-4 text-left font-normal text-ink-muted"
                        >
                            {row.label}
                        </th>
                        <td className="py-1.5 text-right font-mono tabular-nums text-ink">
                            {row.value}
                        </td>
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

/** A GJR-GARCH shape as the rows of a parameter table: alpha, gamma, beta and persistence. */
export function garchRows(
    t: (key: string) => string,
    shape: {
        readonly alpha?: number | null | undefined;
        readonly gamma?: number | null | undefined;
        readonly beta?: number | null | undefined;
        readonly persistence?: number | null | undefined;
    },
): { label: string; value: string }[] {
    return [
        { label: t('marketdata.calibration.variance.alpha'), value: garchText(shape.alpha) },
        { label: t('marketdata.calibration.variance.gamma'), value: garchText(shape.gamma) },
        { label: t('marketdata.calibration.variance.beta'), value: garchText(shape.beta) },
        {
            label: t('marketdata.calibration.variance.persistence'),
            value: garchText(shape.persistence),
        },
    ];
}

const STATE_TONES: Readonly<Record<string, 'up' | 'down' | 'warn' | 'muted' | 'accent'>> = {
    approved: 'up',
    proposed: 'accent',
    rejected: 'down',
    superseded: 'muted',
    failed: 'down',
};

/** A calibration's state as a tag: proposed, approved, rejected, superseded or failed. */
export function VarianceStateTag({ state }: { readonly state: string }): ReactNode {
    const { t } = useTranslation();
    if (state === '') {
        return null;
    }
    return (
        <Tag tone={STATE_TONES[state] ?? 'neutral'} small>
            {t(`marketdata.calibration.variance.state.${state}`)}
        </Tag>
    );
}

/**
 * Everything a reviewer judges a variance calibration by: the shape, its persistence and its
 * starting variance; the fit's evidence, with a warning when the starts disagree; and the input
 * it was fitted from, so the numbers can be traced to the fixings that made them.
 */
export function VarianceCalibrationFacts({
    variance,
}: {
    readonly variance: VarianceCalibration;
}): ReactNode {
    const { t } = useTranslation();
    const shaped = variance.alpha !== null && variance.alpha !== undefined;
    // Only a fit has starts; an authored shape was written, so it has none to agree.
    const fitted = shaped && variance.method === 'gjr_garch_qmle';
    return (
        <div className="space-y-4" data-variance-calibration>
            <div className="flex flex-wrap items-center gap-2">
                <VarianceStateTag state={variance.state} />
                {variance.method !== '' && (
                    <Tag tone="muted" small>
                        {t(`marketdata.calibration.variance.method.${variance.method}`)}
                    </Tag>
                )}
                {variance.state_reason !== '' && (
                    <span className="text-sm text-ink-muted">{variance.state_reason}</span>
                )}
            </div>
            {shaped ? (
                <div className="grid gap-4 lg:grid-cols-2">
                    <ParameterTable rows={garchRows(t, variance)} />
                    <dl className="grid content-start gap-4 sm:grid-cols-2">
                        <Fact
                            label={t('marketdata.calibration.variance.startingRatio')}
                            value={garchText(variance.starting_variance_ratio)}
                        />
                        {fitted && (
                            <Fact
                                label={t('marketdata.calibration.variance.starts')}
                                value={t(
                                    variance.starts_agreed
                                        ? 'marketdata.calibration.variance.startsAgreed'
                                        : 'marketdata.calibration.variance.startsDisagreed',
                                    { starts: String(variance.starts) },
                                )}
                            />
                        )}
                    </dl>
                </div>
            ) : null}
            {fitted && !variance.starts_agreed && (
                <Notice tone="warn">{t('marketdata.calibration.variance.disagreeWarning')}</Notice>
            )}
            {variance.item !== '' && (
                <dl className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
                    <Fact
                        label={t('marketdata.calibration.variance.returns')}
                        value={t('marketdata.calibration.variance.returnsOf', {
                            used: String(variance.used_count),
                            points: String(variance.point_count),
                        })}
                    />
                    <Fact
                        label={t('marketdata.calibration.variance.window')}
                        value={t('marketdata.calibration.variance.windowOf', {
                            days: String(variance.window_days),
                            end: variance.window_end,
                        })}
                    />
                    <Fact
                        label={t('marketdata.calibration.variance.provider')}
                        value={variance.provider_code}
                    />
                    <Fact
                        label={t('marketdata.calibration.variance.orientation')}
                        value={t(
                            variance.inverted
                                ? 'marketdata.calibration.variance.inverted'
                                : 'marketdata.calibration.variance.direct',
                        )}
                    />
                </dl>
            )}
            {variance.item !== '' && (
                <Fact label={t('marketdata.calibration.variance.item')}>
                    <OresmdUrl uri={variance.item} />
                </Fact>
            )}
            <dl className="grid gap-4 sm:grid-cols-2">
                <Fact
                    label={t('marketdata.calibration.variance.calibrationId')}
                    value={variance.calibration_id}
                />
                <Fact
                    label={t('marketdata.calibration.variance.inputId')}
                    value={variance.input_id}
                />
            </dl>
        </div>
    );
}

/**
 * Approve or reject a proposed calibration, with the reviewer's reason. Approving points the
 * pair's GJR-GARCH feeds at it, so the reason is required either way and kept with the decision.
 */
export function VarianceReview({
    variance,
    onReviewed,
}: {
    readonly variance: Pick<VarianceCalibration, 'calibration_id' | 'state'>;
    readonly onReviewed: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [reason, setReason] = useState('');
    const review = useMutation({
        mutationFn: (decision: 'approve' | 'reject') =>
            api.reviewVarianceCalibration(variance.calibration_id, decision, reason.trim()),
        onSuccess: () => {
            setReason('');
            onReviewed();
        },
    });
    if (variance.state !== 'proposed' || variance.calibration_id === '') {
        return null;
    }
    const missing = reason.trim() === '';
    return (
        <div className="space-y-3" data-variance-review>
            <Field
                label={t('marketdata.calibration.variance.reviewReason')}
                hint={t('marketdata.calibration.variance.reviewHint')}
            >
                <Textarea
                    rows={2}
                    value={reason}
                    onChange={(event) => setReason(event.target.value)}
                />
            </Field>
            <div className="flex flex-wrap gap-3">
                <Button
                    variant="primary"
                    disabled={missing}
                    pending={review.isPending && review.variables === 'approve'}
                    onClick={() => review.mutate('approve')}
                >
                    {t('marketdata.calibration.variance.approve')}
                </Button>
                <Button
                    disabled={missing}
                    pending={review.isPending && review.variables === 'reject'}
                    onClick={() => review.mutate('reject')}
                >
                    {t('marketdata.calibration.variance.reject')}
                </Button>
            </div>
            {review.data !== undefined && <Notice tone="success">{review.data.message}</Notice>}
            {review.error !== null && <Notice tone="error">{review.error.message}</Notice>}
        </div>
    );
}

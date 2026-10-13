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
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Input, Notice, Select } from '../ui/Primitives.js';
import { problemsOf, type DraftComponent } from './mixture.js';
import { PROFILES, profileOf, type ProcessType, type Profile } from './processHelp.js';

const TEXTAREA =
    'w-full rounded-md border border-line bg-surface-base px-3 py-2 text-sm text-ink ' +
    'placeholder:text-ink-faint hover:border-line-strong focus:border-accent ' +
    'focus:outline-none focus:ring-3 focus:ring-accent/20';

/**
 * One component of the mixture, edited on its own. Done puts the values back
 * into the mixture on the screen; nothing is saved until the mixture is.
 */
export function ComponentDialog({
    component,
    index,
    adding,
    processType,
    onDone,
    onClose,
}: {
    readonly component: DraftComponent;
    readonly index: number;
    readonly adding: boolean;
    readonly processType: ProcessType;
    readonly onDone: (component: DraftComponent) => void;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [values, setValues] = useState(component);
    const set = (change: Partial<DraftComponent>): void => setValues({ ...values, ...change });
    const problems = problemsOf([values]).filter(
        (problem) => problem === 'number' || problem === 'negative',
    );
    const ou = processType === 'ornstein_uhlenbeck';
    const help = (field: 'mean' | 'stdev' | 'weight'): string =>
        t(`marketdata.process.columns.${processType}.${field}`);

    return (
        <Dialog
            wide
            title={
                adding
                    ? t('marketdata.mixture.addTitle')
                    : t('marketdata.mixture.editTitle', { index: String(index) })
            }
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={problems.length > 0}
                        onClick={() => onDone(values)}
                    >
                        {t('marketdata.mixture.done')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <p className="text-sm text-ink-muted">
                    {t(`marketdata.process.row.${processType}`)}
                </p>
                <Field
                    label={t('refdata.fields.description')}
                    hint={t('marketdata.mixture.descriptionHint')}
                >
                    <textarea
                        className={TEXTAREA}
                        rows={4}
                        maxLength={2000}
                        value={values.description}
                        onChange={(event) => set({ description: event.target.value })}
                    />
                </Field>
                <div className="grid gap-4 sm:grid-cols-2">
                    <Field
                        label={t('marketdata.process.profile')}
                        hint={t('marketdata.process.profileHelp')}
                    >
                        <Select
                            value={profileOf(values.stdev)}
                            onChange={(event) => {
                                const chosen = event.target.value;
                                if (chosen in PROFILES) {
                                    set({ stdev: String(PROFILES[chosen as Profile]) });
                                }
                            }}
                        >
                            {(['custom', ...Object.keys(PROFILES)] as const).map((profile) => (
                                <option key={profile} value={profile}>
                                    {t(`marketdata.process.profiles.${profile}`)}
                                </option>
                            ))}
                        </Select>
                    </Field>
                    <Field label={t('refdata.fields.mean')} hint={help('mean')}>
                        <Input
                            className="font-mono"
                            inputMode="decimal"
                            value={values.mean}
                            onChange={(event) => set({ mean: event.target.value })}
                        />
                    </Field>
                    <Field label={t('refdata.fields.stdev')} hint={help('stdev')}>
                        <Input
                            className="font-mono"
                            inputMode="decimal"
                            value={values.stdev}
                            onChange={(event) => set({ stdev: event.target.value })}
                        />
                    </Field>
                    <Field
                        label={ou ? t('marketdata.process.kappa') : t('refdata.fields.weight')}
                        hint={help('weight')}
                    >
                        <Input
                            className="font-mono"
                            inputMode="decimal"
                            value={values.weight}
                            onChange={(event) => set({ weight: event.target.value })}
                        />
                    </Field>
                </div>
                {problems.map((problem) => (
                    <Notice key={problem} tone="warn">
                        {t(`marketdata.mixture.problems.${problem}`)}
                    </Notice>
                ))}
            </div>
        </Dialog>
    );
}

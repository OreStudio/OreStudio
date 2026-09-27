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

import { useState, type FormEvent, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Field, Input, Notice } from '../ui/Primitives.js';
import { NewPasswordField } from '../ui/PasswordField.js';
import { heroSplash } from '../assets/brand.js';
import type { CreateAdministratorRequest } from '@ores/wire-protocol/browser';

/**
 * The screen a browser shows while the deployment has no administrator.
 *
 * This is the only screen in bootstrap mode, which is why the gate sends every
 * route here rather than redirecting to a setup path: there is nothing else to
 * be at, and a redirect would leave a URL somebody could share that leads
 * nowhere.
 *
 * It is the first thing an empty installation ever shows, so it carries the
 * banner and reads like an installation screen: the condition, the consequence,
 * and the one action that changes it. That action is the first administrator
 * account, and the server closes bootstrap mode when it succeeds, so this screen
 * does not decide when the deployment is ready — it asks again.
 *
 * The password rules shown while typing are the rules the server enforces,
 * because the shared field carries the same policy the validator does.
 */
export interface SetupPageProps {
    readonly message: string;
    readonly onCreate: (request: CreateAdministratorRequest) => Promise<void>;
}

export function SetupPage({ message, onCreate }: SetupPageProps): ReactNode {
    const { t } = useTranslation();
    const [principal, setPrincipal] = useState('');
    const [email, setEmail] = useState('');
    const [password, setPassword] = useState('');
    const [passwordAcceptable, setPasswordAcceptable] = useState(false);
    const [failure, setFailure] = useState<string | undefined>(undefined);
    const [busy, setBusy] = useState(false);

    const complete = principal !== '' && email !== '' && passwordAcceptable;

    const submit = async (event: FormEvent): Promise<void> => {
        event.preventDefault();
        setFailure(undefined);
        setBusy(true);
        try {
            await onCreate({ principal, password, email });
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setBusy(false);
        }
    };

    return (
        <div className="card overflow-hidden">
            {/* The banner the landing page uses, so the first screen of an
                installation looks like the product rather than like a notice. */}
            <img src={heroSplash} alt="" className="w-full border-b border-line" />
            <div className="p-6">
                <h1 className="text-lg font-semibold text-ink">{t('setup.title')}</h1>
                <p className="mt-3 text-sm text-ink-muted">{t('setup.bootstrapMode')}</p>
                {message !== '' && (
                    <div className="mt-4">
                        <Notice tone="info">{message}</Notice>
                    </div>
                )}
                <p className="mt-4 text-sm text-ink-muted">{t('setup.next')}</p>

                {failure !== undefined && (
                    <div className="mt-4">
                        <Notice tone="error">{`${t('setup.failed')} ${failure}`}</Notice>
                    </div>
                )}

                <form className="mt-6 space-y-4" onSubmit={(event) => void submit(event)}>
                    <Field label={t('setup.username')}>
                        <Input
                            value={principal}
                            autoComplete="username"
                            onChange={(event) => setPrincipal(event.target.value)}
                        />
                    </Field>
                    <Field label={t('setup.email')}>
                        <Input
                            type="email"
                            value={email}
                            autoComplete="email"
                            onChange={(event) => setEmail(event.target.value)}
                        />
                    </Field>
                    <NewPasswordField
                        label={t('setup.password')}
                        value={password}
                        onChange={(value, acceptable) => {
                            setPassword(value);
                            setPasswordAcceptable(acceptable);
                        }}
                    />
                    <div className="flex justify-end">
                        <Button
                            type="submit"
                            variant="primary"
                            disabled={!complete}
                            pending={busy}
                            pendingLabel={t('setup.creating')}
                        >
                            {t('setup.create')}
                        </Button>
                    </div>
                </form>
            </div>
        </div>
    );
}

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

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { enFlat } from '../i18n/locales/en.js';
import { createTranslator } from '../i18n/translate.js';
import {
    detailsComplete,
    EMPTY_DRAFT,
    signUpRequest,
    signUpSteps,
    type SignUpDraft,
} from './signUpSteps.js';
import { indexOfStep, type JourneyStep } from './runtime.js';
import type {
    PasswordPolicy,
    RegistrationPolicyView,
    SignupResult,
} from '@ores/wire-protocol/browser';

const rules: PasswordPolicy = {
    success: true,
    message: '',
    minLength: 12,
    requireUppercase: true,
    requireLowercase: true,
    requireDigit: true,
    requireSpecial: true,
    specialChars: '!@#$%^&*()_+-=[]{}|;:,.<>?',
};

const destination: RegistrationPolicyView = {
    success: true,
    message: '',
    errorCode: '',
    signupsEnabled: true,
    authorizationRequired: false,
    tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
    tenantName: 'Northwind Capital',
    partyId: '22222222-2222-2222-2222-222222222222',
    partyName: 'Northwind Operations',
    roleId: '44444444-4444-4444-4444-444444444444',
    roleName: 'Viewer',
    usableNow: true,
};

const draft: SignUpDraft = {
    principal: 'jdoe',
    email: 'jdoe@northwind.example.com',
    password: 'Secret-1!entropy',
    confirmation: 'Secret-1!entropy',
};

/*
 * The catalogue itself, so the steps are asserted on the words a person reads
 * rather than on the keys that produced them.
 */
const t = createTranslator('en', enFlat, enFlat).t;

function build(
    overrides: Partial<Parameters<typeof signUpSteps>[0]> = {},
): readonly JourneyStep<ReactNode>[] {
    return signUpSteps({
        t,
        policy: destination,
        draft,
        passwordPolicy: rules,
        passwordAcceptable: true,
        onChange: () => undefined,
        onPasswordAcceptable: () => undefined,
        onCreate: async () => undefined,
        outcome: undefined,
        ...overrides,
    });
}

function body(steps: readonly JourneyStep<ReactNode>[], id: string): string {
    return renderToStaticMarkup(
        <TranslationProvider>{steps[indexOfStep(steps, id)]?.body}</TranslationProvider>,
    );
}

describe('the details step', () => {
    it('will not move on until the details are complete', () => {
        expect(detailsComplete(EMPTY_DRAFT, false)).toBe(false);
        expect(detailsComplete(draft, true)).toBe(true);
    });

    it('will not move on when the password is unacceptable or the pair differs', () => {
        expect(detailsComplete(draft, false)).toBe(false);
        expect(detailsComplete({ ...draft, confirmation: 'something else' }, true)).toBe(false);
        expect(detailsComplete({ ...draft, email: 'not-an-address' }, true)).toBe(false);
    });

    it('says so when the two passwords do not match', () => {
        const html = body(build({ draft: { ...draft, confirmation: 'other' } }), 'details');

        expect(html).toContain('The two passwords do not match.');
    });

    it('sends only what the person typed, with the edges trimmed', () => {
        expect(signUpRequest({ ...draft, principal: ' jdoe ' })).toEqual({
            principal: 'jdoe',
            email: 'jdoe@northwind.example.com',
            password: 'Secret-1!entropy',
        });
    });
});

describe('the review step', () => {
    it('reads back where the account lands, which is not a question', () => {
        const html = body(build(), 'review');

        expect(html).toContain('Northwind Capital');
        expect(html).toContain('Northwind Operations');
        expect(html).toContain('Viewer');
        expect(html).toContain('You can sign in as soon as the account exists.');
    });

    it('says the account will wait when the tenant nominates no party', () => {
        const html = body(
            build({
                policy: { ...destination, partyId: '', partyName: '', usableNow: false },
            }),
            'review',
        );

        expect(html).toContain('Not yet. An administrator adds you to one.');
        expect(html).toContain('An administrator must add you to a party');
        expect(html).not.toContain('You can sign in as soon as the account exists.');
    });
});

describe('the confirmation step', () => {
    const active: SignupResult = {
        success: true,
        message: '',
        errorCode: '',
        accountId: '55555555-5555-5555-5555-555555555555',
        accountStatus: 'active',
        partyId: '22222222-2222-2222-2222-222222222222',
        roleId: '44444444-4444-4444-4444-444444444444',
    };

    it('names what a usable account received', () => {
        const steps = build({ outcome: active });
        const step = steps[indexOfStep(steps, 'confirmation')];

        expect(step?.title).toBe('Account created');
        expect(body(steps, 'confirmation')).toContain(
            'It holds the Viewer role in Northwind Capital.',
        );
    });

    it('says what a waiting account waits for', () => {
        const steps = build({ outcome: { ...active, accountStatus: 'pending', partyId: '' } });
        const step = steps[indexOfStep(steps, 'confirmation')];

        expect(step?.title).toBe('Account waiting');
        expect(body(steps, 'confirmation')).toContain('It waits for a party to work in.');
    });
});

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
import { MemoryRouter } from 'react-router';
import { TranslationProvider } from '../i18n/Provider.js';
import { enFlat } from '../i18n/locales/en.js';
import { createTranslator } from '../i18n/translate.js';
import { JourneySplash } from './parts.js';
import {
    doorState,
    SignUpDoor,
    type SignUpDoorProps,
    type SignUpReading,
} from './SignUpJourney.js';
import { EMPTY_DRAFT, signUpSteps } from './signUpSteps.js';
import type { PasswordPolicy, RegistrationPolicyView } from '@ores/wire-protocol/browser';

const policy: RegistrationPolicyView = {
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

const t = createTranslator('en', enFlat, enFlat).t;

function buildSteps(policy_: RegistrationPolicyView = policy): SignUpDoorProps['steps'] {
    return signUpSteps({
        t,
        policy: policy_,
        draft: EMPTY_DRAFT,
        passwordPolicy: rules,
        passwordAcceptable: false,
        onChange: () => undefined,
        onPasswordAcceptable: () => undefined,
        onCreate: async () => undefined,
        outcome: undefined,
    });
}

function renderDoor(props: Partial<SignUpDoorProps> & Pick<SignUpDoorProps, 'state'>): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>
                <SignUpDoor
                    steps={buildSteps()}
                    at={0}
                    onMove={() => undefined}
                    onRetry={() => undefined}
                    {...props}
                />
            </MemoryRouter>
        </TranslationProvider>,
    );
}

describe('what the door read decides', () => {
    it('is loading until both answers arrive', () => {
        expect(doorState({})).toEqual({ kind: 'loading' });
        expect(doorState({ policy })).toEqual({ kind: 'loading' });
    });

    it('is closed when the deployment refuses, keeping the code and the reason', () => {
        const closed: RegistrationPolicyView = {
            ...policy,
            success: false,
            errorCode: 'signup_disabled',
            message: 'This deployment does not accept registrations.',
        };

        expect(doorState({ policy: closed, passwordPolicy: rules })).toEqual({
            kind: 'closed',
            code: 'signup_disabled',
            message: 'This deployment does not accept registrations.',
        });
    });

    it('is open when the deployment accepts, handing both answers on', () => {
        const state = doorState({ policy, passwordPolicy: rules });

        expect(state.kind).toBe('open');
    });

    it('is a failure, not a closed door, when nothing answered', () => {
        const reading: SignUpReading = { failure: '503: no broker' };

        expect(doorState(reading)).toEqual({ kind: 'failed', reason: '503: no broker' });
    });
});

describe('the registration door', () => {
    it('renders the rail and the banner together on an open door', () => {
        const html = renderDoor({ state: { kind: 'open', policy, passwordPolicy: rules } });

        // The rail, from JourneyPage, and the same banner element the setup
        // journeys carry: a layout change that dropped either would fail here.
        expect(html).toContain('<nav');
        expect(html).toContain('Your details');
        expect(html).toContain('Review');
        expect(html).toContain('width="964"');
        expect(html).toContain('height="323"');
    });

    it('draws the banner with the ratio nothing may crop', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <JourneySplash />
            </TranslationProvider>,
        );

        expect(html).toContain('width="964"');
        expect(html).toContain('height="323"');
        expect(html).toContain('h-auto w-full');
        // A stated height that is not the ratio's is what cut the wordmark.
        expect(html).not.toContain('max-height');
        expect(html).not.toContain('h-[');
    });

    it('states which closure it is, and offers the way back to the door', () => {
        const html = renderDoor({
            state: {
                kind: 'closed',
                code: 'no_default_role',
                message: 'The tenant has not said what a new account may do.',
            },
        });

        expect(html).toContain('Registration is closed');
        expect(html).toContain('The tenant has not said what a new account may do.');
        expect(html).toContain('/login');
        expect(html).not.toContain('<nav');
        expect(html).toContain('width="964"');
    });

    it('says when the deployment could not be asked, and offers to ask again', () => {
        const html = renderDoor({ state: { kind: 'failed', reason: '503: no broker' } });

        expect(html).toContain('The deployment could not say whether it accepts registrations.');
        expect(html).toContain('503: no broker');
        expect(html).toContain('Try again');
    });
});

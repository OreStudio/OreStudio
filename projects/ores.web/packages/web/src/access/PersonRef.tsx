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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { imageUrl } from '../ui/Images.js';
import { useHolds } from './holds.js';
import { actorPathFor } from './PeoplePage.js';

/** A person as the directory knows them. */
export interface KnownPerson {
    readonly id: string;
    readonly username: string;
    readonly fullName: string;
    /** The identifier of the person's picture, or null when they have none. */
    readonly imageId: string | null;
}

const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/**
 * The people the reader can name, from the widest read they hold.
 *
 * A reader of accounts names anybody in the tenant. A reader of the
 * organisation names the people in their parties and under them. Anybody else
 * names nobody, and a person is then shown as the username the screen was given.
 */
export function usePeopleDirectory(): {
    readonly find: (who: string) => KnownPerson | undefined;
    readonly mayOpen: boolean;
} {
    const holds = useHolds();
    const mayReadAccounts = holds('iam::accounts:read');
    const mayReadOrganisation = holds('iam::organisation:read');
    const accounts = useQuery({
        queryKey: ['accounts'],
        queryFn: api.accounts,
        enabled: mayReadAccounts,
        meta: { quiet: true },
    });
    const organisation = useQuery({
        queryKey: ['reporting-tree'],
        queryFn: () => api.reportingTree(),
        enabled: !mayReadAccounts && mayReadOrganisation,
        meta: { quiet: true },
    });
    const people: readonly KnownPerson[] = mayReadAccounts
        ? (accounts.data?.accounts ?? [])
        : (organisation.data?.nodes ?? []).map((node) => ({
              id: node.accountId,
              username: node.username,
              fullName: node.fullName,
              imageId: node.imageId,
          }));
    return {
        find: (who) => people.find((person) => person.username === who || person.id === who),
        mayOpen: mayReadAccounts,
    };
}

/**
 * A person named for a reader: their full name with the username in brackets,
 * opening their page when the reader may open people.
 *
 * Some screens are given a username and some an account id. An id nobody can
 * resolve is a person outside the reader's view, and is said so, because an
 * identifier tells a reader nothing.
 */
export function PersonRef({ who }: { readonly who: string }): ReactNode {
    const { t } = useTranslation();
    const directory = usePeopleDirectory();
    if (who === '') return null;
    const person = directory.find(who);
    if (person === undefined && UUID.test(who)) {
        return <span className="text-ink-muted">{t('people.outsideView')}</span>;
    }
    const username = person?.username ?? who;
    const label =
        person === undefined || person.fullName === ''
            ? username
            : `${person.fullName} (${username})`;
    const path = actorPathFor(directory.mayOpen)(username);
    return path === undefined ? (
        <span>{label}</span>
    ) : (
        <Link
            to={path}
            className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
        >
            {label}
        </Link>
    );
}

/**
 * The picture a person who wrote something is drawn with: the address of their
 * picture, or null for their initials. A name the directory cannot resolve, such
 * as a service, has no picture and is drawn as initials too.
 */
export function useActorPictures(): (actor: string) => string | null {
    const directory = usePeopleDirectory();
    return (actor) => {
        const imageId = directory.find(actor)?.imageId ?? null;
        return imageId === null ? null : imageUrl(imageId);
    };
}

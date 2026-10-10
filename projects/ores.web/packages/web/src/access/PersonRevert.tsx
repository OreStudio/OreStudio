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

import { useQueryClient } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { fieldValue, type HistoryVersion, type TimelineEvent } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { RevertVersionDialog, type WriteIntent } from '../refdata/records.js';

/** The entities of a person's story whose versions a screen can write back. */
export const ACCOUNT_ENTITY = 'ores.iam.account';
export const CONTACT_ENTITY = 'ores.iam.account_contact_information';

/**
 * Whether a timeline entry is a version of the account or its contact record.
 *
 * Those two are the entries the profile and contact writes can restore. A
 * grant and a sign-in are not versions of a record the screen writes.
 */
export function isRevertable(event: TimelineEvent): boolean {
    return event.entityType === ACCOUNT_ENTITY || event.entityType === CONTACT_ENTITY;
}

/** The newest version the story holds of one entity, which is the record as it stands. */
export function latestVersion(events: readonly TimelineEvent[], entityType: string): number {
    return events
        .filter((event) => event.entityType === entityType)
        .reduce((newest, event) => Math.max(newest, event.version), 0);
}

/** A timeline entry as the history version the revert dialog names. */
function versionOf(event: TimelineEvent): HistoryVersion {
    return {
        version: event.version,
        modifiedBy: event.actor,
        recordedAt: event.at,
        fields: [...event.fields],
        changes: [],
    };
}

/**
 * Writes an older version of the account or its contact record back as a new
 * version, with the reason the person picks.
 *
 * The write goes through the same routes as an edit, so a person reverts
 * exactly what they may edit, and the server still refuses what the
 * permission does not cover. The contact write names the version the story
 * showed, so a record that moved since is refused rather than overwritten.
 */
export function PersonRevertDialog({
    event,
    current,
    username,
    accountId,
    me,
    onClose,
}: {
    readonly event: TimelineEvent;
    readonly current: number;
    readonly username: string;
    readonly accountId: string;
    readonly me: boolean;
    readonly onClose: () => void;
}): ReactNode {
    const queries = useQueryClient();
    const value = (name: string): string => fieldValue(event.fields, name);

    const write = async (intent: WriteIntent): Promise<void> => {
        if (event.entityType === ACCOUNT_ENTITY) {
            const profile = {
                fullName: value('Full Name'),
                jobTitle: value('Job Title'),
                imageId: value('Image ID'),
                ...intent,
            };
            if (me) {
                const view = await api.saveMyProfile(profile);
                if (view.result.outcome !== 'ok') throw new Error(view.result.message);
            } else {
                await api.saveAccountProfile(username, profile);
            }
        } else {
            const contact = {
                streetLine1: value('Street Line 1'),
                streetLine2: value('Street Line 2'),
                city: value('City'),
                state: value('State'),
                countryCode: value('Country Code'),
                postalCode: value('Postal Code'),
                phone: value('Phone'),
                email: value('Email'),
                webPage: value('Web Page'),
                ...intent,
            };
            const view = me
                ? await api.saveMyContactInformation(contact)
                : await api.saveAccountContactInformation(accountId, {
                      ...contact,
                      version: current,
                  });
            if (view.result.outcome !== 'ok') throw new Error(view.result.message);
        }
        await queries.invalidateQueries({ queryKey: ['account', username] });
        await queries.invalidateQueries({ queryKey: ['contact-information'] });
        await queries.invalidateQueries({ queryKey: ['timeline', 'person', username] });
    };

    return (
        <RevertVersionDialog
            current={current}
            version={versionOf(event)}
            write={write}
            onClose={onClose}
        />
    );
}

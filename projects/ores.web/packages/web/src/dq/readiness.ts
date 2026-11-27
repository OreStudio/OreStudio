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
 */

import type { NotReadyReason } from '../api/dq.js';

/**
 * The words a DQ screen gives each reason a dataset cannot publish, and the
 * owner of each.
 *
 * The reason and its owner belong together: a screen that states the reason and
 * not who fixes it sends the person to the wrong place. Both screens that draw a
 * dataset read them from here, so one wording reaches all of them.
 */
export const REASON_KEY: Record<NotReadyReason, string> = {
    'no dataset': 'dq.coverage.reason.noDataset',
    'no artefact type': 'dq.coverage.reason.noArtefactType',
    'no target subject': 'dq.coverage.reason.noTargetSubject',
    'no staged rows': 'dq.coverage.reason.noStagedRows',
};

/** The owner of each reason, because a reason without one sends nobody anywhere. */
export const OWNER_KEY: Record<NotReadyReason, string> = {
    'no dataset': 'dq.coverage.owner.dq',
    'no artefact type': 'dq.coverage.owner.dq',
    'no target subject': 'dq.coverage.owner.dq',
    'no staged rows': 'dq.coverage.owner.importer',
};

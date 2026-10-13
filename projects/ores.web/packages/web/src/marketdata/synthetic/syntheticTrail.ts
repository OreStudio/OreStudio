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

import { useTranslation } from '../../i18n/Provider.js';
import type { TrailStep } from '../../shell/AreaTrail.js';
import { PATHS } from '../../shell/paths.js';

/**
 * The Synthetic folder as a step of a trail. Every screen of the folder, and
 * every page of a feed, has it between Market data and itself.
 */
export function useSyntheticStep(): TrailStep {
    const { t } = useTranslation();
    return { label: t('marketdata.synthetic.title'), to: PATHS.synthetic };
}

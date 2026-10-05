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

import type { ReactNode } from 'react';
import add from '../assets/icons/ic_fluent_add_20_regular.svg';
import arrowClockwise from '../assets/icons/ic_fluent_arrow_clockwise_20_regular.svg';
import arrowRotateCounterclockwise from '../assets/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg';
import copy from '../assets/icons/ic_fluent_copy_20_regular.svg';
import deleteIcon from '../assets/icons/ic_fluent_delete_20_regular.svg';
import dismiss from '../assets/icons/ic_fluent_dismiss_20_regular.svg';
import edit from '../assets/icons/ic_fluent_edit_20_regular.svg';
import filter from '../assets/icons/ic_fluent_filter_20_regular.svg';
import history from '../assets/icons/ic_fluent_history_20_regular.svg';
import linkDismiss from '../assets/icons/ic_fluent_link_dismiss_20_regular.svg';
import save from '../assets/icons/ic_fluent_save_20_regular.svg';
import search from '../assets/icons/ic_fluent_search_20_regular.svg';

/**
 * The actions a record screen draws, each with its one icon, as the record
 * screen standard's icon table sets them. A concept missing here is added
 * here and to the icon reference before a screen uses it.
 */
const ICONS = {
    add,
    cancel: dismiss,
    copy,
    delete: deleteIcon,
    edit,
    filter,
    history,
    refresh: arrowClockwise,
    remove: linkDismiss,
    revert: arrowRotateCounterclockwise,
    save,
    search,
} as const;

export type IconName = keyof typeof ICONS;

/**
 * One icon, drawn in the colour of the text around it. The artwork is used as
 * a mask, so a button's hover and disabled colours reach the icon too.
 */
export function Icon({
    name,
    size = 20,
}: {
    readonly name: IconName;
    readonly size?: 16 | 20;
}): ReactNode {
    const url = `url("${ICONS[name]}")`;
    return (
        <span
            aria-hidden
            className="inline-block shrink-0 bg-current"
            style={{
                width: size,
                height: size,
                maskImage: url,
                WebkitMaskImage: url,
                maskSize: 'contain',
                WebkitMaskSize: 'contain',
                maskRepeat: 'no-repeat',
                WebkitMaskRepeat: 'no-repeat',
            }}
        />
    );
}

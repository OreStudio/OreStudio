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
import alert from '../assets/icons/ic_fluent_alert_20_regular.svg';
import arrowClockwise from '../assets/icons/ic_fluent_arrow_clockwise_20_regular.svg';
import arrowRotateCounterclockwise from '../assets/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg';
import copy from '../assets/icons/ic_fluent_copy_20_regular.svg';
import deleteIcon from '../assets/icons/ic_fluent_delete_20_regular.svg';
import dismiss from '../assets/icons/ic_fluent_dismiss_20_regular.svg';
import edit from '../assets/icons/ic_fluent_edit_20_regular.svg';
import filter from '../assets/icons/ic_fluent_filter_20_regular.svg';
import history from '../assets/icons/ic_fluent_history_20_regular.svg';
import linkDismiss from '../assets/icons/ic_fluent_link_dismiss_20_regular.svg';
import lockClosed from '../assets/icons/ic_fluent_lock_closed_20_regular.svg';
import save from '../assets/icons/ic_fluent_save_20_regular.svg';
import access from '../assets/icons/ic_fluent_key_multiple_20_regular.svg';
import apps from '../assets/icons/ic_fluent_apps_20_regular.svg';
import arrowSync from '../assets/icons/ic_fluent_arrow_sync_20_regular.svg';
import arrowTrending from '../assets/icons/ic_fluent_arrow_trending_20_regular.svg';
import bus from '../assets/icons/ic_fluent_flash_flow_20_regular.svg';
import calendar from '../assets/icons/ic_fluent_calendar_clock_20_regular.svg';
import checkmarkCircle from '../assets/icons/ic_fluent_checkmark_circle_20_regular.svg';
import chart from '../assets/icons/ic_fluent_chart_multiple_20_regular.svg';
import classification from '../assets/icons/ic_fluent_classification_20_regular.svg';
import currency from '../assets/icons/ic_fluent_currency_dollar_euro_20_regular.svg';
import database from '../assets/icons/ic_fluent_database_20_regular.svg';
import desk from '../assets/icons/ic_fluent_briefcase_20_regular.svg';
import document from '../assets/icons/ic_fluent_document_table_20_regular.svg';
import log from '../assets/icons/ic_fluent_notepad_20_regular.svg';
import party from '../assets/icons/ic_fluent_building_bank_20_regular.svg';
import people from '../assets/icons/ic_fluent_people_team_20_regular.svg';
import person from '../assets/icons/ic_fluent_person_accounts_20_regular.svg';
import record from '../assets/icons/ic_fluent_record_20_regular.svg';
import server from '../assets/icons/ic_fluent_server_link_20_regular.svg';
import unlock from '../assets/icons/ic_fluent_lock_open_20_regular.svg';
import search from '../assets/icons/ic_fluent_search_20_regular.svg';

/**
 * The actions a record screen draws, each with its one icon, as the record
 * screen standard's icon table sets them. A concept missing here is added
 * here and to the icon reference before a screen uses it.
 */
const ICONS = {
    access,
    add,
    alert,
    apps,
    bus,
    calendar,
    cancel: dismiss,
    chart,
    classification,
    current: checkmarkCircle,
    copy,
    currency,
    database,
    delete: deleteIcon,
    desk,
    document,
    edit,
    filter,
    history,
    locked: lockClosed,
    log,
    pairs: arrowSync,
    party,
    people,
    person,
    record,
    refresh: arrowClockwise,
    remove: linkDismiss,
    revert: arrowRotateCounterclockwise,
    save,
    search,
    server,
    trend: arrowTrending,
    unlock,
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

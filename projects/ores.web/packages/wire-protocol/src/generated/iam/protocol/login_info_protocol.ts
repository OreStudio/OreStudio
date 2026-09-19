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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { LoginInfo } from '../domain/login_info.js';

export interface GetLoginInfoRequest {
    offset: number;
    limit: number;
}

export interface GetLoginInfoResponse {
    login_info: LoginInfo[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveLoginInfoRequest {
    data: LoginInfo;
}

export interface SaveLoginInfoResponse {
    success: boolean;
    message: string;
}

export interface DeleteLoginInfoRequest {
    ids: string[];
}

export interface DeleteLoginInfoResponse {
    success: boolean;
    message: string;
}

export const subjects = {
    get_login_info_request: "iam.v1.login_info.list",
    save_login_info_request: "iam.v1.login_info.save",
    delete_login_info_request: "iam.v1.login_info.delete",
} as const;

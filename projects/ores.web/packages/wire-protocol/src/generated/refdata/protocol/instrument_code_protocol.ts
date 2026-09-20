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
import type { InstrumentCode } from '../domain/instrument_code.js';

export interface GetInstrumentCodesRequest {
    offset: number;
    limit: number;
}

export interface GetInstrumentCodesResponse {
    instruments: InstrumentCode[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveInstrumentCodeRequest {
    data: InstrumentCode;
}

export interface SaveInstrumentCodeResponse {
    success: boolean;
    message: string;
}

export interface DeleteInstrumentCodeRequest {
    codes: string[];
}

export interface DeleteInstrumentCodeResponse {
    success: boolean;
    message: string;
}

export interface GetInstrumentCodeHistoryRequest {
    code: string;
}

export interface GetInstrumentCodeHistoryResponse {
    history: InstrumentCode[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_instrument_codes_request: "refdata.v1.instrument_codes.list",
    save_instrument_code_request: "refdata.v1.instrument_codes.save",
    delete_instrument_code_request: "refdata.v1.instrument_codes.delete",
    get_instrument_code_history_request: "refdata.v1.instrument_codes.history",
} as const;

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
 * The TypeScript twins of the trading records an export item carries.
 *
 * The C++ types are hand-written structs, not codegen models, so no
 * `ores.ts.domain` facet emits them. The generated trade operations protocol
 * imports these interfaces instead. Keep the members in step with
 * `projects/ores.trading/api/include/ores.trading.api/domain/`.
 */

/**
 * One AdditionalFields entry of an ORE trade envelope, in document order.
 */
export interface TradeEnvelopeField {
    name: string;
    value: string;
}

/**
 * The trade-level data of an ORE envelope that the instrument tables do not
 * hold. A member is null when the document states no such element.
 */
export interface TradeEnvelopeData {
    counter_party: string | null;
    netting_set_id: string | null;
    portfolio_ids: string[] | null;
    additional_fields: TradeEnvelopeField[] | null;
}

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#ifndef ORES_TRADING_DOMAIN_SWAP_LEG_HPP
#define ORES_TRADING_DOMAIN_SWAP_LEG_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>

namespace ores::trading::domain {

// Decomposed into plain nested sub-structs (≤9 fields each) so that
// rfl::internal::no_duplicate_field_names never sees more than 9 field names
// at once, staying below MSVC's C1202 template-graph limit.
// See doc/investigations/msvc_c1202_rfl_complexity.org for full analysis.

struct swap_leg_identity final {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid id;
    boost::uuids::uuid instrument_id;
    int leg_number = 1;
};

struct swap_leg_terms final {
    std::string leg_type_code;
    std::string day_count_fraction_code;
    std::string business_day_convention_code;
    std::string payment_frequency_code;
    std::string floating_index_code;
    double fixed_rate = 0.0;
    double spread = 0.0;
    double notional = 0.0;
    std::string currency;
};

/**
 * @brief One leg of a rates instrument (Swap, CrossCurrencySwap, CapFloor,
 * Swaption).
 *
 * Access fields via the sub-struct members:
 *   sl.identity.id, sl.identity.leg_number
 *   sl.terms.leg_type_code, sl.terms.notional
 *   sl.audit.modified_by, sl.audit.recorded_at
 *
 * JSON wire format is nested: {"identity":{...},"terms":{...},"audit":{...}}.
 */
struct swap_leg final {
    swap_leg_identity identity;
    swap_leg_terms terms;
    dq::domain::audit_record audit;
};

}

#endif

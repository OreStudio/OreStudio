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
#include "ores.trading.core/repository/credit_instrument_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/credit_instrument_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::credit_instrument
credit_instrument_mapper::map(const credit_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::credit_instrument r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.version = v.version;
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.reference_entity = v.reference_entity;
    r.currency = v.currency;
    r.notional = v.notional;
    r.spread = v.spread;
    r.recovery_rate = v.recovery_rate;
    r.tenor = v.tenor;
    r.start_date = v.start_date;
    r.maturity_date = v.maturity_date;
    r.day_count_code = v.day_count_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.index_name = v.index_name.value_or("");
    r.index_series = v.index_series.value_or(0);
    r.seniority = v.seniority.value_or("");
    r.restructuring = v.restructuring.value_or("");
    r.description = v.description.value_or("");
    r.option_type = v.option_type.value_or("");
    r.option_expiry_date = v.option_expiry_date.value_or("");
    r.option_strike = v.option_strike;
    r.linked_asset_code = v.linked_asset_code.value_or("");
    r.tranche_attachment = v.tranche_attachment;
    r.tranche_detachment = v.tranche_detachment;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

credit_instrument_entity
credit_instrument_mapper::map(const domain::credit_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    credit_instrument_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);
    r.version = v.version;
    r.trade_id = v.trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.trade_id)) : std::nullopt;
    r.trade_type_code = v.trade_type_code;
    r.reference_entity = v.reference_entity;
    r.currency = v.currency;
    r.notional = v.notional;
    r.spread = v.spread;
    r.recovery_rate = v.recovery_rate;
    r.tenor = v.tenor;
    r.start_date = v.start_date;
    r.maturity_date = v.maturity_date;
    r.day_count_code = v.day_count_code;
    r.payment_frequency_code = v.payment_frequency_code;
    r.index_name = v.index_name.empty()
        ? std::nullopt : std::optional(v.index_name);
    r.index_series = v.index_series == 0
        ? std::nullopt : std::optional(v.index_series);
    r.seniority = v.seniority.empty()
        ? std::nullopt : std::optional(v.seniority);
    r.restructuring = v.restructuring.empty()
        ? std::nullopt : std::optional(v.restructuring);
    r.description = v.description.empty()
        ? std::nullopt : std::optional(v.description);
    r.option_type = v.option_type.empty()
        ? std::nullopt : std::optional(v.option_type);
    r.option_expiry_date = v.option_expiry_date.empty()
        ? std::nullopt : std::optional(v.option_expiry_date);
    r.option_strike = v.option_strike;
    r.linked_asset_code = v.linked_asset_code.empty()
        ? std::nullopt : std::optional(v.linked_asset_code);
    r.tranche_attachment = v.tranche_attachment;
    r.tranche_detachment = v.tranche_detachment;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::credit_instrument>
credit_instrument_mapper::map(const std::vector<credit_instrument_entity>& v) {
    return map_vector<credit_instrument_entity, domain::credit_instrument>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<credit_instrument_entity>
credit_instrument_mapper::map(const std::vector<domain::credit_instrument>& v) {
    return map_vector<domain::credit_instrument, credit_instrument_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}

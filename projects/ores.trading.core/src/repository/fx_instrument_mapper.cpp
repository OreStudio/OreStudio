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
#include "ores.trading.core/repository/fx_instrument_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/fx_instrument_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::fx_instrument
fx_instrument_mapper::map(const fx_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::fx_instrument r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.version = v.version;
    r.trade_type_code = v.trade_type_code;
    r.bought_currency = v.bought_currency;
    r.bought_amount = v.bought_amount;
    r.sold_currency = v.sold_currency;
    r.sold_amount = v.sold_amount;
    r.value_date = v.value_date;
    r.settlement = v.settlement.value_or("");
    r.option_type = v.option_type.value_or("");
    r.strike_price = v.strike_price.value_or(0.0);
    r.expiry_date = v.expiry_date.value_or("");
    r.description = v.description.value_or("");
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

fx_instrument_entity
fx_instrument_mapper::map(const domain::fx_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    fx_instrument_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_type_code = v.trade_type_code;
    r.bought_currency = v.bought_currency;
    r.bought_amount = v.bought_amount;
    r.sold_currency = v.sold_currency;
    r.sold_amount = v.sold_amount;
    r.value_date = v.value_date;
    r.settlement = v.settlement.empty() ? std::nullopt : std::optional(v.settlement);
    r.option_type = v.option_type.empty() ? std::nullopt : std::optional(v.option_type);
    r.strike_price = v.strike_price == 0.0 ? std::nullopt : std::optional(v.strike_price);
    r.expiry_date = v.expiry_date.empty() ? std::nullopt : std::optional(v.expiry_date);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::fx_instrument>
fx_instrument_mapper::map(const std::vector<fx_instrument_entity>& v) {
    return map_vector<fx_instrument_entity, domain::fx_instrument>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<fx_instrument_entity>
fx_instrument_mapper::map(const std::vector<domain::fx_instrument>& v) {
    return map_vector<domain::fx_instrument, fx_instrument_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}

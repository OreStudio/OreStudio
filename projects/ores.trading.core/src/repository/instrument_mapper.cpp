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
#include "ores.trading.core/repository/instrument_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/instrument_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::instrument
instrument_mapper::map(const instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::instrument r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.version = v.version;
    r.trade_type_code = v.trade_type_code;
    r.notional = v.notional;
    r.currency = v.currency;
    r.start_date = v.start_date;
    r.maturity_date = v.maturity_date;
    r.description = v.description.value_or("");
    r.fra_fixing_date = v.fra_fixing_date.value_or("");
    r.fra_settlement_date = v.fra_settlement_date.value_or("");
    r.lockout_days = v.lockout_days;
    r.callable_dates_json = v.callable_dates_json.value_or("");
    r.rpa_counterparty = v.rpa_counterparty.value_or("");
    r.inflation_index_code = v.inflation_index_code.value_or("");
    r.base_cpi = v.base_cpi;
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

instrument_entity
instrument_mapper::map(const domain::instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    instrument_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.trade_type_code = v.trade_type_code;
    r.notional = v.notional;
    r.currency = v.currency;
    r.start_date = v.start_date;
    r.maturity_date = v.maturity_date;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.fra_fixing_date = v.fra_fixing_date.empty()
        ? std::nullopt : std::optional(v.fra_fixing_date);
    r.fra_settlement_date = v.fra_settlement_date.empty()
        ? std::nullopt : std::optional(v.fra_settlement_date);
    r.lockout_days = v.lockout_days;
    r.callable_dates_json = v.callable_dates_json.empty()
        ? std::nullopt : std::optional(v.callable_dates_json);
    r.rpa_counterparty = v.rpa_counterparty.empty()
        ? std::nullopt : std::optional(v.rpa_counterparty);
    r.inflation_index_code = v.inflation_index_code.empty()
        ? std::nullopt : std::optional(v.inflation_index_code);
    r.base_cpi = v.base_cpi;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::instrument>
instrument_mapper::map(const std::vector<instrument_entity>& v) {
    return map_vector<instrument_entity, domain::instrument>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<instrument_entity>
instrument_mapper::map(const std::vector<domain::instrument>& v) {
    return map_vector<domain::instrument, instrument_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}

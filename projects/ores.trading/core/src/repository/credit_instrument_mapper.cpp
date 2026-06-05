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
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/credit_instrument_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::credit_instrument credit_instrument_mapper::map(const credit_instrument_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::credit_instrument r;
    r.identity.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.workspace_id = boost::lexical_cast<boost::uuids::uuid>(v.workspace_id);
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.version = v.version;
    r.identity.trade_id = v.trade_id.has_value() ?
                              std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.trade_id)) :
                              std::nullopt;
    r.identity.trade_type_code = v.trade_type_code;
    r.terms.reference_entity = v.reference_entity;
    r.terms.currency = v.currency;
    r.terms.notional = v.notional;
    r.terms.spread = v.spread;
    r.terms.recovery_rate = v.recovery_rate;
    r.schedule.tenor = v.tenor;
    r.schedule.start_date = v.start_date;
    r.schedule.maturity_date = v.maturity_date;
    r.schedule.day_count_code = v.day_count_code;
    r.schedule.payment_frequency_code = v.payment_frequency_code;
    r.index.index_name = v.index_name.value_or("");
    r.index.index_series = v.index_series.value_or(0);
    r.terms.seniority = v.seniority.value_or("");
    r.terms.restructuring = v.restructuring.value_or("");
    r.description = v.description.value_or("");
    r.option.option_type = v.option_type.value_or("");
    r.option.option_expiry_date = v.option_expiry_date.value_or("");
    r.option.option_strike = v.option_strike;
    r.terms.linked_asset_code = v.linked_asset_code.value_or("");
    r.tranche.tranche_attachment = v.tranche_attachment;
    r.tranche.tranche_detachment = v.tranche_detachment;
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    r.audit.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

credit_instrument_entity credit_instrument_mapper::map(const domain::credit_instrument& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    credit_instrument_entity r;
    r.id = boost::uuids::to_string(v.identity.instrument_id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.workspace_id = boost::uuids::to_string(v.identity.workspace_id);
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.version = v.identity.version;
    r.trade_id = v.identity.trade_id.has_value() ?
                     std::optional(boost::uuids::to_string(*v.identity.trade_id)) :
                     std::nullopt;
    r.trade_type_code = v.identity.trade_type_code;
    r.reference_entity = v.terms.reference_entity;
    r.currency = v.terms.currency;
    r.notional = v.terms.notional;
    r.spread = v.terms.spread;
    r.recovery_rate = v.terms.recovery_rate;
    r.tenor = v.schedule.tenor;
    r.start_date = v.schedule.start_date;
    r.maturity_date = v.schedule.maturity_date;
    r.day_count_code = v.schedule.day_count_code;
    r.payment_frequency_code = v.schedule.payment_frequency_code;
    r.index_name = v.index.index_name.empty() ? std::nullopt : std::optional(v.index.index_name);
    r.index_series = v.index.index_series == 0 ? std::nullopt : std::optional(v.index.index_series);
    r.seniority = v.terms.seniority.empty() ? std::nullopt : std::optional(v.terms.seniority);
    r.restructuring =
        v.terms.restructuring.empty() ? std::nullopt : std::optional(v.terms.restructuring);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.option_type =
        v.option.option_type.empty() ? std::nullopt : std::optional(v.option.option_type);
    r.option_expiry_date = v.option.option_expiry_date.empty() ?
                               std::nullopt :
                               std::optional(v.option.option_expiry_date);
    r.option_strike = v.option.option_strike;
    r.linked_asset_code =
        v.terms.linked_asset_code.empty() ? std::nullopt : std::optional(v.terms.linked_asset_code);
    r.tranche_attachment = v.tranche.tranche_attachment;
    r.tranche_detachment = v.tranche.tranche_detachment;
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::credit_instrument>
credit_instrument_mapper::map(const std::vector<credit_instrument_entity>& v) {
    return map_vector<credit_instrument_entity, domain::credit_instrument>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<credit_instrument_entity>
credit_instrument_mapper::map(const std::vector<domain::credit_instrument>& v) {
    return map_vector<domain::credit_instrument, credit_instrument_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}

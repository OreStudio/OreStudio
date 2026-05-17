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
#include "ores.trading.core/repository/trade_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/trade_json_io.hpp" // IWYU pragma: keep.

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::trade
trade_mapper::map(const trade_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::trade r;
    r.identity.get().version = v.version;
    r.identity.get().tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.get().id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.identity.get().party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.get().external_id = v.external_id.value_or("");
    r.parties.get().book_id = boost::lexical_cast<boost::uuids::uuid>(v.book_id);
    r.parties.get().portfolio_id = boost::lexical_cast<boost::uuids::uuid>(v.portfolio_id);
    r.parties.get().successor_trade_id = v.successor_trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.successor_trade_id)) : std::nullopt;
    r.parties.get().counterparty_id = v.counterparty_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.counterparty_id)) : std::nullopt;
    r.classification.get().trade_type = v.trade_type;
    if (v.product_type) {
        auto pt = domain::product_type_from_string(*v.product_type);
        if (!pt) {
            throw std::logic_error(
                "Invalid product_type in trade entity: '" + *v.product_type + "'");
        }
        r.classification.get().product_type = *pt;
    } else {
        r.classification.get().product_type = domain::product_type::unknown;
    }
    r.classification.get().instrument_id = v.instrument_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.instrument_id)) : std::nullopt;
    r.classification.get().asset_class = v.asset_class;
    r.classification.get().netting_set_id = v.netting_set_id;
    r.classification.get().activity_type_code = v.activity_type_code;
    r.classification.get().status_id = boost::lexical_cast<boost::uuids::uuid>(v.status_id);
    r.lifecycle.get().trade_date = v.trade_date;
    r.lifecycle.get().execution_timestamp = v.execution_timestamp;
    r.lifecycle.get().effective_date = v.effective_date;
    r.lifecycle.get().termination_date = v.termination_date;
    r.audit.get().modified_by = v.modified_by;
    r.audit.get().performed_by = v.performed_by;
    r.audit.get().change_reason_code = v.change_reason_code;
    r.audit.get().change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.audit.get().recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_entity
trade_mapper::map(const domain::trade& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_entity r;
    r.id = boost::uuids::to_string(v.identity.get().id);
    r.tenant_id = v.identity.get().tenant_id.to_string();
    r.version = v.identity.get().version;
    r.party_id = boost::uuids::to_string(v.identity.get().party_id);
    r.external_id = v.identity.get().external_id.empty() ? std::nullopt : std::optional(v.identity.get().external_id);
    r.book_id = boost::uuids::to_string(v.parties.get().book_id);
    r.portfolio_id = boost::uuids::to_string(v.parties.get().portfolio_id);
    r.successor_trade_id = v.parties.get().successor_trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.parties.get().successor_trade_id)) : std::nullopt;
    r.counterparty_id = v.parties.get().counterparty_id.has_value() ? std::optional(boost::uuids::to_string(*v.parties.get().counterparty_id)) : std::nullopt;
    r.trade_type = v.classification.get().trade_type;
    r.product_type = (v.classification.get().product_type == domain::product_type::unknown)
        ? std::nullopt
        : std::optional(std::string(domain::to_string(v.classification.get().product_type)));
    r.instrument_id = v.classification.get().instrument_id.has_value() ? std::optional(boost::uuids::to_string(*v.classification.get().instrument_id)) : std::nullopt;
    r.asset_class = v.classification.get().asset_class;
    r.netting_set_id = v.classification.get().netting_set_id;
    r.activity_type_code = v.classification.get().activity_type_code;
    r.status_id = boost::uuids::to_string(v.classification.get().status_id);
    r.trade_date = v.lifecycle.get().trade_date;
    r.execution_timestamp = v.lifecycle.get().execution_timestamp;
    r.effective_date = v.lifecycle.get().effective_date;
    r.termination_date = v.lifecycle.get().termination_date;
    r.modified_by = v.audit.get().modified_by;
    r.performed_by = v.audit.get().performed_by;
    r.change_reason_code = v.audit.get().change_reason_code;
    r.change_commentary = v.audit.get().change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::trade>
trade_mapper::map(const std::vector<trade_entity>& v) {
    return map_vector<trade_entity, domain::trade>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<trade_entity>
trade_mapper::map(const std::vector<domain::trade>& v) {
    return map_vector<domain::trade, trade_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}

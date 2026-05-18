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
    r.identity.version = v.version;
    r.identity.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.identity.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.identity.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity.external_id = v.external_id.value_or("");
    r.parties.book_id = boost::lexical_cast<boost::uuids::uuid>(v.book_id);
    r.parties.portfolio_id = boost::lexical_cast<boost::uuids::uuid>(v.portfolio_id);
    r.parties.successor_trade_id = v.successor_trade_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.successor_trade_id)) : std::nullopt;
    r.parties.counterparty_id = v.counterparty_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.counterparty_id)) : std::nullopt;
    r.classification.trade_type = v.trade_type;
    if (v.product_type) {
        auto pt = domain::product_type_from_string(*v.product_type);
        if (!pt) {
            throw std::logic_error(
                "Invalid product_type in trade entity: '" + *v.product_type + "'");
        }
        r.classification.product_type = *pt;
    } else {
        r.classification.product_type = domain::product_type::unknown;
    }
    r.classification.instrument_id = v.instrument_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.instrument_id)) : std::nullopt;
    r.classification.asset_class = v.asset_class;
    r.classification.netting_set_id = v.netting_set_id;
    r.classification.activity_type_code = v.activity_type_code;
    r.classification.status_id = boost::lexical_cast<boost::uuids::uuid>(v.status_id);
    r.lifecycle.trade_date = v.trade_date;
    r.lifecycle.execution_timestamp = v.execution_timestamp;
    r.lifecycle.effective_date = v.effective_date;
    r.lifecycle.termination_date = v.termination_date;
    r.audit.modified_by = v.modified_by;
    r.audit.performed_by = v.performed_by;
    r.audit.change_reason_code = v.change_reason_code;
    r.audit.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.audit.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

trade_entity
trade_mapper::map(const domain::trade& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    trade_entity r;
    r.id = boost::uuids::to_string(v.identity.id);
    r.tenant_id = v.identity.tenant_id.to_string();
    r.version = v.identity.version;
    r.party_id = boost::uuids::to_string(v.identity.party_id);
    r.external_id = v.identity.external_id.empty() ? std::nullopt : std::optional(v.identity.external_id);
    r.book_id = boost::uuids::to_string(v.parties.book_id);
    r.portfolio_id = boost::uuids::to_string(v.parties.portfolio_id);
    r.successor_trade_id = v.parties.successor_trade_id.has_value() ? std::optional(boost::uuids::to_string(*v.parties.successor_trade_id)) : std::nullopt;
    r.counterparty_id = v.parties.counterparty_id.has_value() ? std::optional(boost::uuids::to_string(*v.parties.counterparty_id)) : std::nullopt;
    r.trade_type = v.classification.trade_type;
    r.product_type = (v.classification.product_type == domain::product_type::unknown)
        ? std::nullopt
        : std::optional(std::string(domain::to_string(v.classification.product_type)));
    r.instrument_id = v.classification.instrument_id.has_value() ? std::optional(boost::uuids::to_string(*v.classification.instrument_id)) : std::nullopt;
    r.asset_class = v.classification.asset_class;
    r.netting_set_id = v.classification.netting_set_id;
    r.activity_type_code = v.classification.activity_type_code;
    r.status_id = boost::uuids::to_string(v.classification.status_id);
    r.trade_date = v.lifecycle.trade_date;
    r.execution_timestamp = v.lifecycle.execution_timestamp;
    r.effective_date = v.lifecycle.effective_date;
    r.termination_date = v.lifecycle.termination_date;
    r.modified_by = v.audit.modified_by;
    r.performed_by = v.audit.performed_by;
    r.change_reason_code = v.audit.change_reason_code;
    r.change_commentary = v.audit.change_commentary;

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

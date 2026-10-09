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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.core/repository/market_series_identity_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/market_series_identity.hpp"
#include "ores.marketdata.api/domain/market_series_identity_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_series_identity_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::market_series_identity
market_series_identity_mapper::map(const market_series_identity_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::market_series_identity r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.series_id = boost::lexical_cast<boost::uuids::uuid>(v.series_id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.identity_kind = v.identity_kind;
    r.asset_class = v.asset_class.value_or("");
    r.instrument_type = v.instrument_type.value_or("");
    r.quote_type = v.quote_type.value_or("");
    r.atm = v.atm.value_or("");
    r.cap_floor = v.cap_floor.value_or("");
    r.ccy = v.ccy.value_or("");
    r.cds_index_name = v.cds_index_name.value_or("");
    r.commodity_name = v.commodity_name.value_or("");
    r.contract = v.contract.value_or("");
    r.contract_name = v.contract_name.value_or("");
    r.curve_id = v.curve_id.value_or("");
    r.day_counter = v.day_counter.value_or("");
    r.delivery = v.delivery.value_or("");
    r.delivery_end = v.delivery_end.value_or("");
    r.delivery_start = v.delivery_start.value_or("");
    r.doc_clause = v.doc_clause.value_or("");
    r.dst = v.dst.value_or("");
    r.eq_name = v.eq_name.value_or("");
    r.expiry = v.expiry.value_or("");
    r.family = v.family.value_or("");
    r.fixed_ccy = v.fixed_ccy.value_or("");
    r.fixed_tenor = v.fixed_tenor.value_or("");
    r.flat_ccy = v.flat_ccy.value_or("");
    r.flat_term = v.flat_term.value_or("");
    r.float_ccy = v.float_ccy.value_or("");
    r.float_tenor = v.float_tenor.value_or("");
    r.future_contract = v.future_contract.value_or("");
    r.fwd_start = v.fwd_start.value_or("");
    r.identifier = v.identifier.value_or("");
    r.index = v.index.value_or("");
    r.index1 = v.index1.value_or("");
    r.index2 = v.index2.value_or("");
    r.index_name = v.index_name.value_or("");
    r.index_tenor = v.index_tenor.value_or("");
    r.index_term = v.index_term.value_or("");
    r.spread_offset = v.spread_offset.value_or("");
    r.option_type = v.option_type.value_or("");
    r.payer_receiver = v.payer_receiver.value_or("");
    r.qualifier = v.qualifier.value_or("");
    r.quote_name = v.quote_name.value_or("");
    r.quote_tag = v.quote_tag.value_or("");
    r.rating_name = v.rating_name.value_or("");
    r.relative = v.relative.value_or("");
    r.running_spread = v.running_spread.value_or("");
    r.seasonality_type = v.seasonality_type.value_or("");
    r.security_id = v.security_id.value_or("");
    r.seniority = v.seniority.value_or("");
    r.side = v.side.value_or("");
    r.source = v.source.value_or("");
    r.tenor = v.tenor.value_or("");
    r.term = v.term.value_or("");
    r.time_unit = v.time_unit.value_or("");
    r.underlying_name = v.underlying_name.value_or("");
    r.unit_ccy = v.unit_ccy.value_or("");

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

market_series_identity_entity
market_series_identity_mapper::map(const domain::market_series_identity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    market_series_identity_entity r;
    r.series_id = boost::uuids::to_string(v.series_id);
    r.tenant_id = v.tenant_id.to_string();
    r.party_id = boost::uuids::to_string(v.party_id);
    r.identity_kind = v.identity_kind;
    r.asset_class = v.asset_class.empty() ? std::nullopt : std::optional(v.asset_class);
    r.instrument_type = v.instrument_type.empty() ? std::nullopt : std::optional(v.instrument_type);
    r.quote_type = v.quote_type.empty() ? std::nullopt : std::optional(v.quote_type);
    r.atm = v.atm.empty() ? std::nullopt : std::optional(v.atm);
    r.cap_floor = v.cap_floor.empty() ? std::nullopt : std::optional(v.cap_floor);
    r.ccy = v.ccy.empty() ? std::nullopt : std::optional(v.ccy);
    r.cds_index_name = v.cds_index_name.empty() ? std::nullopt : std::optional(v.cds_index_name);
    r.commodity_name = v.commodity_name.empty() ? std::nullopt : std::optional(v.commodity_name);
    r.contract = v.contract.empty() ? std::nullopt : std::optional(v.contract);
    r.contract_name = v.contract_name.empty() ? std::nullopt : std::optional(v.contract_name);
    r.curve_id = v.curve_id.empty() ? std::nullopt : std::optional(v.curve_id);
    r.day_counter = v.day_counter.empty() ? std::nullopt : std::optional(v.day_counter);
    r.delivery = v.delivery.empty() ? std::nullopt : std::optional(v.delivery);
    r.delivery_end = v.delivery_end.empty() ? std::nullopt : std::optional(v.delivery_end);
    r.delivery_start = v.delivery_start.empty() ? std::nullopt : std::optional(v.delivery_start);
    r.doc_clause = v.doc_clause.empty() ? std::nullopt : std::optional(v.doc_clause);
    r.dst = v.dst.empty() ? std::nullopt : std::optional(v.dst);
    r.eq_name = v.eq_name.empty() ? std::nullopt : std::optional(v.eq_name);
    r.expiry = v.expiry.empty() ? std::nullopt : std::optional(v.expiry);
    r.family = v.family.empty() ? std::nullopt : std::optional(v.family);
    r.fixed_ccy = v.fixed_ccy.empty() ? std::nullopt : std::optional(v.fixed_ccy);
    r.fixed_tenor = v.fixed_tenor.empty() ? std::nullopt : std::optional(v.fixed_tenor);
    r.flat_ccy = v.flat_ccy.empty() ? std::nullopt : std::optional(v.flat_ccy);
    r.flat_term = v.flat_term.empty() ? std::nullopt : std::optional(v.flat_term);
    r.float_ccy = v.float_ccy.empty() ? std::nullopt : std::optional(v.float_ccy);
    r.float_tenor = v.float_tenor.empty() ? std::nullopt : std::optional(v.float_tenor);
    r.future_contract = v.future_contract.empty() ? std::nullopt : std::optional(v.future_contract);
    r.fwd_start = v.fwd_start.empty() ? std::nullopt : std::optional(v.fwd_start);
    r.identifier = v.identifier.empty() ? std::nullopt : std::optional(v.identifier);
    r.index = v.index.empty() ? std::nullopt : std::optional(v.index);
    r.index1 = v.index1.empty() ? std::nullopt : std::optional(v.index1);
    r.index2 = v.index2.empty() ? std::nullopt : std::optional(v.index2);
    r.index_name = v.index_name.empty() ? std::nullopt : std::optional(v.index_name);
    r.index_tenor = v.index_tenor.empty() ? std::nullopt : std::optional(v.index_tenor);
    r.index_term = v.index_term.empty() ? std::nullopt : std::optional(v.index_term);
    r.spread_offset = v.spread_offset.empty() ? std::nullopt : std::optional(v.spread_offset);
    r.option_type = v.option_type.empty() ? std::nullopt : std::optional(v.option_type);
    r.payer_receiver = v.payer_receiver.empty() ? std::nullopt : std::optional(v.payer_receiver);
    r.qualifier = v.qualifier.empty() ? std::nullopt : std::optional(v.qualifier);
    r.quote_name = v.quote_name.empty() ? std::nullopt : std::optional(v.quote_name);
    r.quote_tag = v.quote_tag.empty() ? std::nullopt : std::optional(v.quote_tag);
    r.rating_name = v.rating_name.empty() ? std::nullopt : std::optional(v.rating_name);
    r.relative = v.relative.empty() ? std::nullopt : std::optional(v.relative);
    r.running_spread = v.running_spread.empty() ? std::nullopt : std::optional(v.running_spread);
    r.seasonality_type =
        v.seasonality_type.empty() ? std::nullopt : std::optional(v.seasonality_type);
    r.security_id = v.security_id.empty() ? std::nullopt : std::optional(v.security_id);
    r.seniority = v.seniority.empty() ? std::nullopt : std::optional(v.seniority);
    r.side = v.side.empty() ? std::nullopt : std::optional(v.side);
    r.source = v.source.empty() ? std::nullopt : std::optional(v.source);
    r.tenor = v.tenor.empty() ? std::nullopt : std::optional(v.tenor);
    r.term = v.term.empty() ? std::nullopt : std::optional(v.term);
    r.time_unit = v.time_unit.empty() ? std::nullopt : std::optional(v.time_unit);
    r.underlying_name = v.underlying_name.empty() ? std::nullopt : std::optional(v.underlying_name);
    r.unit_ccy = v.unit_ccy.empty() ? std::nullopt : std::optional(v.unit_ccy);

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::market_series_identity>
market_series_identity_mapper::map(const std::vector<market_series_identity_entity>& v) {
    return map_vector<market_series_identity_entity, domain::market_series_identity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<market_series_identity_entity>
market_series_identity_mapper::map(const std::vector<domain::market_series_identity>& v) {
    return map_vector<domain::market_series_identity, market_series_identity_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}

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
#include "ores.refdata.core/repository/book_change_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/book_change.hpp"
#include "ores.refdata.api/domain/book_change_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/book_change_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::book_change book_change_mapper::map(const book_change_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::book_change r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.request_id = boost::lexical_cast<boost::uuids::uuid>(v.request_id);


    r.line_no = v.line_no;

    r.operation = v.operation;
    r.base_version = v.base_version;
    r.applied = v.applied;
    r.entity_id = boost::lexical_cast<boost::uuids::uuid>(v.entity_id);
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.name = v.name;
    r.description = v.description.value_or("");
    r.parent_portfolio_id = boost::lexical_cast<boost::uuids::uuid>(v.parent_portfolio_id);
    r.owner_unit_id = v.owner_unit_id.has_value() ?
                          std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.owner_unit_id)) :
                          std::nullopt;
    r.functional_currency = v.functional_currency;
    r.gl_account_ref = v.gl_account_ref.value_or("");
    r.cost_center = v.cost_center.value_or("");
    r.book_status = v.book_status;
    r.regulatory_book_type = v.regulatory_book_type;
    r.book_purpose_type = v.book_purpose_type;
    r.ledger_feed_type = v.ledger_feed_type;
    r.is_sweepable = v.is_sweepable;
    r.rates_centre_code = v.rates_centre_code;
    r.sandbox_id = v.sandbox_id.has_value() ?
                       std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.sandbox_id)) :
                       std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

book_change_entity book_change_mapper::map(const domain::book_change& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    book_change_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.request_id = boost::uuids::to_string(v.request_id);


    r.line_no = v.line_no;

    r.operation = v.operation;
    r.base_version = v.base_version;
    r.applied = v.applied;
    r.entity_id = boost::uuids::to_string(v.entity_id);
    r.party_id = boost::uuids::to_string(v.party_id);
    r.name = v.name;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.parent_portfolio_id = boost::uuids::to_string(v.parent_portfolio_id);
    r.owner_unit_id = v.owner_unit_id.has_value() ?
                          std::optional(boost::uuids::to_string(*v.owner_unit_id)) :
                          std::nullopt;
    r.functional_currency = v.functional_currency;
    r.gl_account_ref = v.gl_account_ref.empty() ? std::nullopt : std::optional(v.gl_account_ref);
    r.cost_center = v.cost_center.empty() ? std::nullopt : std::optional(v.cost_center);
    r.book_status = v.book_status;
    r.regulatory_book_type = v.regulatory_book_type;
    r.book_purpose_type = v.book_purpose_type;
    r.ledger_feed_type = v.ledger_feed_type;
    r.is_sweepable = v.is_sweepable;
    r.rates_centre_code = v.rates_centre_code;
    r.sandbox_id = v.sandbox_id.has_value() ?
                       std::optional(boost::uuids::to_string(*v.sandbox_id)) :
                       std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::book_change> book_change_mapper::map(const std::vector<book_change_entity>& v) {
    return map_vector<book_change_entity, domain::book_change>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<book_change_entity> book_change_mapper::map(const std::vector<domain::book_change>& v) {
    return map_vector<domain::book_change, book_change_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
